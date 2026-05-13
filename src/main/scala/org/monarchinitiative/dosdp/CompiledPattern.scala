package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.DOSDPError
import org.monarchinitiative.dosdp.cli.DOSDPError.logErrorFail
import org.monarchinitiative.dosdp.AxiomType
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.manchestersyntax.parser.{ManchesterOWLSyntaxClassExpressionParser, ManchesterOWLSyntaxInlineAxiomParser}
import org.semanticweb.owlapi.model.{AxiomType => _, _}
import zio._
import zio.logging.Logging

import scala.jdk.CollectionConverters._

/**
 * A pattern after all pattern-time resolution: annotation property names
 * looked up, sub-annotations recursively normalized, permutation specs
 * validated, and every Manchester template parsed once into an OWL value
 * carrying variable placeholders.
 *
 * Row expansion replaces those placeholders via `OWLObjectDuplicator`;
 * the Manchester parser is never invoked per row.
 */
private[dosdp] final case class CompiledPattern(
  source: DOSDP,
  prefixes: PartialFunction[String, String],
  patternIRI: Option[IRI],
  equivalentTo: Option[CompiledClassExpression],
  subClassOf: Option[CompiledClassExpression],
  disjointWith: Option[CompiledClassExpression],
  gci: Option[CompiledAxiom],
  logicalAxioms: List[CompiledLogicalAxiom],
  oboAnnotations: Set[NormalizedAnnotation],
  annotations: Set[NormalizedAnnotation],
  readableIdentifierProperties: List[OWLAnnotationProperty],
  permutationProperties: Set[OWLAnnotationProperty],
  substitutions: Seq[ExpandedRegexSub],
  dataVarNames: Set[String]
)

/**
 * One parsed Manchester piece with its substitution slots. `classVarSlots`
 * maps a pattern variable name to every placeholder entity carrying that
 * variable's `urn:dosdp:` IRI in `parsed`; multiple entries occur when the
 * same variable appears in both a class and a property position. `dataVarSlots`
 * is one literal per `data_var` referenced.
 */
private[dosdp] final case class ParsedPiece[T <: OWLObject](
  parsed: T,
  classVarSlots: Map[String, Set[OWLEntity]],
  dataVarSlots: Map[String, OWLLiteral],
  vars: List[String]
)

private[dosdp] sealed trait LogicalOperator
private[dosdp] object LogicalOperator {
  case object And extends LogicalOperator
  case object Or extends LogicalOperator
}

/**
 * A compiled class-expression template, either a single parsed piece or a
 * `multi_clause` assembly where each clause parses independently and the
 * pieces are combined at row time with intersection / union.
 */
private[dosdp] sealed trait CompiledClassExpression {
  def template: PrintfText
  def annotations: Set[NormalizedAnnotation]
}

private[dosdp] final case class CompiledSimpleClassExpression(
  template: PrintfText,
  piece: ParsedPiece[OWLClassExpression],
  annotations: Set[NormalizedAnnotation]
) extends CompiledClassExpression

private[dosdp] final case class CompiledMultiClassExpression(
  template: PrintfText,
  clauses: List[ParsedPiece[OWLClassExpression]],
  operator: LogicalOperator,
  annotations: Set[NormalizedAnnotation]
) extends CompiledClassExpression

/**
 * A compiled full-axiom template (used for GCI, where the Manchester text
 * parses as an axiom rather than a class expression). Only single-text
 * templates are supported: `multi_clause` for an axiom template would have
 * to join axiom strings with `and` / `or`, which is not valid Manchester.
 */
private[dosdp] final case class CompiledAxiom(
  template: PrintfText,
  piece: ParsedPiece[OWLAxiom],
  annotations: Set[NormalizedAnnotation]
)

/**
 * A compiled `logical_axioms` entry. The Manchester text parses as a class
 * expression for `EquivalentTo` / `SubClassOf` / `DisjointWith`, or as a
 * full axiom for `GCI`; the row-time axiom wrapper depends on `axiomType`.
 */
private[dosdp] sealed trait CompiledLogicalAxiom {
  def template: PrintfOWL
  def axiomType: AxiomType
  def annotations: Set[NormalizedAnnotation]
}

private[dosdp] final case class CompiledLogicalClassAxiom(
  template: PrintfOWL,
  expression: CompiledClassExpression,
  annotations: Set[NormalizedAnnotation]
) extends CompiledLogicalAxiom {
  val axiomType: AxiomType = template.axiom_type
}

private[dosdp] final case class CompiledLogicalGCIAxiom(
  template: PrintfOWL,
  axiom: CompiledAxiom,
  annotations: Set[NormalizedAnnotation]
) extends CompiledLogicalAxiom {
  val axiomType: AxiomType = AxiomType.GCI
}

private[dosdp] object PatternCompiler {

  import PrintfAnnotationOBO._

  private val factory = OWLManager.getOWLDataFactory
  private val PlaceholderLiteralPattern = """^'\$(.+)'$""".r

  def compile(dosdp: DOSDP, prefixes: PartialFunction[String, String]): ZIO[Logging, DOSDPError, CompiledPattern] = {
    val checker = new DOSDPEntityChecker(dosdp, prefixes)
    val safeChecker = new SafeOWLEntityChecker(checker)
    val expressionParser = new ManchesterOWLSyntaxClassExpressionParser(factory, checker)
    val axiomParser = new ManchesterOWLSyntaxInlineAxiomParser(factory, checker)
    val dataVarNames = dataVarNamesOf(dosdp)
    for {
      equivalentTo          <- compileClassExpressionTemplate(dosdp.equivalentTo, expressionParser, dataVarNames, safeChecker)
      subClassOf            <- compileClassExpressionTemplate(dosdp.subClassOf, expressionParser, dataVarNames, safeChecker)
      disjointWith          <- compileClassExpressionTemplate(dosdp.disjointWith, expressionParser, dataVarNames, safeChecker)
      gci                   <- compileAxiomTemplate(dosdp.GCI, axiomParser, dataVarNames, safeChecker)
      logicalAxioms         <- ZIO.foreach(dosdp.logical_axioms.toList.flatten)(compileLogicalAxiom(_, expressionParser, axiomParser, dataVarNames, safeChecker))
      oboAnnotations        <- compileOBOAnnotations(dosdp, safeChecker)
      annotations           <- ZIO.foreach(dosdp.annotations.toList.flatten)(AnnotationCompiler.normalizeAnnotation(_, safeChecker)).map(_.toSet)
      readableIDProperties  <- compileReadableIdentifierProperties(dosdp, safeChecker)
      permutationProperties <- AnnotationCompiler.permutationAnnotationProperties(dosdp, safeChecker)
    } yield CompiledPattern(
      source = dosdp,
      prefixes = prefixes,
      patternIRI = dosdp.pattern_iri.flatMap(Prefixes.idToIRI(_, prefixes)),
      equivalentTo = equivalentTo,
      subClassOf = subClassOf,
      disjointWith = disjointWith,
      gci = gci,
      logicalAxioms = logicalAxioms,
      oboAnnotations = oboAnnotations,
      annotations = annotations,
      readableIdentifierProperties = readableIDProperties,
      permutationProperties = permutationProperties,
      substitutions = dosdp.substitutions.toSeq.flatten.map(ExpandedRegexSub),
      dataVarNames = dataVarNames)
  }

  private def compileClassExpressionTemplate(
    templateOpt: Option[PrintfOWLConvenience],
    parser: ManchesterOWLSyntaxClassExpressionParser,
    dataVarNames: Set[String],
    checker: SafeOWLEntityChecker
  ): ZIO[Logging, DOSDPError, Option[CompiledClassExpression]] =
    ZIO.foreach(templateOpt) { template =>
      for {
        annotations <- ZIO.foreach(template.annotations.toList.flatten)(AnnotationCompiler.normalizeAnnotation(_, checker)).map(_.toSet)
        compiledExpr <- compileClassExpressionBody(template, parser, dataVarNames, annotations)
      } yield compiledExpr
    }

  private def compileClassExpressionBody(
    template: PrintfText,
    parser: ManchesterOWLSyntaxClassExpressionParser,
    dataVarNames: Set[String],
    annotations: Set[NormalizedAnnotation]
  ): ZIO[Logging, DOSDPError, CompiledClassExpression] =
    (template.text, template.multi_clause) match {
      case (Some(_), _) =>
        parseClauseText(template.text, template.vars, template.multi_clause, dataVarNames, parser, template.shouldQuote)
          .map(piece => CompiledSimpleClassExpression(template, piece, annotations))
      case (None, Some(mc)) =>
        compileMultiClause(template, mc, dataVarNames, parser, template.shouldQuote, annotations)
      case _ =>
        // No text and no multi_clause — treat as no compiled body.
        ZIO.succeed(CompiledSimpleClassExpression(template, emptyClassPiece, annotations))
    }

  private def compileMultiClause(
    template: PrintfText,
    mc: MultiClausePrintf,
    dataVarNames: Set[String],
    parser: ManchesterOWLSyntaxClassExpressionParser,
    quote: Boolean,
    annotations: Set[NormalizedAnnotation]
  ): ZIO[Logging, DOSDPError, CompiledMultiClassExpression] = {
    val sep = mc.sep.getOrElse("")
    val operator: ZIO[Logging, DOSDPError, LogicalOperator] = sep match {
      case " and " => ZIO.succeed(LogicalOperator.And)
      case " or "  => ZIO.succeed(LogicalOperator.Or)
      case other   => logErrorFail(s"Logical multi_clause separator must be ' and ' or ' or ', found: '$other'")
    }
    for {
      op <- operator
      _ <- ZIO.foreach_(mc.clauses.toList.flatten) { clause =>
        if (clause.sub_clauses.exists(_.nonEmpty))
          logErrorFail(s"sub_clauses are not supported in logical-template multi_clause entries")
        else ZIO.unit
      }
      pieces <- ZIO.foreach(mc.clauses.toList.flatten) { clause =>
        parseClauseText(Some(clause.text), clause.vars, None, dataVarNames, parser, quote)
      }
    } yield CompiledMultiClassExpression(template, pieces, op, annotations)
  }

  private def compileAxiomTemplate(
    templateOpt: Option[PrintfOWLConvenience],
    parser: ManchesterOWLSyntaxInlineAxiomParser,
    dataVarNames: Set[String],
    checker: SafeOWLEntityChecker
  ): ZIO[Logging, DOSDPError, Option[CompiledAxiom]] =
    ZIO.foreach(templateOpt) { template =>
      for {
        _ <- ZIO.when(template.multi_clause.isDefined)(
          logErrorFail("multi_clause is not supported on full-axiom (GCI) templates"))
        annotations <- ZIO.foreach(template.annotations.toList.flatten)(AnnotationCompiler.normalizeAnnotation(_, checker)).map(_.toSet)
        piece <- parseAxiomText(template.text, template.vars, dataVarNames, parser, template.shouldQuote)
      } yield CompiledAxiom(template, piece, annotations)
    }

  private def compileLogicalAxiom(
    template: PrintfOWL,
    expressionParser: ManchesterOWLSyntaxClassExpressionParser,
    axiomParser: ManchesterOWLSyntaxInlineAxiomParser,
    dataVarNames: Set[String],
    checker: SafeOWLEntityChecker
  ): ZIO[Logging, DOSDPError, CompiledLogicalAxiom] = {
    for {
      annotations <- ZIO.foreach(template.annotations.toList.flatten)(AnnotationCompiler.normalizeAnnotation(_, checker)).map(_.toSet)
      compiled <- template.axiom_type match {
        case AxiomType.GCI =>
          for {
            _ <- ZIO.when(template.multi_clause.isDefined)(
              logErrorFail("multi_clause is not supported on logical_axioms entries with axiom_type GCI"))
            piece <- parseAxiomText(template.text, template.vars, dataVarNames, axiomParser, template.shouldQuote)
          } yield CompiledLogicalGCIAxiom(template, CompiledAxiom(template, piece, Set.empty), annotations)
        case _ =>
          compileClassExpressionBody(template, expressionParser, dataVarNames, Set.empty)
            .map(expr => CompiledLogicalClassAxiom(template, expr, annotations))
      }
    } yield compiled
  }

  private def parseClauseText(
    text: Option[String],
    vars: Option[List[String]],
    multi: Option[MultiClausePrintf],
    dataVarNames: Set[String],
    parser: ManchesterOWLSyntaxClassExpressionParser,
    quote: Boolean
  ): ZIO[Logging, DOSDPError, ParsedPiece[OWLClassExpression]] = {
    val resolved = PrintfText.replaced(text, vars, multi, bindings = None, quote, dataVarNames)
    ZIO.fromOption(resolved).orElse(logErrorFail(s"Could not assemble Manchester template text for parsing"))
      .flatMap { rendered =>
        ZIO.effect(parser.parse(rendered))
          .flatMapError(e => DOSDPError.logError(s"Failed to parse class expression: $rendered", e))
          .map(ce => describePiece(ce, vars))
      }
  }

  private def parseAxiomText(
    text: Option[String],
    vars: Option[List[String]],
    dataVarNames: Set[String],
    parser: ManchesterOWLSyntaxInlineAxiomParser,
    quote: Boolean
  ): ZIO[Logging, DOSDPError, ParsedPiece[OWLAxiom]] = {
    val resolved = PrintfText.replaced(text, vars, multi_clause = None, bindings = None, quote, dataVarNames)
    ZIO.fromOption(resolved).orElse(logErrorFail("Could not assemble Manchester axiom text for parsing"))
      .flatMap { rendered =>
        ZIO.effect(parser.parse(rendered))
          .flatMapError(e => DOSDPError.logError(s"Failed to parse axiom: $rendered", e))
          .map(ax => describePiece(ax, vars))
      }
  }

  // The placeholder entities in `parsed` always come from `urn:dosdp:<name>`. The
  // placeholder literals always have lexical form `'$<name>'`. Scan once at compile
  // so row-time substitution can look up by variable name.
  private def describePiece[T <: OWLObject](parsed: T, declaredVars: Option[List[String]]): ParsedPiece[T] = {
    val placeholderEntities = parsed.getSignature.asScala.collect {
      case e: OWLEntity if e.getIRI.toString.startsWith(DOSDP.variablePrefix) =>
        val name = e.getIRI.toString.drop(DOSDP.variablePrefix.length)
        name -> e
    }.toSet
    val classVarSlots = placeholderEntities.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
    val placeholderLiterals = collectLiterals(parsed).collect {
      case lit if PlaceholderLiteralPattern.findFirstMatchIn(lit.getLiteral).isDefined =>
        PlaceholderLiteralPattern.findFirstMatchIn(lit.getLiteral).get.group(1) -> lit
    }
    val dataVarSlots = placeholderLiterals.toMap
    ParsedPiece(parsed, classVarSlots, dataVarSlots, declaredVars.getOrElse(Nil))
  }

  // OWLObject.getSignature only returns entities; for literals we walk nested expressions.
  private def collectLiterals(obj: OWLObject): Set[OWLLiteral] = {
    // OWLObject doesn't expose its component literals directly. Walk nested class
    // expressions and their datatype facets, plus annotation values.
    val builder = scala.collection.mutable.Set.empty[OWLLiteral]
    obj match {
      case ce: OWLClassExpression => collectFromClassExpression(ce, builder)
      case ax: OWLAxiom           =>
        ax.getNestedClassExpressions.asScala.foreach(collectFromClassExpression(_, builder))
        ax.getAnnotations.asScala.foreach { ann =>
          ann.getValue match {
            case lit: OWLLiteral => builder += lit
            case _               => ()
          }
        }
      case _                      => ()
    }
    builder.toSet
  }

  private def collectFromClassExpression(ce: OWLClassExpression, builder: scala.collection.mutable.Set[OWLLiteral]): Unit =
    ce.getNestedClassExpressions.asScala.foreach { nested =>
      nested match {
        case dr: OWLDataRange =>
          dr.asInstanceOf[OWLObject] match {
            case dtr: OWLDatatypeRestriction =>
              dtr.getFacetRestrictions.asScala.foreach(fr => builder += fr.getFacetValue)
            case _ => ()
          }
        case dsv: OWLDataSomeValuesFrom =>
          dsv.getFiller match {
            case dtr: OWLDatatypeRestriction =>
              dtr.getFacetRestrictions.asScala.foreach(fr => builder += fr.getFacetValue)
            case _ => ()
          }
        case dav: OWLDataAllValuesFrom =>
          dav.getFiller match {
            case dtr: OWLDatatypeRestriction =>
              dtr.getFacetRestrictions.asScala.foreach(fr => builder += fr.getFacetValue)
            case _ => ()
          }
        case _ => ()
      }
    }

  private val emptyClassPiece: ParsedPiece[OWLClassExpression] =
    ParsedPiece(factory.getOWLThing, Map.empty, Map.empty, Nil)

  private def compileOBOAnnotations(dosdp: DOSDP, checker: SafeOWLEntityChecker): ZIO[Logging, DOSDPError, Set[NormalizedAnnotation]] = {
    val fields: List[(Iterable[OBOAnnotations], OWLAnnotationProperty, Option[String])] = List(
      (dosdp.name,                                       Name,           Some(overrides(Name))),
      (dosdp.comment,                                    Comment,        Some(overrides(Comment))),
      (dosdp.`def`,                                      Def,            Some(overrides(Def))),
      (dosdp.namespace,                                  Namespace,      Some(overrides(Namespace))),
      (dosdp.exact_synonym,                              ExactSynonym,   None),
      (dosdp.narrow_synonym,                             NarrowSynonym,  None),
      (dosdp.related_synonym,                            RelatedSynonym, None),
      (dosdp.broad_synonym,                              BroadSynonym,   None),
      (dosdp.xref,                                       Xref,           None),
      (dosdp.generated_synonyms.toList.flatten,          ExactSynonym,   Some(overrides(ExactSynonym))),
      (dosdp.generated_narrow_synonyms.toList.flatten,   NarrowSynonym,  Some(overrides(NarrowSynonym))),
      (dosdp.generated_broad_synonyms.toList.flatten,    BroadSynonym,   Some(overrides(BroadSynonym))),
      (dosdp.generated_related_synonyms.toList.flatten,  RelatedSynonym, Some(overrides(RelatedSynonym))))
    ZIO.foreach(fields) { case (anns, property, overrideColumnOpt) =>
      ZIO.foreach(anns)(ann => AnnotationCompiler.normalizeOBOAnnotation(ann, property, overrideColumnOpt, checker))
    }.map(_.flatten.toSet)
  }

  private def compileReadableIdentifierProperties(dosdp: DOSDP, checker: SafeOWLEntityChecker): ZIO[Logging, DOSDPError, List[OWLAnnotationProperty]] =
    ZIO.foreach(dosdp.readable_identifiers) { identifiers =>
      ZIO.foreach(identifiers)(name =>
        checker.getOWLAnnotationProperty(name)
          .orElse(logErrorFail(s"No annotation property mapping for '$name'")))
    }.map(_.getOrElse(org.phenoscape.scowl.RDFSLabel :: Nil))

  private def dataVarNamesOf(dosdp: DOSDP): Set[String] =
    dosdp.data_vars.toSet.flatMap((m: Map[String, String]) => m.keySet) ++
      dosdp.data_list_vars.toSet.flatMap((m: Map[String, String]) => m.keySet)

}