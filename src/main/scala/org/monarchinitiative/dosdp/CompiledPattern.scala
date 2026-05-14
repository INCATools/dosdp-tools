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
  /** Placeholder-form class expression (variable slots left as `urn:dosdp:` IRIs). */
  def parsed: OWLClassExpression
}

private[dosdp] final case class CompiledSimpleClassExpression(
  template: PrintfText,
  piece: ParsedPiece[OWLClassExpression],
  annotations: Set[NormalizedAnnotation]
) extends CompiledClassExpression {
  def parsed: OWLClassExpression = piece.parsed
}

private[dosdp] final case class CompiledMultiClassExpression(
  template: PrintfText,
  clauses: List[ParsedPiece[OWLClassExpression]],
  operator: LogicalOperator,
  annotations: Set[NormalizedAnnotation]
) extends CompiledClassExpression {
  lazy val parsed: OWLClassExpression = CompiledClassExpression.combine(clauses.map(_.parsed), operator)
}

private[dosdp] object CompiledClassExpression {

  private val factory = OWLManager.getOWLDataFactory

  /** Combine clauses with the given operator. Single clause passes through; empty yields `owl:Thing`. */
  def combine(clauses: List[OWLClassExpression], op: LogicalOperator): OWLClassExpression =
    clauses.distinct match {
      case Nil         => factory.getOWLThing
      case head :: Nil => head
      case multiple    => op match {
        case LogicalOperator.And => factory.getOWLObjectIntersectionOf(multiple.toSet.asJava)
        case LogicalOperator.Or  => factory.getOWLObjectUnionOf(multiple.toSet.asJava)
      }
    }

}

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
  // Compile-time substitution renders class slots as the placeholder entity-name string
  // `'$<name>'` (Manchester resolves these to `urn:dosdp:<name>` via DOSDPEntityChecker)
  // and renders data-var slots as a typed literal `"$<name>"^^<datatype>` so the
  // Manchester parser accepts them in any context that expects a literal — facet,
  // `value`, etc. — not only the facet-coercion case. Row-time substitution then
  // matches placeholder literals by their `$<name>` lexical form.
  //
  // Cardinality slots (`min N`, `max N`, `exactly N`) require a bare integer that
  // OWLObjectDuplicator cannot substitute via the literal map, so a `data_var` in a
  // cardinality position is not supported and will fail at pattern compile time.
  private val PlaceholderLiteralPattern = """^\$(.+)$""".r

  def compile(dosdp: DOSDP, prefixes: PartialFunction[String, String]): ZIO[Logging, DOSDPError, CompiledPattern] = {
    val checker = new DOSDPEntityChecker(dosdp, prefixes)
    val safeChecker = new SafeOWLEntityChecker(checker)
    val expressionParser = new ManchesterOWLSyntaxClassExpressionParser(factory, checker)
    val axiomParser = new ManchesterOWLSyntaxInlineAxiomParser(factory, checker)
    val dataVarNames = dataVarNamesOf(dosdp)
    val placeholderBindings = compileTimeBindings(dosdp)
    for {
      equivalentTo          <- compileClassExpressionTemplate(dosdp.equivalentTo, expressionParser, dataVarNames, safeChecker, placeholderBindings)
      subClassOf            <- compileClassExpressionTemplate(dosdp.subClassOf, expressionParser, dataVarNames, safeChecker, placeholderBindings)
      disjointWith          <- compileClassExpressionTemplate(dosdp.disjointWith, expressionParser, dataVarNames, safeChecker, placeholderBindings)
      gci                   <- compileAxiomTemplate(dosdp.GCI, axiomParser, dataVarNames, safeChecker, placeholderBindings)
      logicalAxioms         <- ZIO.foreach(dosdp.logical_axioms.toList.flatten)(compileLogicalAxiom(_, expressionParser, axiomParser, dataVarNames, safeChecker, placeholderBindings))
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
    checker: SafeOWLEntityChecker,
    placeholderBindings: Map[String, Binding]
  ): ZIO[Logging, DOSDPError, Option[CompiledClassExpression]] =
    ZIO.foreach(templateOpt) { template =>
      for {
        annotations <- ZIO.foreach(template.annotations.toList.flatten)(AnnotationCompiler.normalizeAnnotation(_, checker)).map(_.toSet)
        compiledExpr <- compileClassExpressionBody(template, parser, dataVarNames, annotations, placeholderBindings)
      } yield compiledExpr
    }

  private def compileClassExpressionBody(
    template: PrintfText,
    parser: ManchesterOWLSyntaxClassExpressionParser,
    dataVarNames: Set[String],
    annotations: Set[NormalizedAnnotation],
    placeholderBindings: Map[String, Binding]
  ): ZIO[Logging, DOSDPError, CompiledClassExpression] =
    (template.text, template.multi_clause) match {
      case (Some(_), _) =>
        parseClauseText(template.text, template.vars, template.multi_clause, dataVarNames, parser, template.shouldQuote, placeholderBindings)
          .map(piece => CompiledSimpleClassExpression(template, piece, annotations))
      case (None, Some(mc)) =>
        compileMultiClause(template, mc, dataVarNames, parser, template.shouldQuote, annotations, placeholderBindings)
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
    annotations: Set[NormalizedAnnotation],
    placeholderBindings: Map[String, Binding]
  ): ZIO[Logging, DOSDPError, CompiledMultiClassExpression] = {
    val sep = mc.sep.getOrElse("")
    val operator: ZIO[Logging, DOSDPError, LogicalOperator] = sep match {
      case " and " => ZIO.succeed(LogicalOperator.And)
      case " or "  => ZIO.succeed(LogicalOperator.Or)
      case other   => logErrorFail(s"Logical multi_clause separator must be ' and ' or ' or ', found: '$other'")
    }
    for {
      op <- operator
      pieces <- ZIO.foreach(mc.clauses.toList.flatten) { clause =>
        // Render each top-level clause together with its `sub_clauses` so the
        // resulting parsed expression covers all of them. The parent separator
        // also joins clause text and sub-clause text (matching the legacy
        // `PrintfText.replaceMultiClause` semantics: sub-clauses become
        // additional conjuncts/disjuncts of their parent clause).
        val wrapper = MultiClausePrintf(mc.sep, Some(List(clause)))
        parseAssembledClause(wrapper, allClauseVars(clause), dataVarNames, parser, quote, placeholderBindings)
      }
    } yield CompiledMultiClassExpression(template, pieces, op, annotations)
  }

  /** Vars declared on a clause plus all vars declared on its nested sub-clauses. */
  private def allClauseVars(clause: PrintfClause): List[String] = {
    val nested = clause.sub_clauses.toSeq.flatten.flatMap { mc =>
      mc.clauses.toSeq.flatten.flatMap(allClauseVars)
    }
    clause.vars.getOrElse(Nil) ++ nested.toList
  }

  private def parseAssembledClause(
    multi: MultiClausePrintf,
    declaredVars: List[String],
    dataVarNames: Set[String],
    parser: ManchesterOWLSyntaxClassExpressionParser,
    quote: Boolean,
    placeholderBindings: Map[String, Binding]
  ): ZIO[Logging, DOSDPError, ParsedPiece[OWLClassExpression]] = {
    val effective = withFallbackPlaceholders(declaredVars, placeholderBindings)
    val resolved = PrintfText.replaced(None, None, Some(multi), Some(effective), quote, dataVarNames)
    ZIO.fromOption(resolved).orElse(logErrorFail("Could not assemble Manchester template text for parsing"))
      .flatMap { rendered =>
        ZIO.effect(parser.parse(rendered))
          .flatMapError(e => DOSDPError.logError(s"Failed to parse class expression: $rendered", e))
          .map(ce => describePiece(ce, Some(declaredVars)))
      }
  }

  private def compileAxiomTemplate(
    templateOpt: Option[PrintfOWLConvenience],
    parser: ManchesterOWLSyntaxInlineAxiomParser,
    dataVarNames: Set[String],
    checker: SafeOWLEntityChecker,
    placeholderBindings: Map[String, Binding]
  ): ZIO[Logging, DOSDPError, Option[CompiledAxiom]] =
    ZIO.foreach(templateOpt) { template =>
      for {
        _ <- ZIO.when(template.multi_clause.isDefined)(
          logErrorFail("multi_clause is not supported on full-axiom (GCI) templates"))
        annotations <- ZIO.foreach(template.annotations.toList.flatten)(AnnotationCompiler.normalizeAnnotation(_, checker)).map(_.toSet)
        piece <- parseAxiomText(template.text, template.vars, dataVarNames, parser, template.shouldQuote, placeholderBindings)
      } yield CompiledAxiom(template, piece, annotations)
    }

  private def compileLogicalAxiom(
    template: PrintfOWL,
    expressionParser: ManchesterOWLSyntaxClassExpressionParser,
    axiomParser: ManchesterOWLSyntaxInlineAxiomParser,
    dataVarNames: Set[String],
    checker: SafeOWLEntityChecker,
    placeholderBindings: Map[String, Binding]
  ): ZIO[Logging, DOSDPError, CompiledLogicalAxiom] = {
    for {
      annotations <- ZIO.foreach(template.annotations.toList.flatten)(AnnotationCompiler.normalizeAnnotation(_, checker)).map(_.toSet)
      compiled <- template.axiom_type match {
        case AxiomType.GCI =>
          for {
            _ <- ZIO.when(template.multi_clause.isDefined)(
              logErrorFail("multi_clause is not supported on logical_axioms entries with axiom_type GCI"))
            piece <- parseAxiomText(template.text, template.vars, dataVarNames, axiomParser, template.shouldQuote, placeholderBindings)
          } yield CompiledLogicalGCIAxiom(template, CompiledAxiom(template, piece, Set.empty), annotations)
        case _ =>
          compileClassExpressionBody(template, expressionParser, dataVarNames, Set.empty, placeholderBindings)
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
    quote: Boolean,
    placeholderBindings: Map[String, Binding]
  ): ZIO[Logging, DOSDPError, ParsedPiece[OWLClassExpression]] = {
    val effective = withFallbackPlaceholders(referencedVars(vars, multi), placeholderBindings)
    val resolved = PrintfText.replaced(text, vars, multi, Some(effective), quote, dataVarNames)
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
    quote: Boolean,
    placeholderBindings: Map[String, Binding]
  ): ZIO[Logging, DOSDPError, ParsedPiece[OWLAxiom]] = {
    val effective = withFallbackPlaceholders(vars.getOrElse(Nil), placeholderBindings)
    val resolved = PrintfText.replaced(text, vars, multi_clause = None, Some(effective), quote, dataVarNames)
    ZIO.fromOption(resolved).orElse(logErrorFail("Could not assemble Manchester axiom text for parsing"))
      .flatMap { rendered =>
        ZIO.effect(parser.parse(rendered))
          .flatMapError(e => DOSDPError.logError(s"Failed to parse axiom: $rendered", e))
          .map(ax => describePiece(ax, vars))
      }
  }

  /**
   * Add `name → SingleValue("$name")` fallback bindings for any referenced
   * variable not declared in `dosdp.vars` / `list_vars` / `data_vars` /
   * `data_list_vars`. Patterns can reference derived placeholder names
   * (`__attribute`, etc.) in their templates without declaring them; the
   * legacy no-bindings rendering treated all such names as placeholder
   * entities. The fallback preserves that.
   */
  private def withFallbackPlaceholders(referenced: List[String], bindings: Map[String, Binding]): Map[String, Binding] =
    bindings ++ referenced.filterNot(bindings.contains).map(name => name -> SingleValue("$" + name))

  private def referencedVars(vars: Option[List[String]], multi: Option[MultiClausePrintf]): List[String] = {
    val top = vars.getOrElse(Nil)
    val nested = multi.toList.flatMap(multiClauseVars)
    top ++ nested
  }

  private def multiClauseVars(mc: MultiClausePrintf): List[String] =
    mc.clauses.toList.flatten.flatMap(allClauseVars)

  /**
   * Synthetic per-variable bindings used to render templates at compile time.
   * Class-typed vars get the bare placeholder name (`$name`), which
   * `PrintfText` wraps in single quotes so the Manchester parser sees an
   * entity reference and the `DOSDPEntityChecker` resolves it to
   * `urn:dosdp:<name>`. Data-typed vars (declared in `data_vars` /
   * `data_list_vars`) get a typed-literal token `"$<name>"^^<datatype>` so the
   * parser accepts them in any context that expects a literal, not only the
   * facet-coercion case.
   */
  private def compileTimeBindings(dosdp: DOSDP): Map[String, Binding] = {
    val classVars = (dosdp.vars.toSeq.flatMap(_.keys) ++ dosdp.list_vars.toSeq.flatMap(_.keys))
      .map(name => name -> SingleValue("$" + name)).toMap
    val dataBindings = (dosdp.data_vars.toSeq.flatten ++ dosdp.data_list_vars.toSeq.flatten)
      .map { case (name, datatype) => name -> SingleValue(s""""$$$name"^^$datatype""") }.toMap
    classVars ++ dataBindings
  }

  // Placeholder entities in `parsed` come from `urn:dosdp:<name>`. Placeholder
  // literals (data-var slots) have lexical form `$<name>`. Scan once at compile
  // time so row-time substitution can look up by variable name.
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
        case dr: OWLDatatypeRestriction =>
          dr.getFacetRestrictions.asScala.foreach(fr => builder += fr.getFacetValue)
        case dsv: OWLDataSomeValuesFrom =>
          collectFromDataRange(dsv.getFiller, builder)
        case dav: OWLDataAllValuesFrom =>
          collectFromDataRange(dav.getFiller, builder)
        case dhv: OWLDataHasValue =>
          builder += dhv.getFiller
        case _ => ()
      }
    }

  private def collectFromDataRange(range: OWLDataRange, builder: scala.collection.mutable.Set[OWLLiteral]): Unit = range match {
    case dtr: OWLDatatypeRestriction => dtr.getFacetRestrictions.asScala.foreach(fr => builder += fr.getFacetValue)
    case _                           => ()
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