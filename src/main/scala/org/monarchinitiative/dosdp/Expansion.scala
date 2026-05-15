package org.monarchinitiative.dosdp

import cats.implicits._
import org.monarchinitiative.dosdp.AxiomType
import org.monarchinitiative.dosdp.cli.{Config, Generate}
import org.monarchinitiative.dosdp.cli.Generate.RowBindings
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{AxiomType => _, _}
import org.semanticweb.owlapi.util.OWLObjectDuplicator

import scala.jdk.CollectionConverters._

/**
 * Pure expansion of a single TSV row against a `CompiledPattern`, producing
 * the set of OWL axioms that row contributes.
 *
 * Substitution replaces the placeholder entities and placeholder literals
 * in each parsed template with values drawn from the row's bindings via
 * `OWLObjectDuplicator`. Per-row failures (a binding value that cannot be
 * resolved to an IRI, a row missing the `defined_class` column, a malformed
 * `restrict-axioms-column` value) are returned as `Left(ExpansionError)`.
 */
private[dosdp] object Expansion {

  private val factory = OWLManager.getOWLDataFactory
  private val patternTerm: OWLClass = factory.getOWLClass(DOSDP.variableToIRI(DOSDP.DefinedClassVariable))

  /** Inputs that vary by ontology and CLI configuration but stay fixed across a pattern's rows. */
  final case class ExpansionContext(
    readableIDIndex: Map[IRI, Map[IRI, String]],
    permutationIndex: Map[IRI, Map[IRI, Set[String]]],
    outputLogicalAxioms: Boolean,
    outputAnnotationAxioms: Boolean,
    restrictAxiomsColumn: Option[String],
    generateDefinedClass: Boolean,
    readableIdentifiers: List[OWLAnnotationProperty],
    localLabelProperty: IRI
  )

  def expandRow(
    pattern: CompiledPattern,
    bindings: RowBindings,
    row: Map[String, String],
    context: ExpansionContext
  ): Either[ExpansionError, Set[OWLAxiom]] =
    for {
      definedClass <- resolveDefinedClass(pattern, bindings, row, context.generateDefinedClass)
      iriBinding = DOSDP.DefinedClassVariable -> SingleValue(definedClass)
      classMap = pattern.source.classes.getOrElse(Map.empty)
      // Class-var bindings may carry quoted-label range expressions like `'thing'`
      // (notably when callers — Prototype — synthesize fillers from `dosdp.vars`).
      // Resolve those to IRIs via the pattern's classes map for OWL substitution,
      // but feed the unresolved values into annotation rendering so labels still
      // appear in the rendered text.
      logicalVarBindings = bindings.varBindings.view.mapValues(resolveBindingLabels(_, classMap, pattern.prefixes)).toMap
      logicalListBindings = bindings.listVarBindings.view.mapValues(resolveBindingLabels(_, classMap, pattern.prefixes)).toMap
      logicalBindings = logicalVarBindings ++ logicalListBindings ++ bindings.dataVarBindings ++ bindings.dataListBindings + iriBinding
      readableIDIndexPlusLocal = context.readableIDIndex + (context.localLabelProperty -> bindings.localLabels)
      annotationBindings = buildAnnotationBindings(bindings, pattern, iriBinding, readableIDIndexPlusLocal, context.readableIdentifiers, context.localLabelProperty)
      axiomKinds <- resolveAxiomKinds(context.restrictAxiomsColumn, row, context.outputLogicalAxioms, context.outputAnnotationAxioms)
      (localOutputLogicalAxioms, localOutputAnnotationAxioms) = axiomKinds
      logicalAxioms <- if (localOutputLogicalAxioms) buildLogicalAxioms(pattern, logicalBindings, annotationBindings) else Right(Set.empty[OWLAxiom])
      annotationAxioms = if (localOutputAnnotationAxioms) buildAnnotationAxioms(pattern, annotationBindings, logicalBindings, context.permutationIndex) else Set.empty[OWLAxiom]
    } yield logicalAxioms ++ annotationAxioms

  private def resolveDefinedClass(
    pattern: CompiledPattern,
    bindings: RowBindings,
    row: Map[String, String],
    generateDefinedClass: Boolean
  ): Either[ExpansionError, String] =
    if (generateDefinedClass)
      pattern.patternIRI
        .map(iri => Right(DOSDP.computeDefinedIRI(iri, bindings.bindingsForDefinedClassIRI).toString))
        .getOrElse(Left(ExpansionError.MissingPatternIRI))
    else
      row.get(DOSDP.DefinedClassVariable).map(_.trim).filter(_.nonEmpty)
        .toRight(ExpansionError.MissingDefinedClass)

  private def resolveAxiomKinds(
    restrictColumn: Option[String],
    row: Map[String, String],
    defaultLogical: Boolean,
    defaultAnnotation: Boolean
  ): Either[ExpansionError, (Boolean, Boolean)] =
    restrictColumn.flatMap(c => row.get(c)).flatMap(stripToOption) match {
      case None        => Right((defaultLogical, defaultAnnotation))
      case Some(value) =>
        Config.parseAxiomKind(value)
          .map(kind => Generate.axiomsOutputChoice(kind))
          .left.map(err => ExpansionError.MalformedAxiomKind(err.error))
    }

  private def stripToOption(text: String): Option[String] = {
    val trimmed = text.trim
    if (trimmed.isEmpty) None else Some(trimmed)
  }

  /**
   * Replace a binding value with its resolved IRI when the value is a quoted
   * class-label reference that maps through the pattern's `classes` map.
   * Values that already parse as CURIEs or IRIs are left alone, as are values
   * that don't resolve through either route — those propagate unchanged so
   * the existing UnresolvableBinding error fires downstream.
   */
  private def resolveBindingLabels(binding: Binding, classMap: Map[String, String], prefixes: PartialFunction[String, String]): Binding = {
    def resolve(value: String): String =
      if (Prefixes.idToIRI(value, prefixes).isDefined) value
      else Prefixes.nameOrVariableToIRI(value, classMap, prefixes).map(_.toString).getOrElse(value)
    binding match {
      case SingleValue(v) => SingleValue(resolve(v))
      case MultiValue(vs) => MultiValue(vs.map(resolve))
    }
  }

  private def buildAnnotationBindings(
    bindings: RowBindings,
    pattern: CompiledPattern,
    iriBinding: (String, SingleValue),
    readableIDIndexPlusLocal: Map[IRI, Map[IRI, String]],
    readableIdentifiers: List[OWLAnnotationProperty],
    localLabelProperty: IRI
  ): Map[String, Binding] = {
    val labelled: Map[String, Binding] =
      bindings.varBindings.view.mapValues(v => irisToLabels(readableIdentifiers, v, pattern.prefixes, readableIDIndexPlusLocal, localLabelProperty)).toMap ++
        bindings.listVarBindings.view.mapValues(v => irisToLabels(readableIdentifiers, v, pattern.prefixes, readableIDIndexPlusLocal, localLabelProperty)).toMap ++
        bindings.internalVarBindings.view.mapValues(v => resolveEmbeddedIRIs(readableIdentifiers, v, pattern.prefixes, readableIDIndexPlusLocal, localLabelProperty)).toMap ++
        bindings.dataVarBindings ++
        bindings.dataListBindings +
        iriBinding
    val expanded = pattern.substitutions.foldLeft(labelled)((bs, sub) => sub.expandBindings(bs))
    expanded ++ bindings.additionalBindings
  }

  private def irisToLabels(
    readableIdentifiers: List[OWLAnnotationProperty],
    binding: Binding,
    prefixes: PartialFunction[String, String],
    index: Map[IRI, Map[IRI, String]],
    localLabelProperty: IRI
  ): Binding = binding match {
    case SingleValue(value) =>
      SingleValue(Prefixes.idToIRI(value, prefixes).map(iri => readableIdentifierForIRI(readableIdentifiers, iri, index, localLabelProperty)).getOrElse(value))
    case MultiValue(values) =>
      MultiValue(values.map(value => Prefixes.idToIRI(value, prefixes).map(iri => readableIdentifierForIRI(readableIdentifiers, iri, index, localLabelProperty)).getOrElse(value)))
  }

  private def resolveEmbeddedIRIs(
    readableIdentifiers: List[OWLAnnotationProperty],
    binding: Binding,
    prefixes: PartialFunction[String, String],
    index: Map[IRI, Map[IRI, String]],
    localLabelProperty: IRI
  ): Binding = binding match {
    case sv @ SingleValue(value) =>
      val CURIEList = "([^ ,:]*):([^ ,]*)".r
      val CURIEListEmbed = CURIEList.unanchored
      if (CURIEListEmbed.matches(value)) {
        var resolvedValue = value
        CURIEList.findAllMatchIn(value).foreach { matching =>
          prefixes.lift(matching.group(1)).foreach { uri =>
            resolvedValue = resolvedValue.replaceFirst(
              matching.group(1) + ":" + matching.group(2),
              readableIdentifierForIRI(readableIdentifiers, IRI.create(uri + matching.group(2)), index, localLabelProperty))
          }
        }
        SingleValue(resolvedValue)
      } else sv
    case mv => mv
  }

  private def readableIdentifierForIRI(
    readableIdentifiers: List[OWLAnnotationProperty],
    iri: IRI,
    index: Map[IRI, Map[IRI, String]],
    localLabelProperty: IRI
  ): String = {
    val properties = readableIdentifiers.map(_.getIRI) ::: localLabelProperty :: Nil
    properties.collectFirst {
      case prop if index.get(prop).exists(_.isDefinedAt(iri)) => index(prop)(iri)
    }.getOrElse(iri.toString)
  }

  private def buildLogicalAxioms(
    pattern: CompiledPattern,
    logicalBindings: Map[String, Binding],
    annotationBindings: Map[String, Binding]
  ): Either[ExpansionError, Set[OWLAxiom]] = {
    val definedTerm = definedTermOf(logicalBindings, pattern.prefixes)
    for {
      equivalentTo <- pattern.equivalentTo.traverse(substituteClassExpression(_, logicalBindings, pattern.prefixes)).map(_.flatten)
      subClassOf   <- pattern.subClassOf.traverse(substituteClassExpression(_, logicalBindings, pattern.prefixes)).map(_.flatten)
      disjointWith <- pattern.disjointWith.traverse(substituteClassExpression(_, logicalBindings, pattern.prefixes)).map(_.flatten)
      gci          <- pattern.gci.traverse(substituteAxiom(_, logicalBindings, pattern.prefixes)).map(_.flatten)
      logicalAxs   <- pattern.logicalAxioms.traverse(substituteLogicalAxiom(_, logicalBindings, annotationBindings, definedTerm, pattern.prefixes))
    } yield {
      val classExprAxioms: Set[OWLAxiom] = Set(
        equivalentTo.map { e =>
          val anns = translateAxiomAnnotations(pattern.equivalentTo.get.annotations, annotationBindings, logicalBindings, pattern.prefixes)
          EquivalentClasses(anns.toSeq: _*)(definedTerm, e): OWLAxiom
        },
        subClassOf.map { e =>
          val anns = translateAxiomAnnotations(pattern.subClassOf.get.annotations, annotationBindings, logicalBindings, pattern.prefixes)
          SubClassOf(anns, definedTerm, e): OWLAxiom
        },
        disjointWith.map { e =>
          val anns = translateAxiomAnnotations(pattern.disjointWith.get.annotations, annotationBindings, logicalBindings, pattern.prefixes)
          DisjointClasses(anns.toSeq: _*)(definedTerm, e): OWLAxiom
        },
        gci.map { ax =>
          val anns = translateAxiomAnnotations(pattern.gci.get.annotations, annotationBindings, logicalBindings, pattern.prefixes)
          ax.getAnnotatedAxiom(anns.asJava): OWLAxiom
        }
      ).flatten
      classExprAxioms ++ logicalAxs.flatten.toSet
    }
  }

  private def definedTermOf(bindings: Map[String, Binding], prefixes: PartialFunction[String, String]): OWLClass =
    bindings.get(DOSDP.DefinedClassVariable).collect {
      case SingleValue(value) => Prefixes.idToIRI(value, prefixes).map(factory.getOWLClass)
    }.flatten.getOrElse(patternTerm)

  private def substituteLogicalAxiom(
    compiled: CompiledLogicalAxiom,
    logicalBindings: Map[String, Binding],
    annotationBindings: Map[String, Binding],
    definedTerm: OWLClass,
    prefixes: PartialFunction[String, String]
  ): Either[ExpansionError, Option[OWLAxiom]] =
    compiled match {
      case CompiledLogicalClassAxiom(_, expr, anns) =>
        substituteClassExpression(expr, logicalBindings, prefixes).map(_.flatMap { ce =>
          val translated = translateAxiomAnnotations(anns, annotationBindings, logicalBindings, prefixes)
          compiled.axiomType match {
            case AxiomType.EquivalentTo => Some(EquivalentClasses(translated.toSeq: _*)(definedTerm, ce))
            case AxiomType.SubClassOf   => Some(SubClassOf(translated, definedTerm, ce))
            case AxiomType.DisjointWith => Some(DisjointClasses(translated.toSeq: _*)(definedTerm, ce))
            case AxiomType.GCI          => None
          }
        })
      case CompiledLogicalGCIAxiom(_, ax, anns) =>
        substituteAxiom(ax, logicalBindings, prefixes).map(_.map { axiom =>
          val translated = translateAxiomAnnotations(anns, annotationBindings, logicalBindings, prefixes)
          axiom.getAnnotatedAxiom(translated.asJava)
        })
    }

  /**
   * Substitute bindings into a class-expression template. Returns `None` when
   * the template's required variables are not all bound — matching the legacy
   * behavior of `PrintfText.replaced` returning `None` and dropping the
   * axiom. For multi-clause templates, unsatisfied clauses fall out and the
   * survivors combine; if no clause is satisfied, the whole expression is
   * dropped.
   */
  private def substituteClassExpression(
    compiled: CompiledClassExpression,
    bindings: Map[String, Binding],
    prefixes: PartialFunction[String, String]
  ): Either[ExpansionError, Option[OWLClassExpression]] = compiled match {
    case CompiledSimpleClassExpression(_, piece, _) =>
      substituteSinglePiece(piece, bindings, prefixes, multiValueOverride = None)
    case CompiledMultiClassExpression(_, clauses, op, _) =>
      clauses.flatTraverse(clause => expandPrintfClause(clause, bindings, prefixes))
        .map(exprs => if (exprs.isEmpty) None else Some(CompiledClassExpression.combine(exprs, op)))
  }

  /**
   * Expand one top-level `CompiledPrintfClause` against the row bindings.
   * Returns the expression for the main piece (one entry per multi-value
   * expansion) followed by each surviving sub-expression's combined result.
   * If the main piece has missing bindings, the entire clause-group is
   * dropped — sub-clauses cannot anchor without their parent.
   */
  private def expandPrintfClause(
    clause: CompiledPrintfClause,
    bindings: Map[String, Binding],
    prefixes: PartialFunction[String, String]
  ): Either[ExpansionError, List[OWLClassExpression]] =
    expandClauseWithMultiValue(clause.main, bindings, prefixes).flatMap { mainExprs =>
      if (mainExprs.isEmpty) Right(Nil)
      else clause.subExpressions.traverse(expandSubExpression(_, bindings, prefixes))
        .map(subOpts => mainExprs ++ subOpts.flatten)
    }

  private def expandSubExpression(
    sub: CompiledSubExpression,
    bindings: Map[String, Binding],
    prefixes: PartialFunction[String, String]
  ): Either[ExpansionError, Option[OWLClassExpression]] =
    sub.clauses.flatTraverse(expandPrintfClause(_, bindings, prefixes))
      .map(exprs => if (exprs.isEmpty) None else Some(CompiledClassExpression.combine(exprs, sub.operator)))

  private def substituteAxiom(
    compiled: CompiledAxiom,
    bindings: Map[String, Binding],
    prefixes: PartialFunction[String, String]
  ): Either[ExpansionError, Option[OWLAxiom]] =
    substituteSinglePiece(compiled.piece, bindings, prefixes, multiValueOverride = None)

  /**
   * Expand one parsed clause into one expression per value when a multi-value binding
   * targets one of the clause's declared vars. Only the first multi-value binding found
   * is expanded; other multi-value bindings on the same clause are ignored — a clause
   * with multiple multi-value vars produces a cartesian product that cannot be encoded
   * in a single substitution step.
   */
  private def expandClauseWithMultiValue(
    piece: ParsedPiece[OWLClassExpression],
    bindings: Map[String, Binding],
    prefixes: PartialFunction[String, String]
  ): Either[ExpansionError, List[OWLClassExpression]] = {
    val multiValueOverride = piece.vars.collectFirst {
      case v if bindings.get(v).exists(_.isInstanceOf[MultiValue]) =>
        v -> bindings(v).asInstanceOf[MultiValue].value.toList
    }
    multiValueOverride match {
      case None =>
        substituteSinglePiece(piece, bindings, prefixes, None).map(_.toList)
      case Some((varName, values)) =>
        values.traverse(value => substituteSinglePiece(piece, bindings, prefixes, Some(varName -> value))).map(_.flatten)
    }
  }

  /**
   * Apply a parsed template's substitution slots against the row bindings.
   * `multiValueOverride`, when supplied, pins the named variable to a single
   * value drawn from a multi-value binding; the rest of the bindings resolve
   * normally. Returns `None` when any class or literal slot in `piece` lacks
   * a usable single-value binding — those rows produce no axiom from this
   * piece (matching the legacy `PrintfText.replaced` drop-on-missing-var
   * behavior).
   */
  private def substituteSinglePiece[T <: OWLObject](
    piece: ParsedPiece[T],
    bindings: Map[String, Binding],
    prefixes: PartialFunction[String, String],
    multiValueOverride: Option[(String, String)]
  ): Either[ExpansionError, Option[T]] = {
    val effectiveBindings: Map[String, Binding] = multiValueOverride match {
      case Some((name, value)) => bindings.updated(name, SingleValue(value))
      case None                => bindings
    }
    if (!allSlotsBound(piece, effectiveBindings)) Right(None)
    else for {
      entityMap <- buildEntitySubstitutions(piece, effectiveBindings, prefixes)
      literalMap = buildLiteralSubstitutions(piece, effectiveBindings)
      duplicator = new OWLObjectDuplicator(entityMap.asJava, factory, literalMap.asJava)
    } yield Some(duplicator.duplicateObject(piece.parsed).asInstanceOf[T])
  }

  private def allSlotsBound(piece: ParsedPiece[_], bindings: Map[String, Binding]): Boolean = {
    val required = piece.classVarSlots.keySet ++ piece.dataVarSlots.keySet
    required.forall(v => bindings.get(v).exists(_.isInstanceOf[SingleValue]))
  }

  private def buildEntitySubstitutions[T <: OWLObject](
    piece: ParsedPiece[T],
    bindings: Map[String, Binding],
    prefixes: PartialFunction[String, String]
  ): Either[ExpansionError, Map[OWLEntity, IRI]] = {
    val entries: Either[ExpansionError, List[(OWLEntity, IRI)]] =
      piece.classVarSlots.toList.flatTraverse { case (varName, entities) =>
        bindings.get(varName) match {
          case Some(SingleValue(value)) =>
            Prefixes.idToIRI(value, prefixes)
              .toRight(ExpansionError.UnresolvableBinding(varName, value))
              .map(iri => entities.toList.map(_ -> iri))
          case Some(_: MultiValue) =>
            // A multi-value binding for this var resolves per-clause via `multiValueOverride`
            // higher up; nothing to substitute at this layer.
            Right(Nil)
          case None =>
            // No binding for this variable: leave its placeholder IRI in the result.
            Right(Nil)
        }
      }
    entries.map(_.toMap)
  }

  private def buildLiteralSubstitutions[T <: OWLObject](
    piece: ParsedPiece[T],
    bindings: Map[String, Binding]
  ): Map[OWLLiteral, OWLLiteral] =
    piece.dataVarSlots.flatMap { case (varName, placeholder) =>
      bindings.get(varName).collect {
        case SingleValue(value) => placeholder -> factory.getOWLLiteral(value, placeholder.getDatatype)
      }
    }

  private def buildAnnotationAxioms(
    pattern: CompiledPattern,
    annotationBindings: Map[String, Binding],
    logicalBindings: Map[String, Binding],
    permutationIndex: Map[IRI, Map[IRI, Set[String]]]
  ): Set[OWLAxiom] = {
    val definedTerm = annotationBindings.get(DOSDP.DefinedClassVariable).collect {
      case SingleValue(value) => Prefixes.idToIRI(value, pattern.prefixes).map(factory.getOWLClass)
    }.flatten.getOrElse(patternTerm)
    val allNormalizedAnns = pattern.oboAnnotations ++ pattern.annotations
    (for {
      normalized <- allNormalizedAnns
      annotation <- AnnotationTranslation.translate(normalized, Some(annotationBindings), Some(logicalBindings), permutationIndex, pattern.prefixes)
    } yield AnnotationAssertion(annotation.getAnnotations.asScala.toSet, annotation.getProperty, definedTerm, annotation.getValue): OWLAxiom)
  }

  private def translateAxiomAnnotations(
    normalized: Set[NormalizedAnnotation],
    annotationBindings: Map[String, Binding],
    logicalBindings: Map[String, Binding],
    prefixes: PartialFunction[String, String]
  ): Set[OWLAnnotation] =
    normalized.flatMap(AnnotationTranslation.translate(_, Some(annotationBindings), Some(logicalBindings), Map.empty, prefixes))

}

private[dosdp] sealed trait ExpansionError {
  def message: String
}

private[dosdp] object ExpansionError {
  final case class UnresolvableBinding(varName: String, value: String) extends ExpansionError {
    val message = s"Binding for '$varName' did not resolve to an IRI: $value"
  }
  case object MissingPatternIRI extends ExpansionError {
    val message = "Pattern must have an IRI if generate-defined-class is requested."
  }
  case object MissingDefinedClass extends ExpansionError {
    val message = s"No input column provided for ${DOSDP.DefinedClassVariable}"
  }
  final case class MalformedAxiomKind(value: String) extends ExpansionError {
    val message = s"Malformed value in table restrict-axioms-column: $value"
  }
}