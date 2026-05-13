package org.monarchinitiative.dosdp

import cats.implicits._
import org.monarchinitiative.dosdp.AxiomType
import org.monarchinitiative.dosdp.cli.DOSDPError
import org.monarchinitiative.dosdp.cli.DOSDPError.logError
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.manchestersyntax.parser.{ManchesterOWLSyntaxClassExpressionParser, ManchesterOWLSyntaxInlineAxiomParser}
import org.semanticweb.owlapi.model._
import zio._
import zio.logging._

import scala.jdk.CollectionConverters._
import scala.util.matching.Regex.Match

/**
 * Per-row helper around a `CompiledPattern`: feeds the Manchester parser and
 * the annotation translator the row's bindings. Every pattern-time value
 * (normalized annotations, resolved properties, substitutions, dataVarNames)
 * comes from `compiled`; nothing is recomputed per row beyond Manchester
 * parsing and value rendering.
 */
final case class ExpandedDOSDP(dosdp: DOSDP, prefixes: PartialFunction[String, String], compiled: CompiledPattern) {

  private lazy val checker = new DOSDPEntityChecker(dosdp, prefixes)
  private lazy val expressionParser = new ManchesterOWLSyntaxClassExpressionParser(OWLManager.getOWLDataFactory, checker)
  private lazy val axiomParser = new ManchesterOWLSyntaxInlineAxiomParser(OWLManager.getOWLDataFactory, checker)

  private type Bindings = Map[String, Binding]

  /**
   * Index mapping filler term IRIs to their annotation property values.
   * Structure: Map[FillerTermIRI, Map[AnnotationPropertyIRI, Set[AnnotationValues]]]
   */
  type PermutationIndex = Map[IRI, Map[IRI, Set[String]]]

  val substitutions: Seq[ExpandedRegexSub] = compiled.substitutions

  def allObjectProperties: Map[String, String] = dosdp.relations.getOrElse(Map.empty) ++ dosdp.objectProperties.getOrElse(Map.empty)

  // Data var fillers are numeric/string literals that get substituted into datatype
  // facets (e.g. `xsd:short[>= %s]`). They must NOT be wrapped in the Manchester-syntax
  // name-quoting that PrintfText applies to class/property name fillers — otherwise the
  // resulting literal has stray apostrophes in its lexical form and is ill-typed.
  private val dataVarNames: Set[String] = compiled.dataVarNames

  private def parseExpression(exp: Option[PrintfOWLConvenience], logicalBindings: Option[Map[String, Binding]], annotationBindings: Option[Map[String, Binding]]): ZIO[Logging, DOSDPError, Option[(OWLClassExpression, Set[OWLAnnotation])]] =
    ZIO.foreach(exp) { eq =>
      expressionFor(eq, logicalBindings).map(_.map(ce => ce -> annotationsFor(eq, annotationBindings, logicalBindings)))
    }.map(_.flatten)

  def equivalentToExpression(logicalBindings: Option[Map[String, Binding]], annotationBindings: Option[Map[String, Binding]]): ZIO[Logging, DOSDPError, Option[(OWLClassExpression, Set[OWLAnnotation])]] =
    parseExpression(dosdp.equivalentTo, logicalBindings, annotationBindings)

  def subClassOfExpression(logicalBindings: Option[Map[String, Binding]], annotationBindings: Option[Map[String, Binding]]): ZIO[Logging, DOSDPError, Option[(OWLClassExpression, Set[OWLAnnotation])]] =
    parseExpression(dosdp.subClassOf, logicalBindings, annotationBindings)

  def disjointWithExpression(logicalBindings: Option[Map[String, Binding]], annotationBindings: Option[Map[String, Binding]]): ZIO[Logging, DOSDPError, Option[(OWLClassExpression, Set[OWLAnnotation])]] =
    parseExpression(dosdp.disjointWith, logicalBindings, annotationBindings)

  def gciAxiom(logicalBindings: Option[Map[String, Binding]], annotationBindings: Option[Map[String, Binding]]): ZIO[Logging, DOSDPError, Option[(OWLAxiom, Set[OWLAnnotation])]] =
    ZIO.foreach(dosdp.GCI) { gci =>
      axiomFor(gci, logicalBindings).map(_.map(ax => ax -> annotationsFor(gci, annotationBindings, logicalBindings)))
    }.map(_.flatten)

  def logicalAxioms(logicalBindings: Option[Map[String, Binding]], annotationBindings: Option[Map[String, Binding]]): ZIO[Logging, DOSDPError, Set[OWLAxiom]] = {
    val defTerm = definedTerm(logicalBindings)
    ZIO.foreach(dosdp.logical_axioms.toList.flatten) { axiomDef =>
      val annotations = annotationsFor(axiomDef, annotationBindings, logicalBindings)
      axiomDef.axiom_type match {
        case AxiomType.EquivalentTo => expressionFor(axiomDef, logicalBindings).map(_.map(ce => EquivalentClasses(annotations.to(Seq): _*)(defTerm, ce)))
        case AxiomType.SubClassOf   => expressionFor(axiomDef, logicalBindings).map(_.map(ce => SubClassOf(annotations, defTerm, ce)))
        case AxiomType.DisjointWith => expressionFor(axiomDef, logicalBindings).map(_.map(ce => DisjointClasses(annotations.to(Seq): _*)(defTerm, ce)))
        case AxiomType.GCI          => axiomFor(axiomDef, logicalBindings).map(_.map(ax => ax.getAnnotatedAxiom(annotations.asJava)))
      }
    }.map(_.flatten.to(Set))
  }

  private val term = Class(DOSDP.variableToIRI(DOSDP.DefinedClassVariable))

  private def definedTerm(bindings: Option[Map[String, Binding]]): OWLClass = (for {
    actualBindings <- bindings
    defClass <- actualBindings.collect { case (key, SingleValue(value)) => (key, SingleValue(value)) }.get(DOSDP.DefinedClassVariable)
    iri <- Prefixes.idToIRI(defClass.value, prefixes)
  } yield Class(iri)).getOrElse(term)

  def filledLogicalAxioms(logicalBindings: Option[Map[String, Binding]], annotationBindings: Option[Map[String, Binding]]): ZIO[Logging, DOSDPError, Set[OWLAxiom]] = {
    val theDefinedTerm = definedTerm(logicalBindings)
    for {
      gciAxiomOpt <- gciAxiom(logicalBindings, annotationBindings)
      logicalAxs <- logicalAxioms(logicalBindings, annotationBindings)
      equivalentToOpt <- equivalentToExpression(logicalBindings, annotationBindings)
      subClassOfOpt <- subClassOfExpression(logicalBindings, annotationBindings)
      disjointWithOpt <- disjointWithExpression(logicalBindings, annotationBindings)
    } yield {
      equivalentToOpt.map { case (e, anns) => EquivalentClasses(anns.toSeq: _*)(theDefinedTerm, e) }.toSet ++
        subClassOfOpt.map { case (e, anns) => SubClassOf(anns, theDefinedTerm, e) }.toSet ++
        disjointWithOpt.map { case (e, anns) => DisjointClasses(anns.toSeq: _*)(theDefinedTerm, e) }.toSet ++
        gciAxiomOpt.map { case (axiom, anns) => axiom.getAnnotatedAxiom(anns.asJava) }.to(Set) ++ logicalAxs
    }
  }

  def varExpressions: ZIO[Logging, DOSDPError, Map[String, OWLClassExpression]] =
    ZIO.foreach(dosdp.vars.getOrElse(Map.empty)) { case (k, v) =>
      ZIO.effect(expressionParser.parse(v))
        .flatMapError(e => logError(s"Failed to parse class expression: $v", e))
        .map(k -> _)
    }

  def listVarExpressions: ZIO[Logging, DOSDPError, Map[String, OWLClassExpression]] =
    ZIO.foreach(dosdp.list_vars.getOrElse(Map.empty)) { case (k, v) =>
      ZIO.effect(expressionParser.parse(v))
        .flatMapError(e => logError(s"Failed to parse class expression: $v", e))
        .map(k -> _)
    }

  private def expressionFor(template: PrintfText, bindings: Option[Map[String, Binding]]): ZIO[Logging, DOSDPError, Option[OWLClassExpression]] =
    ZIO.foreach(PrintfText.replaced(template.text, template.vars, template.multi_clause, bindings, template.shouldQuote, dataVarNames)) { text =>
      ZIO.effect(expressionParser.parse(text)).flatMapError(e => logError(s"Failed to parse class expression: $text", e))
    }

  private def axiomFor(template: PrintfText, bindings: Option[Map[String, Binding]]): ZIO[Logging, DOSDPError, Option[OWLAxiom]] =
    ZIO.foreach(PrintfText.replaced(template.text, template.vars, template.multi_clause, bindings, template.shouldQuote, dataVarNames)) { text =>
      ZIO.effect(axiomParser.parse(text)).flatMapError(e => logError(s"Failed to parse axiom: $text", e))
    }

  private def annotationsFor(element: PrintfText, annotationBindings: Option[Map[String, Binding]], logicalBindings: Option[Map[String, Binding]]): Set[OWLAnnotation] =
    precomputedAnnotationsFor(element).flatMap(translateAnnotations(_, annotationBindings, logicalBindings))

  // Templates land in CompiledPattern by reference: each compiled value holds the same
  // PrintfText instance that the caller passes in here, so a reference-equality match
  // recovers the right annotation set.
  private def precomputedAnnotationsFor(template: PrintfText): Set[NormalizedAnnotation] = {
    val classExprMatches = List(compiled.equivalentTo, compiled.subClassOf, compiled.disjointWith).flatten
      .collectFirst { case ce if ce.template eq template => ce.annotations }
    val gciMatch = compiled.gci.collect { case ax if ax.template eq template => ax.annotations }
    val logicalAxMatch = compiled.logicalAxioms.collectFirst { case la if la.template eq template => la.annotations }
    classExprMatches.orElse(gciMatch).orElse(logicalAxMatch).getOrElse(Set.empty)
  }

  def filledAnnotationAxioms(annotationBindings: Option[Bindings], logicalBindings: Option[Bindings], permutationIndex: PermutationIndex = Map.empty): ZIO[Logging, DOSDPError, Set[OWLAnnotationAssertionAxiom]] = ZIO.succeed {
    val definedTerm = (for {
      actualBindings <- annotationBindings
      SingleValue(value) <- actualBindings.get(DOSDP.DefinedClassVariable)
      iri <- Prefixes.idToIRI(value, prefixes)
    } yield Class(iri)).getOrElse(term)
    for {
      normalizedAnnotationField <- compiled.oboAnnotations ++ compiled.annotations
      annotation <- translateAnnotations(normalizedAnnotationField, annotationBindings, logicalBindings, permutationIndex)
    } yield AnnotationAssertion(annotation.getAnnotations.asScala.toSet, annotation.getProperty, definedTerm, annotation.getValue)
  }

  private def translateAnnotations(annotationField: NormalizedAnnotation, annotationBindings: Option[Bindings], logicalBindings: Option[Bindings], permutationIndex: PermutationIndex = Map.empty): Set[OWLAnnotation] = annotationField match {
    case NormalizedPrintfAnnotation(prop, text, vars, multiClause, overrideColumnOpt, subAnnotations, permutations) =>
      val valueOpts = (for {
        column <- overrideColumnOpt
        bindings <- annotationBindings
        SingleValue(binding) <- bindings.get(column)
        trimmed = binding.trim
        if trimmed.nonEmpty
      } yield Seq(trimmed)).orElse(Some(printAnnotationWithPermutations(text, vars, multiClause, annotationBindings, logicalBindings, permutations, permutationIndex)))
      valueOpts.getOrElse(Seq.empty).toSet[String].map(value => Annotation(subAnnotations.flatMap(translateAnnotations(_, annotationBindings, logicalBindings, permutationIndex)), prop, value))
    case NormalizedListAnnotation(prop, value, subAnnotations)                                        =>
      // If no variable bindings are passed in, dummy value is filled in using variable name
      val multiValBindingsOpt = annotationBindings.map(multiValueBindings)
      val bindingsMap = multiValBindingsOpt.getOrElse(Map(value -> MultiValue(Set("'$" + value + "'"))))
      val listValueOpt = bindingsMap.get(value)
      listValueOpt.toSet[MultiValue].flatMap(listValue => listValue.value.map(v => Annotation(subAnnotations.flatMap(translateAnnotations(_, annotationBindings, logicalBindings, permutationIndex)), prop, v)))
    case NormalizedIRIValueAnnotation(prop, varr, subAnnotations)                                     =>
      val maybeIRIValue = logicalBindings.map { actualBindings =>
        for {
          SingleValue(value) <- actualBindings.get(varr)
          iri <- Prefixes.idToIRI(value, prefixes)
        } yield iri
      }.getOrElse(Some(DOSDP.variableToIRI(varr)))
      maybeIRIValue.toSet[IRI].map(iriValue => Annotation(
        subAnnotations.flatMap(translateAnnotations(_, annotationBindings, logicalBindings, permutationIndex)),
        prop,
        iriValue))
  }

  /**
   * PrintfText tend to build concatenated text from multiValue bindings. But printing an annotation requires printing
   * a distinct text per multiValue item. This method enables calling PrintfText.replaced for each multiValue clause.
   *
   * @param text               annotation text
   * @param vars               annotation variables
   * @param multiClause        annotation multiClauses
   * @param annotationBindings variable bindings
   * @return a sequence of printed and replaced annotation texts
   */
  def printAnnotation(text: Option[String], vars: Option[List[String]], multiClause: Option[MultiClausePrintf], annotationBindings: Option[Bindings]): Seq[String] = {
    val clauseVars = for {
      mc <- multiClause.toList
      clauses <- mc.clauses.toList
      clause <- clauses
      vars <- clause.vars
    } yield vars
    val variables = vars.getOrElse(List.empty) ++ clauseVars.flatten
    val annotationRelatedMultiValueBindings = annotationBindings.getOrElse(Map.empty[String, Binding])
      .view.filterKeys(variables.contains(_)).collectFirst { case (key, MultiValue(value)) => (key, value) }
    val singleValBindings = annotationBindings.getOrElse(Map.empty[String, Binding]).collect { case (key, SingleValue(value)) => (key, SingleValue(value)) }
    annotationRelatedMultiValueBindings match {
      case None                 =>
        PrintfText.replaced(text, vars, multiClause, annotationBindings.map(singleValueBindings), quote = false).toSeq
      case Some(multiValuePair) =>
        val multiValueText = for {
          value <- multiValuePair._2
          multiText <- PrintfText.replaced(None, None, multiClause, Some(singleValBindings + (multiValuePair._1 -> SingleValue(value))), quote = false)
        } yield multiText
        multiValueText.toSeq
    }
  }

  /**
   * Generates annotation texts with permutations. This extends the standard annotation generation
   * by also substituting synonym values from filler terms to generate additional annotations.
   *
   * The algorithm:
   * 1. For each variable in vars, collect all possible values:
   *    - Always include the label (from annotationBindings)
   *    - If the variable has permutation specs, also include values from the specified annotation properties
   * 2. Generate the cartesian product of all value combinations
   * 3. Format each combination using the text template
   */
  private def printAnnotationWithPermutations(
    text: Option[String],
    vars: Option[List[String]],
    multiClause: Option[MultiClausePrintf],
    annotationBindings: Option[Bindings],
    logicalBindings: Option[Bindings],
    permutations: List[NormalizedPermutation],
    permutationIndex: PermutationIndex
  ): Seq[String] = {
    val variableList = vars.getOrElse(List.empty)

    if (permutations.isEmpty || variableList.isEmpty)
      printAnnotation(text, vars, multiClause, annotationBindings)
    else {
      val permutationsByVar: Map[String, NormalizedPermutation] = permutations.map(p => p.`var` -> p).toMap
      val valuesPerVar = variableList.map(v =>
        valuesForVar(v, permutationsByVar, annotationBindings, logicalBindings, permutationIndex))
      cartesianProduct(valuesPerVar).flatMap { combination =>
        val bindingsForCombination = variableList.zip(combination).map { case (n, v) => n -> SingleValue(v) }.toMap
        PrintfText.replaced(text, vars, multiClause, Some(bindingsForCombination), quote = false)
      }.distinct
    }
  }

  /**
   * Candidate values for one variable in a permuted annotation: the label always
   * comes first (so its combination shows up before any permutation-only combination),
   * followed by values pulled from the filler term's annotation properties named by
   * any matching permutation spec.
   */
  private def valuesForVar(
    varName: String,
    permutationsByVar: Map[String, NormalizedPermutation],
    annotationBindings: Option[Bindings],
    logicalBindings: Option[Bindings],
    permutationIndex: PermutationIndex
  ): List[String] = {
    val labelValue: Option[String] = for {
      bindings <- annotationBindings
      SingleValue(value) <- bindings.get(varName)
    } yield value

    val fillerIRI: Option[IRI] = for {
      bindings <- logicalBindings
      SingleValue(value) <- bindings.get(varName)
      iri <- Prefixes.idToIRI(value, prefixes)
    } yield iri

    val permutationValues: Set[String] = (for {
      perm <- permutationsByVar.get(varName)
      iri <- fillerIRI
      termAnnos <- permutationIndex.get(iri)
    } yield perm.annotationProperties.flatMap(prop => termAnnos.getOrElse(prop.getIRI, Set.empty)).toSet)
      .getOrElse(Set.empty)

    labelValue.toList ++ permutationValues.toList
  }

  /**
   * Computes the cartesian product of a list of lists.
   * E.g., [[a, b], [1, 2]] => [[a, 1], [a, 2], [b, 1], [b, 2]]
   */
  private def cartesianProduct[T](lists: List[List[T]]): List[List[T]] = lists match {
    case Nil => List(Nil)
    case head :: tail =>
      val tailProduct = cartesianProduct(tail)
      for {
        h <- head
        t <- tailProduct
      } yield h :: t
  }

  private def singleValueBindings(bindings: Bindings): Map[String, SingleValue] = bindings.collect { case (key, value: SingleValue) => key -> value }

  private def multiValueBindings(bindings: Bindings): Map[String, MultiValue] = bindings.collect { case (key, value: MultiValue) => key -> value }

  val readableIdentifierProperties: List[OWLAnnotationProperty] = compiled.readableIdentifierProperties

  val permutationAnnotationProperties: Set[OWLAnnotationProperty] = compiled.permutationProperties

}

final case class ExpandedRegexSub(regexSub: RegexSub) {

  private val groupFinder = raw"\\(\d+)".r

  private val regex = regexSub.`match`.r

  /** Apply `regexSub` to a value; if the pattern does not match, return the value unchanged. */
  def substitute(value: String): String =
    regex.findFirstMatchIn(value).map { valueMatch =>
      groupFinder.replaceAllIn(regexSub.sub, (placeholder: Match) => valueMatch.group(placeholder.group(1).toInt))
    }.getOrElse(value)

  def expandBindings(bindings: Map[String, Binding]): Map[String, Binding] =
    bindings.get(regexSub.in).map {
      case SingleValue(value) => regexSub.out -> SingleValue(substitute(value))
      case MultiValue(values) => regexSub.out -> MultiValue(values.map(substitute))
    }.fold(bindings)(bindings + _)

}
