package org.monarchinitiative.dosdp

import org.phenoscape.scowl._
import org.semanticweb.owlapi.model._

/**
 * Pure rendering of a `NormalizedAnnotation` into one or more `OWLAnnotation`
 * values for a single row's bindings. Handles printf-style text templates,
 * literal list values, IRI-valued annotations, and permutation expansion
 * driven by an annotation-property index built from the loaded ontology.
 */
private[dosdp] object AnnotationTranslation {

  private type Bindings = Map[String, Binding]
  private type PermutationIndex = Map[IRI, Map[IRI, Set[String]]]

  def translate(
    annotationField: NormalizedAnnotation,
    annotationBindings: Option[Bindings],
    logicalBindings: Option[Bindings],
    permutationIndex: PermutationIndex,
    prefixes: PartialFunction[String, String]
  ): Set[OWLAnnotation] = annotationField match {
    case NormalizedPrintfAnnotation(prop, text, vars, multiClause, overrideColumnOpt, subAnnotations, permutations) =>
      val valueOpts = (for {
        column <- overrideColumnOpt
        bindings <- annotationBindings
        SingleValue(binding) <- bindings.get(column)
        trimmed = binding.trim
        if trimmed.nonEmpty
      } yield Seq(trimmed)).orElse(Some(
        printAnnotationWithPermutations(text, vars, multiClause, annotationBindings, logicalBindings, permutations, permutationIndex, prefixes)))
      valueOpts.getOrElse(Seq.empty).toSet[String].map(value =>
        Annotation(subAnnotations.flatMap(translate(_, annotationBindings, logicalBindings, permutationIndex, prefixes)), prop, value))
    case NormalizedListAnnotation(prop, value, subAnnotations) =>
      val multiValBindingsOpt = annotationBindings.map(multiValueBindings)
      val bindingsMap = multiValBindingsOpt.getOrElse(Map(value -> MultiValue(Set("'$" + value + "'"))))
      val listValueOpt = bindingsMap.get(value)
      listValueOpt.toSet[MultiValue].flatMap(listValue =>
        listValue.value.map(v =>
          Annotation(subAnnotations.flatMap(translate(_, annotationBindings, logicalBindings, permutationIndex, prefixes)), prop, v)))
    case NormalizedIRIValueAnnotation(prop, varr, subAnnotations) =>
      val maybeIRIValue = logicalBindings.map { actualBindings =>
        for {
          SingleValue(value) <- actualBindings.get(varr)
          iri <- Prefixes.idToIRI(value, prefixes)
        } yield iri
      }.getOrElse(Some(DOSDP.variableToIRI(varr)))
      maybeIRIValue.toSet[IRI].map(iriValue =>
        Annotation(subAnnotations.flatMap(translate(_, annotationBindings, logicalBindings, permutationIndex, prefixes)), prop, iriValue))
  }

  /**
   * Render an annotation's text against the row bindings, expanding any single
   * multi-value binding that appears in the clause's variable list into one
   * rendered string per value.
   */
  def printAnnotation(
    text: Option[String],
    vars: Option[List[String]],
    multiClause: Option[MultiClausePrintf],
    annotationBindings: Option[Bindings]
  ): Seq[String] = {
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
      case None =>
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
   * Generate annotation texts with permutations: for each annotation variable,
   * collect its label value plus permutation values pulled from the filler
   * term's annotation properties, then format the cartesian product of those
   * value lists through the printf template.
   */
  def printAnnotationWithPermutations(
    text: Option[String],
    vars: Option[List[String]],
    multiClause: Option[MultiClausePrintf],
    annotationBindings: Option[Bindings],
    logicalBindings: Option[Bindings],
    permutations: List[NormalizedPermutation],
    permutationIndex: PermutationIndex,
    prefixes: PartialFunction[String, String]
  ): Seq[String] = {
    val variableList = vars.getOrElse(List.empty)
    if (permutations.isEmpty || variableList.isEmpty)
      printAnnotation(text, vars, multiClause, annotationBindings)
    else {
      val permutationsByVar: Map[String, NormalizedPermutation] = permutations.map(p => p.`var` -> p).toMap
      val valuesPerVar = variableList.map(v =>
        valuesForVar(v, permutationsByVar, annotationBindings, logicalBindings, permutationIndex, prefixes))
      cartesianProduct(valuesPerVar).flatMap { combination =>
        val bindingsForCombination = variableList.zip(combination).map { case (n, v) => n -> SingleValue(v) }.toMap
        PrintfText.replaced(text, vars, multiClause, Some(bindingsForCombination), quote = false)
      }.distinct
    }
  }

  /**
   * Candidate values for one variable in a permuted annotation: the label
   * always comes first (so its combination is emitted before any
   * permutation-only combination), followed by values pulled from the filler
   * term's annotation properties named by any matching permutation spec.
   */
  private def valuesForVar(
    varName: String,
    permutationsByVar: Map[String, NormalizedPermutation],
    annotationBindings: Option[Bindings],
    logicalBindings: Option[Bindings],
    permutationIndex: PermutationIndex,
    prefixes: PartialFunction[String, String]
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

  /** Cartesian product: `[[a, b], [1, 2]] => [[a, 1], [a, 2], [b, 1], [b, 2]]`. */
  private def cartesianProduct[T](lists: List[List[T]]): List[List[T]] = lists match {
    case Nil          => List(Nil)
    case head :: tail =>
      val tailProduct = cartesianProduct(tail)
      for {
        h <- head
        t <- tailProduct
      } yield h :: t
  }

  private def singleValueBindings(bindings: Bindings): Map[String, SingleValue] =
    bindings.collect { case (key, value: SingleValue) => key -> value }

  private def multiValueBindings(bindings: Bindings): Map[String, MultiValue] =
    bindings.collect { case (key, value: MultiValue) => key -> value }

}