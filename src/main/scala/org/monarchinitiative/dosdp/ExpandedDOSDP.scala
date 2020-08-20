package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.AxiomType
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.manchestersyntax.parser.{ManchesterOWLSyntaxClassExpressionParser, ManchesterOWLSyntaxInlineAxiomParser}
import org.semanticweb.owlapi.model._

import scala.jdk.CollectionConverters._
import scala.util.matching.Regex.Match

/**
 * Wraps a DOSDP data structure with functionality dependent on expanding IDs into IRIs
 */
final case class ExpandedDOSDP(dosdp: DOSDP, prefixes: PartialFunction[String, String]) {

  lazy val checker = new DOSDPEntityChecker(dosdp, prefixes)
  lazy val safeChecker = new SafeOWLEntityChecker(checker)
  private lazy val expressionParser = new ManchesterOWLSyntaxClassExpressionParser(OWLManager.getOWLDataFactory, checker)
  private lazy val axiomParser = new ManchesterOWLSyntaxInlineAxiomParser(OWLManager.getOWLDataFactory, checker)

  private type Bindings = Map[String, Binding]

  val substitutions: Seq[ExpandedRegexSub] = dosdp.substitutions.toSeq.flatten.map(ExpandedRegexSub)

  def allObjectProperties: Map[String, String] = dosdp.relations.getOrElse(Map.empty) ++ dosdp.objectProperties.getOrElse(Map.empty)

  def equivalentToExpression(logicalBindings: Option[Map[String, SingleValue]], annotationBindings: Option[Map[String, Binding]]): Option[(OWLClassExpression, Set[OWLAnnotation])] = for {
    eq <- dosdp.equivalentTo
    ce <- expressionFor(eq, logicalBindings)
  } yield ce -> annotationsFor(eq, annotationBindings, logicalBindings)

  def subClassOfExpression(logicalBindings: Option[Map[String, SingleValue]], annotationBindings: Option[Map[String, Binding]]): Option[(OWLClassExpression, Set[OWLAnnotation])] = for {
    sco <- dosdp.subClassOf
    ce <- expressionFor(sco, logicalBindings)
  } yield ce -> annotationsFor(sco, annotationBindings, logicalBindings)

  def disjointWithExpression(logicalBindings: Option[Map[String, SingleValue]], annotationBindings: Option[Map[String, Binding]]): Option[(OWLClassExpression, Set[OWLAnnotation])] = for {
    dw <- dosdp.disjointWith
    ce <- expressionFor(dw, logicalBindings)
  } yield ce -> annotationsFor(dw, annotationBindings, logicalBindings)

  def gciAxiom(logicalBindings: Option[Map[String, SingleValue]], annotationBindings: Option[Map[String, Binding]]): Option[(OWLAxiom, Set[OWLAnnotation])] = for {
    gci <- dosdp.GCI
    ax <- axiomFor(gci, logicalBindings)
  } yield ax -> annotationsFor(gci, annotationBindings, logicalBindings)

  def logicalAxioms(logicalBindings: Option[Map[String, SingleValue]], annotationBindings: Option[Map[String, Binding]]): Set[OWLAxiom] = (for {
    axiomDefs <- dosdp.logical_axioms.toList
    axiomDef <- axiomDefs
    defTerm = definedTerm(logicalBindings)
  } yield axiomDef.axiom_type match {
    case AxiomType.EquivalentTo => expressionFor(axiomDef, logicalBindings).map(ce => EquivalentClasses(annotationsFor(axiomDef, annotationBindings, logicalBindings).toSeq: _*)(defTerm, ce))
    case AxiomType.SubClassOf   => expressionFor(axiomDef, logicalBindings).map(ce => SubClassOf(annotationsFor(axiomDef, annotationBindings, logicalBindings), defTerm, ce))
    case AxiomType.DisjointWith => expressionFor(axiomDef, logicalBindings).map(ce => DisjointClasses(annotationsFor(axiomDef, annotationBindings, logicalBindings).toSeq: _*)(defTerm, ce))
    case AxiomType.GCI          => axiomFor(axiomDef, logicalBindings).map(ax => ax.getAnnotatedAxiom(annotationsFor(axiomDef, annotationBindings, logicalBindings).asJava))
  }).toSet.flatten

  private val term = Class(DOSDP.variableToIRI(DOSDP.DefinedClassVariable))

  private def definedTerm(bindings: Option[Map[String, SingleValue]]): OWLClass = (for {
    actualBindings <- bindings
    defClass <- actualBindings.get(DOSDP.DefinedClassVariable)
    iri <- Prefixes.idToIRI(defClass.value, prefixes)
  } yield Class(iri)).getOrElse(term)

  def filledLogicalAxioms(logicalBindings: Option[Map[String, SingleValue]], annotationBindings: Option[Map[String, Binding]]): Set[OWLAxiom] = {
    val theDefinedTerm = definedTerm(logicalBindings)
    equivalentToExpression(logicalBindings, annotationBindings).map { case (e, anns) => EquivalentClasses(anns.toSeq: _*)(theDefinedTerm, e) }.toSet ++
      subClassOfExpression(logicalBindings, annotationBindings).map { case (e, anns) => SubClassOf(anns, theDefinedTerm, e) }.toSet ++
      disjointWithExpression(logicalBindings, annotationBindings).map { case (e, anns) => DisjointClasses(anns.toSeq: _*)(theDefinedTerm, e) }.toSet ++
      gciAxiom(logicalBindings, annotationBindings).map { case (axiom, anns) => axiom.getAnnotatedAxiom(anns.asJava) }.toSet ++ logicalAxioms(logicalBindings, annotationBindings)
  }

  def varExpressions: Map[String, OWLClassExpression] = {
    val vars = dosdp.vars.getOrElse(Map.empty)
    vars.view.mapValues(expressionParser.parse).toMap
  }

  private def expressionFor(template: PrintfText, bindings: Option[Map[String, SingleValue]]): Option[OWLClassExpression] =
    template.replaced(bindings).map(expressionParser.parse)

  private def axiomFor(template: PrintfText, bindings: Option[Map[String, SingleValue]]): Option[OWLAxiom] =
    template.replaced(bindings).map(axiomParser.parse)

  private def annotationsFor(element: PrintfText, annotationBindings: Option[Map[String, Binding]], logicalBindings: Option[Map[String, Binding]]): Set[OWLAnnotation] =
    (for {
      annotations <- element.annotations.toSeq
      annotation <- annotations
      normalizedAnnotation = normalizeAnnotation(annotation)
      owlAnnotation <- translateAnnotations(normalizedAnnotation, annotationBindings, logicalBindings)
    } yield owlAnnotation).toSet

  def filledAnnotationAxioms(annotationBindings: Option[Bindings], logicalBindings: Option[Bindings]): Set[OWLAnnotationAssertionAxiom] = {
    val definedTerm = (for {
      actualBindings <- annotationBindings
      SingleValue(value) <- actualBindings.get(DOSDP.DefinedClassVariable)
      iri <- Prefixes.idToIRI(value, prefixes)
    } yield Class(iri)).getOrElse(term)
    for {
      normalizedAnnotationField <- normalizedOBOAnnotations ++ dosdp.annotations.toList.flatten.map(normalizeAnnotation)
      annotation <- translateAnnotations(normalizedAnnotationField, annotationBindings, logicalBindings)
    } yield AnnotationAssertion(annotation.getAnnotations.asScala.toSet, annotation.getProperty, definedTerm, annotation.getValue)
  }

  private def normalizedOBOAnnotations: Set[NormalizedAnnotation] = {
    import PrintfAnnotationOBO._
    Map(
      dosdp.name.toSet -> (Name, Some(overrides(Name))),
      dosdp.comment.toSet -> (Comment, Some(overrides(Comment))),
      dosdp.`def`.toSet -> (Def, Some(overrides(Def))),
      dosdp.namespace.toSet -> (Namespace, Some(overrides(Namespace))),
      dosdp.exact_synonym.toSet -> (ExactSynonym, None),
      dosdp.narrow_synonym.toSet -> (NarrowSynonym, None),
      dosdp.related_synonym.toSet -> (RelatedSynonym, None),
      dosdp.broad_synonym.toSet -> (BroadSynonym, None),
      dosdp.xref.toSet -> (Xref, None),
      dosdp.generated_synonyms.toSet.flatten -> (ExactSynonym, Some(overrides(ExactSynonym))),
      dosdp.generated_narrow_synonyms.toSet.flatten -> (NarrowSynonym, Some(overrides(NarrowSynonym))),
      dosdp.generated_broad_synonyms.toSet.flatten -> (BroadSynonym, Some(overrides(BroadSynonym))),
      dosdp.generated_related_synonyms.toSet.flatten -> (RelatedSynonym, Some(overrides(RelatedSynonym)))).flatMap {
      case (value, (property, overrideColumnOpt)) => value.map(ann => normalizeOBOAnnotation(ann, property, overrideColumnOpt))
    }.toSet
  }

  private def translateAnnotations(annotationField: NormalizedAnnotation, annotationBindings: Option[Bindings], logicalBindings: Option[Bindings]): Set[OWLAnnotation] = annotationField match {
    case NormalizedPrintfAnnotation(prop, text, vars, overrideColumnOpt, subAnnotations) =>
      val valueOpt = (for {
        column <- overrideColumnOpt
        bindings <- annotationBindings
        SingleValue(binding) <- bindings.get(column)
        trimmed = binding.trim
        if trimmed.nonEmpty
      } yield trimmed).orElse(PrintfText.replaced(text, vars, annotationBindings.map(singleValueBindings), false))
      valueOpt.toSet[String].map(value => Annotation(subAnnotations.flatMap(translateAnnotations(_, annotationBindings, logicalBindings)), prop, value))
    case NormalizedListAnnotation(prop, value, subAnnotations)                           =>
      // If no variable bindings are passed in, dummy value is filled in using variable name
      val multiValBindingsOpt = annotationBindings.map(multiValueBindings)
      val bindingsMap = multiValBindingsOpt.getOrElse(Map(value -> MultiValue(Set("'$" + value + "'"))))
      val listValueOpt = bindingsMap.get(value)
      listValueOpt.toSet[MultiValue].flatMap(listValue => listValue.value.map(v => Annotation(subAnnotations.flatMap(translateAnnotations(_, annotationBindings, logicalBindings)), prop, v)))
    case NormalizedIRIValueAnnotation(prop, varr, subAnnotations)                        =>
      val maybeIRIValue = logicalBindings.map { actualBindings =>
        for {
          SingleValue(value) <- actualBindings.get(varr)
          iri <- Prefixes.idToIRI(value, prefixes)
        } yield iri
      }.getOrElse(Some(DOSDP.variableToIRI(varr)))
      maybeIRIValue.toSet[IRI].map(iriValue => Annotation(
        subAnnotations.flatMap(translateAnnotations(_, annotationBindings, logicalBindings)),
        prop,
        iriValue))
  }

  private def normalizeAnnotation(annotation: Annotations): NormalizedAnnotation = annotation match {
    case PrintfAnnotation(anns, ap, text, vars, overrideColumn) => NormalizedPrintfAnnotation(
      safeChecker.getOWLAnnotationProperty(ap).getOrElse(throw new RuntimeException(s"No annotation property binding: $ap")),
      text,
      vars,
      overrideColumn,
      anns.toSet.flatten.map(normalizeAnnotation))
    case ListAnnotation(anns, ap, value)                        => NormalizedListAnnotation(
      safeChecker.getOWLAnnotationProperty(ap).getOrElse(throw new RuntimeException(s"No annotation property binding: $ap")),
      value,
      anns.toSet.flatten.map(normalizeAnnotation))
    case IRIValueAnnotation(anns, ap, varr)                     => NormalizedIRIValueAnnotation(
      safeChecker.getOWLAnnotationProperty(ap).getOrElse(throw new RuntimeException(s"No annotation property binding: $ap")),
      varr,
      anns.toSet.flatten.map(normalizeAnnotation))
  }

  private def normalizeOBOAnnotation(annotation: OBOAnnotations, property: OWLAnnotationProperty, overrideColumn: Option[String]): NormalizedAnnotation = annotation match {
    case PrintfAnnotationOBO(anns, xrefs, text, vars) => NormalizedPrintfAnnotation(
      property,
      text,
      vars,
      overrideColumn,
      anns.toSet.flatten.map(normalizeAnnotation) ++ xrefs.map(NormalizedListAnnotation(PrintfAnnotationOBO.Xref, _, Set.empty)))
    case ListAnnotationOBO(value, xrefs)              => NormalizedListAnnotation(
      property,
      value,
      xrefs.map(NormalizedListAnnotation(PrintfAnnotationOBO.Xref, _, Set.empty)).toSet)
  }

  private def singleValueBindings(bindings: Bindings): Map[String, SingleValue] = bindings.collect { case (key, value: SingleValue) => key -> value }

  private def multiValueBindings(bindings: Bindings): Map[String, MultiValue] = bindings.collect { case (key, value: MultiValue) => key -> value }

  lazy val readableIdentifierProperties: List[OWLAnnotationProperty] = dosdp.readable_identifiers.map { identifiers =>
    identifiers.flatMap { name =>
      val prop = safeChecker.getOWLAnnotationProperty(name)
      if (prop.isEmpty) scribe.error(s"No annotation property mapping for '$name'")
      prop
    }
  }.getOrElse(RDFSLabel :: Nil)

  private sealed trait NormalizedAnnotation {
    def property: OWLAnnotationProperty

    def subAnnotations: Set[NormalizedAnnotation]
  }

  private case class NormalizedPrintfAnnotation(property: OWLAnnotationProperty, text: String, vars: Option[List[String]], overrideColumn: Option[String], subAnnotations: Set[NormalizedAnnotation]) extends NormalizedAnnotation

  private case class NormalizedListAnnotation(property: OWLAnnotationProperty, value: String, subAnnotations: Set[NormalizedAnnotation]) extends NormalizedAnnotation

  private case class NormalizedIRIValueAnnotation(property: OWLAnnotationProperty, `var`: String, subAnnotations: Set[NormalizedAnnotation]) extends NormalizedAnnotation

}

final case class ExpandedRegexSub(regexSub: RegexSub) {

  private val groupFinder = raw"\\(\d+)".r

  private val regex = regexSub.`match`.r

  def substitute(value: String): String = {
    val valueMatchOpt = regex.findFirstMatchIn(value)
    val substitutedOpt = valueMatchOpt.map { valueMatch =>
      groupFinder.replaceAllIn(regexSub.sub, (placeholder: Match) => {
        val group = placeholder.group(1).toInt
        valueMatch.group(group)
      })
    }
    substitutedOpt match {
      case Some(substitution) => substitution
      case None               =>
        scribe.warn(s"Regex sub '$regexSub' did not match on '$value'")
        value
    }
  }

  def expandBindings(bindings: Map[String, Binding]): Map[String, Binding] = {
    val substituted: Option[(String, Binding)] = bindings.get(regexSub.in).map {
      case SingleValue(value) => regexSub.out -> SingleValue(substitute(value))
      case MultiValue(values) => regexSub.out -> MultiValue(values.map(substitute))
    }
    bindings ++ substituted
  }

}
