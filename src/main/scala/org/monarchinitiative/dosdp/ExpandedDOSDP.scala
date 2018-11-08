package org.monarchinitiative.dosdp

import scala.collection.JavaConverters._
import scala.util.matching.Regex.Match

import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntaxClassExpressionParser
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntaxInlineAxiomParser
import org.semanticweb.owlapi.model.OWLAnnotation
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom
import org.semanticweb.owlapi.model.OWLAnnotationProperty
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLClassExpression

import com.typesafe.scalalogging.LazyLogging

/**
  * Wraps a DOSDP data structure with functionality dependent on expanding IDs into IRIs
  */
final case class ExpandedDOSDP(dosdp: DOSDP, prefixes: PartialFunction[String, String]) extends LazyLogging {

  lazy val checker = new DOSDPEntityChecker(dosdp, prefixes)
  lazy val safeChecker = new SafeOWLEntityChecker(checker)
  private lazy val expressionParser = new ManchesterOWLSyntaxClassExpressionParser(OWLManager.getOWLDataFactory, checker)
  private lazy val axiomParser = new ManchesterOWLSyntaxInlineAxiomParser(OWLManager.getOWLDataFactory, checker)

  private type Bindings = Map[String, Binding]

  val substitutions: Seq[ExpandedRegexSub] = dosdp.substitutions.toSeq.flatten.map(ExpandedRegexSub)

  def allObjectProperties: Map[String, String] = dosdp.relations.getOrElse(Map.empty) ++ dosdp.objectProperties.getOrElse(Map.empty)

  def equivalentToExpression(logicalBindings: Option[Map[String, SingleValue]], annotationBindings: Option[Map[String, Binding]]): Option[(OWLClassExpression, Set[OWLAnnotation])] = dosdp.equivalentTo.map(eq => expressionFor(eq, logicalBindings) -> annotationsFor(eq, annotationBindings))

  def subClassOfExpression(logicalBindings: Option[Map[String, SingleValue]], annotationBindings: Option[Map[String, Binding]]): Option[(OWLClassExpression, Set[OWLAnnotation])] = dosdp.subClassOf.map(eq => expressionFor(eq, logicalBindings) -> annotationsFor(eq, annotationBindings))

  def disjointWithExpression(logicalBindings: Option[Map[String, SingleValue]], annotationBindings: Option[Map[String, Binding]]): Option[(OWLClassExpression, Set[OWLAnnotation])] = dosdp.disjointWith.map(eq => expressionFor(eq, logicalBindings) -> annotationsFor(eq, annotationBindings))

  def gciAxiom(logicalBindings: Option[Map[String, SingleValue]], annotationBindings: Option[Map[String, Binding]]): Option[(OWLAxiom, Set[OWLAnnotation])] = dosdp.GCI.map(gci => axiomFor(gci, logicalBindings) -> annotationsFor(gci, annotationBindings))

  def logicalAxioms(logicalBindings: Option[Map[String, SingleValue]], annotationBindings: Option[Map[String, Binding]]): Set[OWLAxiom] = (for {
    axiomDefs <- dosdp.logical_axioms.toList
    axiomDef <- axiomDefs
    defTerm = definedTerm(logicalBindings)
  } yield axiomDef.axiom_type match {
    case AxiomType.EquivalentTo => EquivalentClasses(annotationsFor(axiomDef, annotationBindings).toSeq: _*)(defTerm, expressionFor(axiomDef, logicalBindings))
    case AxiomType.SubClassOf   => SubClassOf(annotationsFor(axiomDef, annotationBindings), defTerm, expressionFor(axiomDef, logicalBindings))
    case AxiomType.DisjointWith => DisjointClasses(annotationsFor(axiomDef, annotationBindings).toSeq: _*)(defTerm, expressionFor(axiomDef, logicalBindings))
    case AxiomType.GCI          => axiomFor(axiomDef, logicalBindings).getAnnotatedAxiom(annotationsFor(axiomDef, annotationBindings).asJava)
  }).toSet

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
    vars.mapValues(expressionParser.parse)
  }

  private def expressionFor(template: PrintfText, bindings: Option[Map[String, SingleValue]]): OWLClassExpression =
    expressionParser.parse(template.replaced(bindings))

  private def axiomFor(template: PrintfText, bindings: Option[Map[String, SingleValue]]): OWLAxiom =
    axiomParser.parse(template.replaced(bindings))

  private def annotationsFor(element: PrintfText, bindings: Option[Map[String, Binding]]): Set[OWLAnnotation] =
    (for {
      annotations <- element.annotations.toSeq
      annotation <- annotations
      normalizedAnnotation = normalizeAnnotation(annotation)
      owlAnnotation <- translateAnnotations(normalizedAnnotation, bindings)
    } yield owlAnnotation).toSet

  def filledAnnotationAxioms(bindings: Option[Bindings]): Set[OWLAnnotationAssertionAxiom] = {
    val definedTerm = (for {
      actualBindings <- bindings
      SingleValue(value) <- actualBindings.get(DOSDP.DefinedClassVariable)
      iri <- Prefixes.idToIRI(value, prefixes)
    } yield Class(iri)).getOrElse(term)
    for {
      normalizedAnnotationField <- normalizedOBOAnnotations ++ dosdp.annotations.toList.flatten.map(normalizeAnnotation)
      annotation <- translateAnnotations(normalizedAnnotationField, bindings)
    } yield AnnotationAssertion(annotation.getAnnotations.asScala.toSet, annotation.getProperty, definedTerm, annotation.getValue)
  }

  private def normalizedOBOAnnotations: Set[NormalizedAnnotation] = {
    import PrintfAnnotationOBO._
    Map(
      dosdp.name.toSet -> Name,
      dosdp.comment.toSet -> Comment,
      dosdp.`def`.toSet -> Def,
      dosdp.namespace.toSet -> Namespace,
      dosdp.exact_synonym.toSet -> ExactSynonym,
      dosdp.narrow_synonym.toSet -> NarrowSynonym,
      dosdp.related_synonym.toSet -> RelatedSynonym,
      dosdp.broad_synonym.toSet -> BroadSynonym,
      dosdp.xref.toSet -> Xref,
      dosdp.generated_synonyms.toSet.flatten -> ExactSynonym,
      dosdp.generated_narrow_synonyms.toSet.flatten -> NarrowSynonym,
      dosdp.generated_broad_synonyms.toSet.flatten -> BroadSynonym,
      dosdp.generated_related_synonyms.toSet.flatten -> RelatedSynonym).flatMap {
      case (value, property) => value.map(ann => normalizeOBOAnnotation(ann, property))
    }.toSet
  }

  private def translateAnnotations(annotationField: NormalizedAnnotation, bindings: Option[Bindings]): Set[OWLAnnotation] = annotationField match {
    case NormalizedPrintfAnnotation(prop, text, vars, subAnnotations) => Set(Annotation(
      subAnnotations.flatMap(translateAnnotations(_, bindings)),
      prop,
      PrintfText.replaced(text, vars, bindings.map(singleValueBindings))))
    case NormalizedListAnnotation(prop, value, subAnnotations)        =>
      // If no variable bindings are passed in, dummy value is filled in using variable name
      val multiValBindingsOpt = bindings.map(multiValueBindings)
      val bindingsMap = multiValBindingsOpt.getOrElse(Map(value -> MultiValue(Set("'$" + value + "'"))))
      val listValue = bindingsMap(value)
      listValue.value.map(v => Annotation(subAnnotations.flatMap(translateAnnotations(_, bindings)), prop, v))
  }

  private def normalizeAnnotation(annotation: Annotations): NormalizedAnnotation = annotation match {
    case PrintfAnnotation(anns, ap, text, vars) => NormalizedPrintfAnnotation(
      safeChecker.getOWLAnnotationProperty(ap).getOrElse(throw new RuntimeException(s"No annotation property binding: $ap")),
      text,
      vars,
      anns.toSet.flatten.map(normalizeAnnotation))
    case ListAnnotation(anns, ap, value)        => NormalizedListAnnotation(
      safeChecker.getOWLAnnotationProperty(ap).getOrElse(throw new RuntimeException(s"No annotation property binding: $ap")),
      value,
      anns.toSet.flatten.map(normalizeAnnotation))
  }

  private def normalizeOBOAnnotation(annotation: OBOAnnotations, property: OWLAnnotationProperty): NormalizedAnnotation = annotation match {
    case PrintfAnnotationOBO(anns, xrefs, text, vars) => NormalizedPrintfAnnotation(
      property,
      text,
      vars,
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
      if (prop.isEmpty) logger.error(s"No annotation property mapping for '$name'")
      prop
    }
  }.getOrElse(RDFSLabel :: Nil)

  private sealed trait NormalizedAnnotation {
    def property: OWLAnnotationProperty

    def subAnnotations: Set[NormalizedAnnotation]
  }

  private case class NormalizedPrintfAnnotation(property: OWLAnnotationProperty, text: String, vars: Option[List[String]], subAnnotations: Set[NormalizedAnnotation]) extends NormalizedAnnotation

  private case class NormalizedListAnnotation(property: OWLAnnotationProperty, value: String, subAnnotations: Set[NormalizedAnnotation]) extends NormalizedAnnotation

}

final case class ExpandedRegexSub(regexSub: RegexSub) extends LazyLogging {

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
        logger.warn(s"Regex sub '$regexSub' did not match on '$value'")
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
