package org.monarchinitiative.dosdp

import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntaxClassExpressionParser
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntaxInlineAxiomParser
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom
import org.semanticweb.owlapi.model.OWLAnnotation
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom
import scala.collection.JavaConverters._
import org.semanticweb.owlapi.model.OWLAnnotationProperty
import org.semanticweb.owlapi.model.OWLAnnotationProperty

/**
 * Wraps a DOSDP data structure with functionality dependent on expanding IDs into IRIs
 */
final case class ExpandedDOSDP(dosdp: DOSDP, prefixes: PartialFunction[String, String]) {

  lazy val checker = new DOSDPEntityChecker(dosdp, prefixes)
  private lazy val expressionParser = new ManchesterOWLSyntaxClassExpressionParser(OWLManager.getOWLDataFactory, checker)
  private lazy val axiomParser = new ManchesterOWLSyntaxInlineAxiomParser(OWLManager.getOWLDataFactory, checker)

  private type Bindings = Map[String, Binding]

  def allObjectProperties: Map[String, String] = dosdp.relations.getOrElse(Map.empty) ++ dosdp.objectProperties.getOrElse(Map.empty)

  def equivalentToExpression(bindings: Option[Map[String, SingleValue]]): Option[OWLClassExpression] = dosdp.equivalentTo.map(expressionFor(_, bindings))

  def subClassOfExpression(bindings: Option[Map[String, SingleValue]]): Option[OWLClassExpression] = dosdp.subClassOf.map(expressionFor(_, bindings))

  def disjointWithExpression(bindings: Option[Map[String, SingleValue]]): Option[OWLClassExpression] = dosdp.disjointWith.map(expressionFor(_, bindings))

  def gciAxiom(bindings: Option[Map[String, SingleValue]]): Option[OWLAxiom] = dosdp.GCI.map(gci => axiomFor(gci, bindings))

  def logicalAxioms(bindings: Option[Map[String, SingleValue]]): Set[OWLAxiom] = (for {
    axiomDefs <- dosdp.logical_axioms.toList
    axiomDef <- axiomDefs
  } yield axiomDef.axiom_type match {
    case AxiomType.EquivalentTo => term EquivalentTo expressionFor(axiomDef, bindings)
    case AxiomType.SubClassOf   => term SubClassOf expressionFor(axiomDef, bindings)
    case AxiomType.DisjointWith => term DisjointWith expressionFor(axiomDef, bindings)
    case AxiomType.GCI          => axiomFor(axiomDef, bindings)
  }).toSet

  private val term = Class(DOSDP.variableToIRI(DOSDP.DefinedClassVariable))

  def filledLogicalAxioms(bindings: Option[Map[String, SingleValue]]): Set[OWLAxiom] = {
    val definedTerm = (for {
      actualBindings <- bindings
      defClass <- actualBindings.get("defined_class")
      iri <- checker.idToIRI(defClass.value)
    } yield Class(iri)).getOrElse(term)
    equivalentToExpression(bindings).map(e => (definedTerm EquivalentTo e)).toSet ++
      subClassOfExpression(bindings).map(e => (definedTerm SubClassOf e)).toSet ++
      disjointWithExpression(bindings).map(e => (definedTerm DisjointWith e)).toSet ++
      gciAxiom(bindings).toSet ++ logicalAxioms(bindings)
  }

  def varExpressions: Map[String, OWLClassExpression] = {
    val vars = dosdp.vars.getOrElse(Map.empty)
    vars.mapValues(expressionParser.parse)
  }

  private def expressionFor(template: PrintfText, bindings: Option[Map[String, SingleValue]]): OWLClassExpression =
    expressionParser.parse(template.replaced(bindings))

  private def axiomFor(template: PrintfText, bindings: Option[Map[String, SingleValue]]): OWLAxiom =
    axiomParser.parse(template.replaced(bindings))

  def filledAnnotationAxioms(bindings: Option[Bindings]): Set[OWLAnnotationAssertionAxiom] = {
    val definedTerm = (for {
      actualBindings <- bindings
      SingleValue(value) <- actualBindings.get("defined_class")
      iri <- checker.idToIRI(value)
    } yield Class(iri)).getOrElse(term)
    (oboAnnotations(bindings) ++ annotations(bindings))
      .map(ann =>
        AnnotationAssertion(ann.getAnnotations.asScala.toSet, ann.getProperty, definedTerm, ann.getValue))
  }

  def oboAnnotations(bindings: Option[Bindings]): Set[OWLAnnotation] = {
    import PrintfAnnotationOBO._
    (Map(
      dosdp.name -> Name,
      dosdp.comment -> Comment,
      dosdp.`def` -> Def,
      dosdp.exact_synonym -> ExactSynonym,
      dosdp.narrow_synonym -> NarrowSynonym,
      dosdp.related_synonym -> RelatedSynonym,
      dosdp.broad_synonym -> BroadSynonym,
      dosdp.xref -> Xref).flatMap {
        case (value, property) => value.map(v => translateOBOAnnotation(property, v, bindings)).toSet
      }).toSet
  }

  def annotations(bindings: Option[Bindings]): Set[OWLAnnotation] = (for {
    annotationDefs <- dosdp.annotations.toList
    annotationDef <- annotationDefs
    annotation <- translateAnnotations(annotationDef, bindings)
  } yield {
    annotation
  }).toSet

  //TODO check membership of variable in various variable sets: regular vs. list, regular vs. data
  // if annotation: ids must be translated to labels using readable_identifiers

  private def translateAnnotations(annotationField: Annotations, bindings: Option[Bindings]): Set[OWLAnnotation] = {
    annotationField match {
      case pfa: PrintfAnnotation =>
        Set(Annotation(
          pfa.annotations.toList.flatten.flatMap(translateAnnotations(_, bindings)).toSet,
          checker.getOWLAnnotationProperty(pfa.annotationProperty),
          pfa.replaced(bindings.map(singleValueBindings))))
      case la: ListAnnotation =>
        val ap = checker.getOWLAnnotationProperty(la.annotationProperty)
        // If no variable bindings are passed in, dummy value is filled in using variable name
        val multiValBindingsOpt = bindings.map(multiValueBindings)
        val bindingsMap = multiValBindingsOpt.getOrElse(Map(la.value -> MultiValue(Set("'$" + la.value + "'"))))
        val listValue = bindingsMap(la.value)
        listValue.value.map(v => Annotation(ap, v))
    }
  }

  private def singleValueBindings(bindings: Bindings): Map[String, SingleValue] = bindings.collect { case (key, value: SingleValue) => key -> value }

  private def multiValueBindings(bindings: Bindings): Map[String, MultiValue] = bindings.collect { case (key, value: MultiValue) => key -> value }

  private def translateOBOAnnotation(ap: OWLAnnotationProperty, pfao: PrintfAnnotationOBO, bindings: Option[Bindings]): OWLAnnotation = {
    val multiValBindingsOpt = bindings.map(multiValueBindings)
    val xrefAnnotations: Set[OWLAnnotation] = (for {
      xrefsVar <- pfao.xrefs
      // If no variable bindings are passed in, dummy value is filled in using variable name
      bindingsMap = multiValBindingsOpt.getOrElse(Map(xrefsVar -> MultiValue(Set("'$" + xrefsVar + "'"))))
      xrefValues <- bindingsMap.get(xrefsVar)
    } yield xrefValues.value.map(xrefVal => Annotation(PrintfAnnotationOBO.Xref, xrefVal)).toSet).getOrElse(Set.empty) //TODO how are list values delimited?
    val annotationAnnotations = (for {
      annotationDefs <- pfao.annotations.toList
      annotationDef <- annotationDefs
      annotation <- translateAnnotations(annotationDef, bindings)
    } yield {
      annotation
    }).toSet
    Annotation(xrefAnnotations ++ annotationAnnotations, ap, pfao.replaced(bindings.map(singleValueBindings)))
  }

  lazy val readableIdentifierProperties: List[OWLAnnotationProperty] = (dosdp.readable_identifiers.map(identifiers =>
    identifiers.map(checker.getOWLAnnotationProperty))).getOrElse(RDFSLabel :: Nil)

}