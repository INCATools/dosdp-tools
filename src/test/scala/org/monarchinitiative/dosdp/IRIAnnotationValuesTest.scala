package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.Generate
import org.phenoscape.scowl._
import org.semanticweb.owlapi.model.{OWLAnnotationAssertionAxiom, OWLAnnotationProperty, OWLClass}
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary
import zio.test.Assertion._
import zio.test._

object IRIAnnotationValuesTest extends DefaultRunnableSpec {

  val term: OWLClass = Class("http://purl.obolibrary.org/obo/ONT_0000001")
  val item: OWLClass = Class("http://purl.obolibrary.org/obo/ONT_0000002")
  val isDefinedBy: OWLAnnotationProperty = AnnotationProperty(OWLRDFVocabulary.RDFS_IS_DEFINED_BY.getIRI)

  val dosdp: DOSDP = DOSDP.empty.copy(
    pattern_name = Some("test_iri_values_pattern"),
    classes = Some(Map("thing" -> "owl:Thing")),
    annotationProperties = Some(Map("isDefinedBy" -> "rdfs:isDefinedBy")),
    vars = Some(Map("item" -> "'thing'")),
    annotations = Some(List(IRIValueAnnotation(None, "isDefinedBy", "item")))
  )

  val annotationAxiom: OWLAnnotationAssertionAxiom = term Annotation(isDefinedBy, item.getIRI)

  def spec = suite("IRI valued annotation") {
    testM("Axioms should contain IRI valued annotation") {
      val axioms1 = Generate.renderPattern(dosdp, OBOPrefixes, List(Map("defined_class" -> "ONT:0000001", "item" -> "ONT:0000002")), None, true, true, None, false, AxiomRestrictionsTest.OboInOwlSource, false)
      assertM(axioms1)(contains(annotationAxiom))
    }
  }

}
