package org.monarchinitiative.dosdp

import cli.Generate
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary

class IRIAnnotationValuesTest extends UnitSpec {

  val term = Class("http://purl.obolibrary.org/obo/ONT_0000001")
  val item = Class("http://purl.obolibrary.org/obo/ONT_0000002")
  val isDefinedBy = AnnotationProperty(OWLRDFVocabulary.RDFS_IS_DEFINED_BY.getIRI)

  val dosdp = DOSDP.empty.copy(
    pattern_name = Some("test_iri_values_pattern"),
    classes = Some(Map("thing" -> "owl:Thing")),
    annotationProperties = Some(Map("isDefinedBy" -> "rdfs:isDefinedBy")),
    vars = Some(Map("item" -> "'thing'")),
    annotations = Some(List(IRIValueAnnotation(None, "isDefinedBy", "item")))
  )

  val annotationAxiom = term Annotation(isDefinedBy, item.getIRI)

  "Axioms" should "contain IRI valued annotation" in {
    val axioms1 = Generate.renderPattern(dosdp, OBOPrefixes, Iterator(Map("defined_class" -> "ONT:0000001", "item" -> "ONT:0000002")), None, true, true, None, false)
    axioms1(annotationAxiom) shouldEqual true
  }

}
