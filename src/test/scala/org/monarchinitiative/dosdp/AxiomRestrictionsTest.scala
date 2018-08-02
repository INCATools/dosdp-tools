package org.monarchinitiative.dosdp

import cli.Generate
import org.phenoscape.scowl._

class AxiomRestrictionsTest extends UnitSpec {

  val term = Class("http://purl.obolibrary.org/obo/ONT_0000001")
  val item = Class("http://purl.obolibrary.org/obo/ONT_0000002")
  val partOf = ObjectProperty("http://purl.obolibrary.org/obo/BFO_0000050")

  val dosdp = DOSDP.empty.copy(
    pattern_name = Some("test_restrictions_pattern"),
    classes = Some(Map("thing" -> "owl:Thing")),
    relations = Some(Map("part_of" -> "BFO:0000050")),
    vars = Some(Map("item" -> "'thing'")),
    name = Some(PrintfAnnotationOBO(None, None, "%s item", Some(List("item")))),
    subClassOf = Some(PrintfOWLConvenience(None, "'part_of' some %s", Some(List("item")))))

  val annotationAxiom = term Annotation (RDFSLabel, "http://purl.obolibrary.org/obo/ONT_0000002 item")
  val logicalAxiom = term SubClassOf (partOf some item)

  "Axiom filters" should "filter correctly" in {
    val axioms1 = Generate.renderPattern(dosdp, OBOPrefixes, Iterator(Map("defined_class" -> "ONT:0000001", "item" -> "ONT:0000002", "axiom_filter" -> "all")), None, true, true, None)
    axioms1(annotationAxiom) shouldEqual true
    axioms1(logicalAxiom) shouldEqual true
    val axioms2 = Generate.renderPattern(dosdp, OBOPrefixes, Iterator(Map("defined_class" -> "ONT:0000001", "item" -> "ONT:0000002", "axiom_filter" -> "all")), None, true, true, Some("axiom_filter"))
    axioms2(annotationAxiom) shouldEqual true
    axioms2(logicalAxiom) shouldEqual true
    val axioms3 = Generate.renderPattern(dosdp, OBOPrefixes, Iterator(Map("defined_class" -> "ONT:0000001", "item" -> "ONT:0000002", "axiom_filter" -> "logical")), None, true, true, Some("axiom_filter"))
    axioms3(annotationAxiom) shouldEqual false
    axioms3(logicalAxiom) shouldEqual true
    val axioms4 = Generate.renderPattern(dosdp, OBOPrefixes, Iterator(Map("defined_class" -> "ONT:0000001", "item" -> "ONT:0000002", "axiom_filter" -> "annotation")), None, true, true, Some("axiom_filter"))
    axioms4(annotationAxiom) shouldEqual true
    axioms4(logicalAxiom) shouldEqual false
    val axioms5 = Generate.renderPattern(dosdp, OBOPrefixes, Iterator(Map("defined_class" -> "ONT:0000001", "item" -> "ONT:0000002", "axiom_filter" -> "annotation")), None, false, false, Some("axiom_filter"))
    axioms5(annotationAxiom) shouldEqual true
    axioms5(logicalAxiom) shouldEqual false
    val axioms6 = Generate.renderPattern(dosdp, OBOPrefixes, Iterator(Map("defined_class" -> "ONT:0000001", "item" -> "ONT:0000002", "axiom_filter" -> "")), None, true, false, Some("axiom_filter"))
    axioms6(annotationAxiom) shouldEqual false
    axioms6(logicalAxiom) shouldEqual true
    assertThrows[UnsupportedOperationException] {
      Generate.renderPattern(dosdp, OBOPrefixes, Iterator(Map("defined_class" -> "ONT:0000001", "item" -> "ONT:0000002", "axiom_filter" -> "blah")), None, true, false, Some("axiom_filter"))
    }
  }

}