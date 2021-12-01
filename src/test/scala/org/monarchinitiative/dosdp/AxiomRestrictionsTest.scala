package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.Generate
import org.phenoscape.scowl.{not => _, _}
import org.semanticweb.owlapi.model.{OWLAnnotationAssertionAxiom, OWLAnnotationProperty, OWLClass, OWLObjectProperty, OWLSubClassOfAxiom}
import zio.test.Assertion._
import zio.test._

object AxiomRestrictionsTest extends DefaultRunnableSpec {

  val term: OWLClass = Class("http://purl.obolibrary.org/obo/ONT_0000001")
  val item: OWLClass = Class("http://purl.obolibrary.org/obo/ONT_0000002")
  val partOf: OWLObjectProperty = ObjectProperty("http://purl.obolibrary.org/obo/BFO_0000050")
  val OboInOwlSource: OWLAnnotationProperty = AnnotationProperty("http://www.geneontology.org/formats/oboInOwl#source")

  val dosdp: DOSDP = DOSDP.empty.copy(
    pattern_name = Some("test_restrictions_pattern"),
    classes = Some(Map("thing" -> "owl:Thing")),
    relations = Some(Map("part_of" -> "BFO:0000050")),
    vars = Some(Map("item" -> "'thing'")),
    name = Some(PrintfAnnotationOBO(None, None, Some("%s item"), Some(List("item")), None)),
    subClassOf = Some(PrintfOWLConvenience(None, Some("'part_of' some %s"), Some(List("item")))))

  val annotationAxiom: OWLAnnotationAssertionAxiom = term Annotation(RDFSLabel, "http://purl.obolibrary.org/obo/ONT_0000002 item")
  val logicalAxiom: OWLSubClassOfAxiom = term SubClassOf (partOf some item)

  def spec = suite("Axiom filters") {
    testM("Axiom filters should filter correctly") {
      for {
        axioms1 <- Generate.renderPattern(dosdp, OBOPrefixes, List(Map("defined_class" -> "ONT:0000001", "item" -> "ONT:0000002", "axiom_filter" -> "all")), None, true, true, None, false, OboInOwlSource, false, Map.empty)
        axioms2 <- Generate.renderPattern(dosdp, OBOPrefixes, List(Map("defined_class" -> "ONT:0000001", "item" -> "ONT:0000002", "axiom_filter" -> "all")), None, true, true, Some("axiom_filter"), false, OboInOwlSource, false, Map.empty)
        axioms3 <- Generate.renderPattern(dosdp, OBOPrefixes, List(Map("defined_class" -> "ONT:0000001", "item" -> "ONT:0000002", "axiom_filter" -> "logical")), None, true, true, Some("axiom_filter"), false, OboInOwlSource, false, Map.empty)
        axioms4 <- Generate.renderPattern(dosdp, OBOPrefixes, List(Map("defined_class" -> "ONT:0000001", "item" -> "ONT:0000002", "axiom_filter" -> "annotation")), None, true, true, Some("axiom_filter"), false, OboInOwlSource, false, Map.empty)
        axioms5 <- Generate.renderPattern(dosdp, OBOPrefixes, List(Map("defined_class" -> "ONT:0000001", "item" -> "ONT:0000002", "axiom_filter" -> "annotation")), None, false, false, Some("axiom_filter"), false, OboInOwlSource, false, Map.empty)
        axioms6 <- Generate.renderPattern(dosdp, OBOPrefixes, List(Map("defined_class" -> "ONT:0000001", "item" -> "ONT:0000002", "axiom_filter" -> "")), None, true, false, Some("axiom_filter"), false, OboInOwlSource, false, Map.empty)
        axioms7 <- Generate.renderPattern(dosdp, OBOPrefixes, List(Map("defined_class" -> "ONT:0000001", "item" -> "ONT:0000002", "axiom_filter" -> "blah")), None, true, false, Some("axiom_filter"), false, OboInOwlSource, false, Map.empty).either
      } yield assert(axioms1)(contains(annotationAxiom)) &&
        assert(axioms1)(contains(logicalAxiom)) &&
        assert(axioms2)(contains(annotationAxiom)) &&
        assert(axioms2)(contains(logicalAxiom)) &&
        assert(axioms3)(not(contains(annotationAxiom))) &&
        assert(axioms3)(contains(logicalAxiom)) &&
        assert(axioms4)(contains(annotationAxiom)) &&
        assert(axioms4)(not(contains(logicalAxiom))) &&
        assert(axioms5)(contains(annotationAxiom)) &&
        assert(axioms5)(not(contains(logicalAxiom))) &&
        assert(axioms6)(not(contains(annotationAxiom))) &&
        assert(axioms6)(contains(logicalAxiom)) &&
        assert(axioms7)(isLeft)
    }
  }

}
