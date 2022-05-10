package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.Generate
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import zio.logging._
import zio.test._

import scala.jdk.CollectionConverters._

object MultipleLabelsTest extends DefaultRunnableSpec {

  val term: OWLClass = Class("http://purl.obolibrary.org/obo/ONT_0000001")
  val item: OWLClass = Class("http://purl.obolibrary.org/obo/ONT_0000002")
  val partOf: OWLObjectProperty = ObjectProperty("http://purl.obolibrary.org/obo/BFO_0000050")
  val OboInOwlSource: OWLAnnotationProperty = AnnotationProperty("http://www.geneontology.org/formats/oboInOwl#source")

  val dosdp: DOSDP = DOSDP.empty.copy(
    pattern_name = Some("test_multiple_labels_pattern"),
    classes = Some(Map("thing" -> "owl:Thing")),
    vars = Some(Map("item" -> "'thing'")),
    name = Some(PrintfAnnotationOBO(None, None, Some("%s item"), Some(List("item")), None)),
  )

  val ontologyAxioms: Set[OWLAxiom] = Set(AnnotationAssertion(RDFSLabel, item, "label1"), AnnotationAssertion(RDFSLabel, item, "label2"))
  val ontology = OWLManager.createOWLOntologyManager().createOntology(ontologyAxioms.asJava)

  val annotationAxiom: OWLAnnotationAssertionAxiom = term Annotation(RDFSLabel, "http://purl.obolibrary.org/obo/ONT_0000002 item")
  val logicalAxiom: OWLSubClassOfAxiom = term SubClassOf (partOf some item)

  def spec = suite("Multiple labels") {
    testM("Only one term label should be used for a readable identifier") {
      for {
        axioms <- Generate.renderPattern(dosdp, OBOPrefixes, List(Map("defined_class" -> "ONT:0000001", "item" -> "ONT:0000002", "axiom_filter" -> "all")), Some(ontology), true, true, None, false, OboInOwlSource, false, Map.empty)
      } yield assertTrue(axioms.size == 1) && assertTrue(axioms.exists { case AnnotationAssertion(_, RDFSLabel, iri: IRI, v ^^ _) => term.getIRI == iri && v == "label1 item" })
    }
  }.provideCustomLayer(Logging.consoleErr())

}
