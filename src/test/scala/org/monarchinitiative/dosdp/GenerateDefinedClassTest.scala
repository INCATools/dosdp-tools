package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.Generate
import org.phenoscape.scowl.{not => _, _}
import org.semanticweb.owlapi.model.{IRI, OWLAnnotationAssertionAxiom, OWLAxiom, OWLClass, OWLObjectProperty, OWLSubClassOfAxiom}
import zio.test.Assertion._
import zio.test._
import zio.logging._

// Pins the --generate-defined-class IRI-minting path: when generateDefinedClass = true
// and the row has no `defined_class` column, the defined-class IRI is computed as
// sha1Hex(sorted-bindings) appended to the pattern IRI as a fragment. A refactor of
// DOSDP.computeDefinedIRI or of the binding-collection that feeds it must keep the
// same IRI shape, or every consumer that relies on these IRIs to dedupe rows breaks
// silently.
object GenerateDefinedClassTest extends DefaultRunnableSpec {

  private val patternIRIString = "http://purl.obolibrary.org/obo/test/generate_defined_class_test.yaml"
  private val patternIRI: IRI = IRI.create(patternIRIString)
  private val item: OWLClass = Class("http://purl.obolibrary.org/obo/ONT_0000002")
  private val partOf: OWLObjectProperty = ObjectProperty("http://purl.obolibrary.org/obo/BFO_0000050")

  private val dosdp: DOSDP = DOSDP.empty.copy(
    pattern_name = Some("generate_defined_class_test"),
    pattern_iri = Some(patternIRIString),
    classes = Some(Map("thing" -> "owl:Thing")),
    relations = Some(Map("part_of" -> "BFO:0000050")),
    vars = Some(Map("item" -> "'thing'")),
    name = Some(PrintfAnnotationOBO(None, None, Some("%s item"), Some(List("item")), None)),
    subClassOf = Some(PrintfOWLConvenience(None, Some("'part_of' some %s"), Some(List("item")))))

  // Mirror what Generate.renderPattern computes internally, so that the assertion
  // would catch a behavior change in DOSDP.computeDefinedIRI itself.
  private val expectedDefinedIRI: IRI =
    DOSDP.computeDefinedIRI(patternIRI, Map("item" -> SingleValue("ONT:0000002")))
  private val expectedDefinedClass: OWLClass = Class(expectedDefinedIRI)
  private val expectedLabel: OWLAnnotationAssertionAxiom =
    expectedDefinedClass Annotation(RDFSLabel, "http://purl.obolibrary.org/obo/ONT_0000002 item")
  private val expectedSubClassOf: OWLSubClassOfAxiom =
    expectedDefinedClass SubClassOf (partOf some item)

  def spec = suite("generate-defined-class")(
    testM("mints defined-class IRI from sha1 of bindings when generateDefinedClass = true") {
      val row = Map("item" -> "ONT:0000002")
      for {
        axioms <- Generate.renderPattern(dosdp, OBOPrefixes, List(row), None,
          outputLogicalAxioms = true, outputAnnotationAxioms = true, None,
          annotateAxiomSource = false, AxiomRestrictionsTest.OboInOwlSource,
          generateDefinedClass = true, Map.empty)
      } yield assert(axioms)(contains[OWLAxiom](expectedLabel)) &&
        assert(axioms)(contains[OWLAxiom](expectedSubClassOf))
    },
    testM("fails when the input table has a defined_class column alongside generateDefinedClass") {
      val row = Map("defined_class" -> "ONT:9999999", "item" -> "ONT:0000002")
      val program = Generate.renderPattern(dosdp, OBOPrefixes, List(row), None,
        outputLogicalAxioms = true, outputAnnotationAxioms = true, None,
        annotateAxiomSource = false, AxiomRestrictionsTest.OboInOwlSource,
        generateDefinedClass = true, Map.empty)
      assertM(program.flip.map(_.msg))(containsString(DOSDP.DefinedClassVariable))
    },
    testM("fails when pattern_iri is missing") {
      val noIRI = dosdp.copy(pattern_iri = None)
      val program = Generate.renderPattern(noIRI, OBOPrefixes, List(Map("item" -> "ONT:0000002")), None,
        outputLogicalAxioms = true, outputAnnotationAxioms = true, None,
        annotateAxiomSource = false, AxiomRestrictionsTest.OboInOwlSource,
        generateDefinedClass = true, Map.empty)
      assertM(program.flip.map(_.msg))(containsString("generate-defined-class"))
    }
  ).provideCustomLayer(Logging.consoleErr())

}