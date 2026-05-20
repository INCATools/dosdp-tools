package org.monarchinitiative.dosdp

import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{IRI, OWLAxiom}
import zio.test.Assertion._
import zio.test._

/**
 * Sanity tests for the Harness invariants. The interesting case is an
 * AnnotationAssertion whose subject is a bare IRI: `OWLAxiom.getSignature`
 * does not include such subjects (they're not OWLEntities), so a
 * signature-only walk would miss a leaked placeholder there.
 */
object HarnessTest extends ZIOSpecDefault {

  private val factory = OWLManager.getOWLDataFactory
  private val placeholder: IRI = DOSDP.variableToIRI("defined_class")
  private val realIRI: IRI = IRI.create("http://purl.obolibrary.org/obo/EX_0001")

  def spec = suite("Harness")(
    test("placeholderIRIs flags a placeholder used as an annotation-assertion subject") {
      val axiom: OWLAxiom = factory.getOWLAnnotationAssertionAxiom(
        RDFSLabel, placeholder, factory.getOWLLiteral("a label"))
      assert(Harness.placeholderIRIs(Set(axiom)))(contains(placeholder.toString))
    },
    test("placeholderIRIs flags a placeholder used as an IRI-valued annotation") {
      val axiom: OWLAxiom = factory.getOWLAnnotationAssertionAxiom(
        RDFSLabel, realIRI, placeholder)
      assert(Harness.placeholderIRIs(Set(axiom)))(contains(placeholder.toString))
    },
    test("placeholderIRIs flags a placeholder used as an entity IRI (signature case)") {
      val axiom: OWLAxiom = Class(placeholder) SubClassOf Class(realIRI)
      assert(Harness.placeholderIRIs(Set(axiom)))(contains(placeholder.toString))
    },
    test("placeholderIRIs returns empty for a clean axiom set") {
      val axiom: OWLAxiom = factory.getOWLAnnotationAssertionAxiom(
        RDFSLabel, realIRI, factory.getOWLLiteral("a label"))
      assert(Harness.placeholderIRIs(Set(axiom)))(isEmpty)
    },
    test("placeholderLiterals flags a literal whose lexical form is $name") {
      val axiom: OWLAxiom = factory.getOWLAnnotationAssertionAxiom(
        RDFSLabel, realIRI, factory.getOWLLiteral("$age"))
      assert(Harness.placeholderLiterals(Set(axiom)))(contains("$age"))
    },
    test("placeholderLiterals returns empty for a clean literal") {
      val axiom: OWLAxiom = factory.getOWLAnnotationAssertionAxiom(
        RDFSLabel, realIRI, factory.getOWLLiteral("ordinary value"))
      assert(Harness.placeholderLiterals(Set(axiom)))(isEmpty)
    }
  )

}
