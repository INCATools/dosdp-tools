package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.Generate
import org.monarchinitiative.dosdp.{AxiomType => DOSDPAxiomType}
import org.phenoscape.scowl._
import zio.logging._
import zio.test.Assertion._
import zio.test._

/**
 * A row missing a binding for a variable that appears in a logical template
 * must produce no axiom mentioning that variable — the legacy behavior was
 * `PrintfText.replaced` returning `None`, dropping the axiom. With
 * parse-at-compile templates, leaving the placeholder IRI in place would
 * leak `urn:dosdp:` IRIs into the output ontology.
 */
object MissingBindingsTest extends DefaultRunnableSpec {

  private val basePattern: DOSDP = DOSDP.empty.copy(
    pattern_name = Some("missing_bindings"),
    classes = Some(Map("thing" -> "owl:Thing")),
    relations = Some(Map("part_of" -> "BFO:0000050")),
    dataProperties = Some(Map("has_age" -> "RO:0002000")),
    vars = Some(Map("structure" -> "'thing'", "taxon" -> "'thing'")),
    data_vars = Some(Map("min_age" -> "xsd:integer"))
  )

  val spec = suite("Missing-binding row drops the axiom") (
    testM("equivalentTo with missing class var emits no axiom") {
      val pattern = basePattern.copy(
        equivalentTo = Some(PrintfOWLConvenience(None, Some("%s and ('part_of' some %s)"), Some(List("structure", "taxon"))))
      )
      val row = Map("defined_class" -> "EX:0001", "structure" -> "UBERON:0000001") // taxon missing
      for {
        axioms <- Generate.renderPattern(pattern, OBOPrefixes, List(row), None, true, false, None, false, AxiomRestrictionsTest.OboInOwlSource, false, Map.empty)
      } yield Harness.assertNoPlaceholderIRIs(axioms) &&
        assert(axioms.exists(_.isInstanceOf[org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom]))(isFalse)
    },
    testM("subClassOf with missing class var emits no axiom") {
      val pattern = basePattern.copy(
        subClassOf = Some(PrintfOWLConvenience(None, Some("'part_of' some %s"), Some(List("structure"))))
      )
      val row = Map("defined_class" -> "EX:0001") // structure missing
      for {
        axioms <- Generate.renderPattern(pattern, OBOPrefixes, List(row), None, true, false, None, false, AxiomRestrictionsTest.OboInOwlSource, false, Map.empty)
      } yield Harness.assertNoPlaceholderIRIs(axioms) &&
        assert(axioms.exists(_.isInstanceOf[org.semanticweb.owlapi.model.OWLSubClassOfAxiom]))(isFalse)
    },
    testM("disjointWith with missing class var emits no axiom") {
      val pattern = basePattern.copy(
        disjointWith = Some(PrintfOWLConvenience(None, Some("'part_of' some %s"), Some(List("structure"))))
      )
      val row = Map("defined_class" -> "EX:0001")
      for {
        axioms <- Generate.renderPattern(pattern, OBOPrefixes, List(row), None, true, false, None, false, AxiomRestrictionsTest.OboInOwlSource, false, Map.empty)
      } yield Harness.assertNoPlaceholderIRIs(axioms)
    },
    testM("GCI with missing class var emits no axiom") {
      val pattern = basePattern.copy(
        GCI = Some(PrintfOWLConvenience(None, Some("'part_of' some %s SubClassOf: 'thing'"), Some(List("structure"))))
      )
      val row = Map("defined_class" -> "EX:0001")
      for {
        axioms <- Generate.renderPattern(pattern, OBOPrefixes, List(row), None, true, false, None, false, AxiomRestrictionsTest.OboInOwlSource, false, Map.empty)
      } yield Harness.assertNoPlaceholderIRIs(axioms)
    },
    testM("logical_axioms entry with missing class var emits no axiom") {
      val pattern = basePattern.copy(
        logical_axioms = Some(List(PrintfOWL(
          annotations = None,
          axiom_type = DOSDPAxiomType.SubClassOf,
          text = Some("'part_of' some %s"),
          vars = Some(List("structure")))))
      )
      val row = Map("defined_class" -> "EX:0001")
      for {
        axioms <- Generate.renderPattern(pattern, OBOPrefixes, List(row), None, true, false, None, false, AxiomRestrictionsTest.OboInOwlSource, false, Map.empty)
      } yield Harness.assertNoPlaceholderIRIs(axioms)
    },
    testM("data_var in a Manchester slot drops the axiom when missing") {
      val pattern = basePattern.copy(
        equivalentTo = Some(PrintfOWLConvenience(None,
          Some("'thing' and ('has_age' some xsd:integer[>= %s])"),
          Some(List("min_age"))))
      )
      val row = Map("defined_class" -> "EX:0001") // min_age missing
      for {
        axioms <- Generate.renderPattern(pattern, OBOPrefixes, List(row), None, true, false, None, false, AxiomRestrictionsTest.OboInOwlSource, false, Map.empty)
      } yield assert(axioms.exists(_.isInstanceOf[org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom]))(isFalse)
    },
    testM("multi_clause: missing-binding clause is dropped, satisfied clauses survive") {
      // subClassOf: A and (part_of some B) — clause 2 (B) bound, clause 1 (A) missing.
      val clauseA = PrintfClause("%s", Some(List("structure")), None)
      val clauseB = PrintfClause("'part_of' some %s", Some(List("taxon")), None)
      val pattern = basePattern.copy(
        subClassOf = Some(PrintfOWLConvenience(None, None, None,
          Some(MultiClausePrintf(Some(" and "), Some(List(clauseA, clauseB))))))
      )
      val row = Map("defined_class" -> "EX:0001", "taxon" -> "UBERON:0000001") // structure missing
      for {
        axioms <- Generate.renderPattern(pattern, OBOPrefixes, List(row), None, true, false, None, false, AxiomRestrictionsTest.OboInOwlSource, false, Map.empty)
      } yield Harness.assertNoPlaceholderIRIs(axioms) &&
        assert(axioms.exists(_.isInstanceOf[org.semanticweb.owlapi.model.OWLSubClassOfAxiom]))(isTrue)
    },
    testM("multi_clause: all clauses missing drops the entire expression") {
      val clauseA = PrintfClause("%s", Some(List("structure")), None)
      val clauseB = PrintfClause("'part_of' some %s", Some(List("taxon")), None)
      val pattern = basePattern.copy(
        subClassOf = Some(PrintfOWLConvenience(None, None, None,
          Some(MultiClausePrintf(Some(" and "), Some(List(clauseA, clauseB))))))
      )
      val row = Map("defined_class" -> "EX:0001") // both missing
      for {
        axioms <- Generate.renderPattern(pattern, OBOPrefixes, List(row), None, true, false, None, false, AxiomRestrictionsTest.OboInOwlSource, false, Map.empty)
      } yield assert(axioms.exists(_.isInstanceOf[org.semanticweb.owlapi.model.OWLSubClassOfAxiom]))(isFalse) &&
        Harness.assertNoPlaceholderIRIs(axioms)
    }
  ).provideCustomLayer(Logging.consoleErr())

}
