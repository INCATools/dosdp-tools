package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.Generate
import org.monarchinitiative.dosdp.{AxiomType => DOSDPAxiomType}
import org.phenoscape.scowl._
import org.semanticweb.owlapi.model.{OWLEquivalentClassesAxiom, OWLSubClassOfAxiom}
import zio.logging._
import zio.test.Assertion._
import zio.test._

/**
 * Correctness invariants on pattern compilation:
 *
 *   1. A `subClassOf` / `equivalentTo` / `disjointWith` / non-GCI
 *      `logical_axioms` entry with neither `text` nor `multi_clause` emits no
 *      logical axiom — an annotations-only or empty entry must not surface as
 *      `EquivalentClasses(defined_class, owl:Thing)` (or similar).
 */
object CompiledPatternCorrectnessTest extends DefaultRunnableSpec {

  private val emptyOWLSource = AxiomRestrictionsTest.OboInOwlSource

  val spec = suite("CompiledPattern correctness") (
    testM("empty equivalentTo (no text, no multi_clause) emits no logical axiom") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("empty_equiv"),
        classes = Some(Map("thing" -> "owl:Thing")),
        vars = Some(Map("structure" -> "'thing'")),
        equivalentTo = Some(PrintfOWLConvenience(None, None, None, None)),
        subClassOf = Some(PrintfOWLConvenience(None, Some("%s"), Some(List("structure")))))
      val row = Map("defined_class" -> "EX:0001", "structure" -> "UBERON:0000001")
      for {
        axioms <- Generate.renderPattern(pattern, OBOPrefixes, List(row), None,
          true, false, None, false, emptyOWLSource, false, Map.empty)
      } yield {
        val equivAxioms = axioms.collect { case e: OWLEquivalentClassesAxiom => e }
        val subAxioms = axioms.collect { case s: OWLSubClassOfAxiom => s }
        assert(equivAxioms.size)(equalTo(0)) &&
          assert(subAxioms.size)(equalTo(1))
      }
    },
    testM("empty subClassOf emits no logical axiom") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("empty_sub"),
        classes = Some(Map("thing" -> "owl:Thing")),
        vars = Some(Map("structure" -> "'thing'")),
        subClassOf = Some(PrintfOWLConvenience(None, None, None, None)))
      val row = Map("defined_class" -> "EX:0001", "structure" -> "UBERON:0000001")
      for {
        axioms <- Generate.renderPattern(pattern, OBOPrefixes, List(row), None,
          true, false, None, false, emptyOWLSource, false, Map.empty)
      } yield assert(axioms.collect { case s: OWLSubClassOfAxiom => s }.size)(equalTo(0))
    },
    testM("empty disjointWith emits no logical axiom") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("empty_disjoint"),
        classes = Some(Map("thing" -> "owl:Thing")),
        vars = Some(Map("structure" -> "'thing'")),
        disjointWith = Some(PrintfOWLConvenience(None, None, None, None)))
      val row = Map("defined_class" -> "EX:0001", "structure" -> "UBERON:0000001")
      for {
        axioms <- Generate.renderPattern(pattern, OBOPrefixes, List(row), None,
          true, false, None, false, emptyOWLSource, false, Map.empty)
      } yield assert(axioms.exists(_.isInstanceOf[org.semanticweb.owlapi.model.OWLDisjointClassesAxiom]))(isFalse)
    },
    testM("empty non-GCI logical_axioms entry is skipped") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("empty_logical_axiom"),
        classes = Some(Map("thing" -> "owl:Thing")),
        vars = Some(Map("structure" -> "'thing'")),
        logical_axioms = Some(List(PrintfOWL(
          annotations = None,
          axiom_type = DOSDPAxiomType.EquivalentTo,
          text = None,
          vars = None))))
      val row = Map("defined_class" -> "EX:0001", "structure" -> "UBERON:0000001")
      for {
        axioms <- Generate.renderPattern(pattern, OBOPrefixes, List(row), None,
          true, false, None, false, emptyOWLSource, false, Map.empty)
      } yield assert(axioms.collect { case e: OWLEquivalentClassesAxiom => e }.size)(equalTo(0))
    },
    // H1 — empty multi_clause containers must not surface as `defined_class ≡ owl:Thing`.
    testM("equivalentTo with empty multi_clause clauses list emits no axiom") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("empty_multi_clause_list"),
        classes = Some(Map("thing" -> "owl:Thing")),
        vars = Some(Map("structure" -> "'thing'")),
        equivalentTo = Some(PrintfOWLConvenience(None, None, None,
          Some(MultiClausePrintf(Some(" and "), Some(Nil))))))
      val row = Map("defined_class" -> "EX:0001", "structure" -> "UBERON:0000001")
      for {
        axioms <- Generate.renderPattern(pattern, OBOPrefixes, List(row), None,
          true, false, None, false, emptyOWLSource, false, Map.empty)
      } yield assert(axioms.collect { case e: OWLEquivalentClassesAxiom => e }.size)(equalTo(0)) &&
        Harness.assertNoPlaceholderIRIs(axioms)
    },
    testM("equivalentTo with multi_clause clauses=None emits no axiom") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("empty_multi_clause_none"),
        classes = Some(Map("thing" -> "owl:Thing")),
        vars = Some(Map("structure" -> "'thing'")),
        equivalentTo = Some(PrintfOWLConvenience(None, None, None,
          Some(MultiClausePrintf(Some(" and "), None)))))
      val row = Map("defined_class" -> "EX:0001", "structure" -> "UBERON:0000001")
      for {
        axioms <- Generate.renderPattern(pattern, OBOPrefixes, List(row), None,
          true, false, None, false, emptyOWLSource, false, Map.empty)
      } yield assert(axioms.collect { case e: OWLEquivalentClassesAxiom => e }.size)(equalTo(0)) &&
        Harness.assertNoPlaceholderIRIs(axioms)
    },
    // H2 — annotations declared on an otherwise-empty logical block do not anchor
    // a spurious `defined_class ≡ owl:Thing` axiom.
    testM("annotations-only equivalentTo (no text, no multi_clause) emits no logical axiom") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("annotations_only_equiv"),
        classes = Some(Map("thing" -> "owl:Thing")),
        annotationProperties = Some(Map("comment" -> "rdfs:comment")),
        vars = Some(Map("structure" -> "'thing'")),
        equivalentTo = Some(PrintfOWLConvenience(
          annotations = Some(List(PrintfAnnotation(None, "comment", Some("note"), None, None, None, None))),
          text = None, vars = None, multi_clause = None)))
      val row = Map("defined_class" -> "EX:0001", "structure" -> "UBERON:0000001")
      for {
        axioms <- Generate.renderPattern(pattern, OBOPrefixes, List(row), None,
          true, true, None, false, emptyOWLSource, false, Map.empty)
      } yield assert(axioms.collect { case e: OWLEquivalentClassesAxiom => e }.size)(equalTo(0))
    },
    // H4 — empty `logical_axioms` entries for each non-GCI axiom_type drop silently
    // through the same `Option[CompiledLogicalAxiom]` path.
    testM("empty SubClassOf logical_axioms entry is skipped") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("empty_logical_sub"),
        classes = Some(Map("thing" -> "owl:Thing")),
        vars = Some(Map("structure" -> "'thing'")),
        logical_axioms = Some(List(PrintfOWL(None, DOSDPAxiomType.SubClassOf, None, None))))
      val row = Map("defined_class" -> "EX:0001", "structure" -> "UBERON:0000001")
      for {
        axioms <- Generate.renderPattern(pattern, OBOPrefixes, List(row), None,
          true, false, None, false, emptyOWLSource, false, Map.empty)
      } yield assert(axioms.collect { case s: OWLSubClassOfAxiom => s }.size)(equalTo(0))
    },
    testM("empty DisjointWith logical_axioms entry is skipped") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("empty_logical_disjoint"),
        classes = Some(Map("thing" -> "owl:Thing")),
        vars = Some(Map("structure" -> "'thing'")),
        logical_axioms = Some(List(PrintfOWL(None, DOSDPAxiomType.DisjointWith, None, None))))
      val row = Map("defined_class" -> "EX:0001", "structure" -> "UBERON:0000001")
      for {
        axioms <- Generate.renderPattern(pattern, OBOPrefixes, List(row), None,
          true, false, None, false, emptyOWLSource, false, Map.empty)
      } yield assert(axioms.exists(_.isInstanceOf[org.semanticweb.owlapi.model.OWLDisjointClassesAxiom]))(isFalse)
    },
    // Empty GCI templates compile to no axiom — symmetric with the
    // class-expression types, matching the legacy `PrintfText.replaced(None)`
    // silent-drop behavior.
    testM("empty top-level GCI body emits no axiom") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("empty_gci"),
        classes = Some(Map("thing" -> "owl:Thing")),
        vars = Some(Map("structure" -> "'thing'")),
        GCI = Some(PrintfOWLConvenience(None, None, None, None)),
        subClassOf = Some(PrintfOWLConvenience(None, Some("%s"), Some(List("structure")))))
      val row = Map("defined_class" -> "EX:0001", "structure" -> "UBERON:0000001")
      for {
        axioms <- Generate.renderPattern(pattern, OBOPrefixes, List(row), None,
          true, false, None, false, emptyOWLSource, false, Map.empty)
      } yield assert(axioms.collect { case s: OWLSubClassOfAxiom => s }.size)(equalTo(1))
    },
    testM("empty GCI logical_axioms entry is skipped") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("empty_logical_gci"),
        classes = Some(Map("thing" -> "owl:Thing")),
        vars = Some(Map("structure" -> "'thing'")),
        logical_axioms = Some(List(PrintfOWL(None, DOSDPAxiomType.GCI, None, None))))
      val row = Map("defined_class" -> "EX:0001", "structure" -> "UBERON:0000001")
      for {
        axioms <- Generate.renderPattern(pattern, OBOPrefixes, List(row), None,
          true, false, None, false, emptyOWLSource, false, Map.empty)
      } yield assert(axioms.size)(equalTo(0))
    },
    testM("annotations-only top-level GCI emits no axiom") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("annotations_only_gci"),
        classes = Some(Map("thing" -> "owl:Thing")),
        annotationProperties = Some(Map("comment" -> "rdfs:comment")),
        vars = Some(Map("structure" -> "'thing'")),
        GCI = Some(PrintfOWLConvenience(
          annotations = Some(List(PrintfAnnotation(None, "comment", Some("note"), None, None, None, None))),
          text = None, vars = None, multi_clause = None)))
      val row = Map("defined_class" -> "EX:0001", "structure" -> "UBERON:0000001")
      for {
        axioms <- Generate.renderPattern(pattern, OBOPrefixes, List(row), None,
          true, true, None, false, emptyOWLSource, false, Map.empty)
      } yield assert(axioms.size)(equalTo(0))
    }
  ).provideCustomLayer(Logging.consoleErr())

}
