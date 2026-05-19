package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.Generate
import org.monarchinitiative.dosdp.{AxiomType => DOSDPAxiomType}
import org.phenoscape.scowl._
import org.semanticweb.owlapi.model.{OWLEquivalentClassesAxiom, OWLSubClassOfAxiom}
import zio.logging._
import zio.test.Assertion._
import zio.test._

import scala.jdk.CollectionConverters._

/**
 * Two correctness invariants on pattern compilation:
 *
 *   1. A pattern variable whose name contains a space (e.g. `cell type`) keeps
 *      that name in `classVarSlots`. `DOSDP.variableToIRI` normalizes spaces
 *      to underscores when minting `urn:dosdp:` IRIs, but row bindings stay
 *      keyed by the original TSV column name; recovering the original name
 *      (not the underscored form) is what lets `allSlotsBound` find the
 *      binding.
 *
 *   2. A `subClassOf` / `equivalentTo` / `disjointWith` / non-GCI
 *      `logical_axioms` entry with neither `text` nor `multi_clause` emits no
 *      logical axiom — an annotations-only or empty entry must not surface as
 *      `EquivalentClasses(defined_class, owl:Thing)` (or similar).
 */
object CompiledPatternCorrectnessTest extends DefaultRunnableSpec {

  private val emptyOWLSource = AxiomRestrictionsTest.OboInOwlSource

  val spec = suite("CompiledPattern correctness") (
    testM("var name with a space — equivalentTo emits axiom with row's binding") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("space_in_varname"),
        classes = Some(Map("thing" -> "owl:Thing")),
        relations = Some(Map("part_of" -> "BFO:0000050")),
        vars = Some(Map("cell type" -> "'thing'", "organism" -> "'thing'")),
        equivalentTo = Some(PrintfOWLConvenience(
          annotations = None,
          text = Some("%s and ('part_of' some %s)"),
          vars = Some(List("cell type", "organism")))))
      val row = Map(
        "defined_class" -> "EX:0001",
        "cell type" -> "UBERON:0000001",
        "organism" -> "NCBITaxon:9606")
      for {
        axioms <- Generate.renderPattern(pattern, OBOPrefixes, List(row), None,
          true, false, None, false, emptyOWLSource, false, Map.empty)
      } yield {
        val equivAxioms = axioms.collect { case e: OWLEquivalentClassesAxiom => e }
        val signature = equivAxioms.flatMap(_.getSignature.asScala).map(_.getIRI.toString)
        assert(equivAxioms.size)(equalTo(1)) &&
          assert(signature)(contains("http://purl.obolibrary.org/obo/UBERON_0000001")) &&
          assert(signature)(contains("http://purl.obolibrary.org/obo/NCBITaxon_9606")) &&
          Harness.assertNoPlaceholderIRIs(axioms)
      }
    },
    testM("var name with a space — subClassOf emits axiom with row's binding") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("space_in_varname_sub"),
        classes = Some(Map("thing" -> "owl:Thing")),
        relations = Some(Map("part_of" -> "BFO:0000050")),
        vars = Some(Map("cell type" -> "'thing'")),
        subClassOf = Some(PrintfOWLConvenience(
          annotations = None,
          text = Some("'part_of' some %s"),
          vars = Some(List("cell type")))))
      val row = Map("defined_class" -> "EX:0001", "cell type" -> "UBERON:0000001")
      for {
        axioms <- Generate.renderPattern(pattern, OBOPrefixes, List(row), None,
          true, false, None, false, emptyOWLSource, false, Map.empty)
      } yield {
        val subAxioms = axioms.collect { case s: OWLSubClassOfAxiom => s }
        val signature = subAxioms.flatMap(_.getSignature.asScala).map(_.getIRI.toString)
        assert(subAxioms.size)(equalTo(1)) &&
          assert(signature)(contains("http://purl.obolibrary.org/obo/UBERON_0000001")) &&
          Harness.assertNoPlaceholderIRIs(axioms)
      }
    },
    testM("var name with a space — logical_axioms entry emits axiom with row's binding") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("space_in_varname_logical"),
        classes = Some(Map("thing" -> "owl:Thing")),
        relations = Some(Map("part_of" -> "BFO:0000050")),
        vars = Some(Map("cell type" -> "'thing'")),
        logical_axioms = Some(List(PrintfOWL(
          annotations = None,
          axiom_type = DOSDPAxiomType.SubClassOf,
          text = Some("'part_of' some %s"),
          vars = Some(List("cell type"))))))
      val row = Map("defined_class" -> "EX:0001", "cell type" -> "UBERON:0000001")
      for {
        axioms <- Generate.renderPattern(pattern, OBOPrefixes, List(row), None,
          true, false, None, false, emptyOWLSource, false, Map.empty)
      } yield {
        val subAxioms = axioms.collect { case s: OWLSubClassOfAxiom => s }
        val signature = subAxioms.flatMap(_.getSignature.asScala).map(_.getIRI.toString)
        assert(subAxioms.size)(equalTo(1)) &&
          assert(signature)(contains("http://purl.obolibrary.org/obo/UBERON_0000001"))
      }
    },
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
    }
  ).provideCustomLayer(Logging.consoleErr())

}
