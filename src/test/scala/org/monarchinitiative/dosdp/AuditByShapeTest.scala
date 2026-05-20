package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.Generate
import org.monarchinitiative.dosdp.{AxiomType => DOSDPAxiomType}
import org.phenoscape.scowl._
import org.semanticweb.owlapi.model._
import zio.logging._
import zio.test.Assertion._
import zio.test._

import scala.jdk.CollectionConverters._

/**
 * Audit-by-shape coverage (REFACTOR_PLAN.md Phase 0.3.3).
 *
 * Each cell of the template-kind × var-kind × annotation-presence matrix
 * needs at least one positive ("renders correctly") and one
 * missing-binding negative ("drops the axiom / annotation"). The other
 * test files cover most cells; this file fills the gaps:
 *
 *   - Logical axioms (equivalentTo / disjointWith / GCI) carrying their
 *     own axiom-level annotations.
 *   - Missing-binding negatives on annotation paths (OBO, custom,
 *     permutations, override) — annotations must not survive a missing
 *     `vars` binding, including when an `override` column is also empty.
 *   - Missing-binding negatives for the three `data_var` Manchester
 *     slots not covered in `MissingBindingsTest` (value, DataOneOf,
 *     inside a cardinality filler).
 */
object AuditByShapeTest extends DefaultRunnableSpec {

  private val term: OWLClass = Class("http://purl.obolibrary.org/obo/EX_0001")
  private val anatomy: OWLClass = Class("http://purl.obolibrary.org/obo/UBERON_0000001")
  private val partOf: OWLObjectProperty = ObjectProperty("http://purl.obolibrary.org/obo/BFO_0000050")
  private val DCSource: OWLAnnotationProperty = AnnotationProperty("http://purl.org/dc/terms/source")
  private val OBORdfsLabel: OWLAnnotationProperty = PrintfAnnotationOBO.Name
  private val OBOExactSynonym: OWLAnnotationProperty = PrintfAnnotationOBO.ExactSynonym

  private val basePattern: DOSDP = DOSDP.empty.copy(
    pattern_name = Some("audit_by_shape"),
    classes = Some(Map("thing" -> "owl:Thing")),
    relations = Some(Map("part_of" -> "BFO:0000050")),
    dataProperties = Some(Map("has_age" -> "RO:0002000")),
    annotationProperties = Some(Map(
      "source" -> "http://purl.org/dc/terms/source",
      "exact_synonym" -> "oio:hasExactSynonym")),
    vars = Some(Map("structure" -> "'thing'")),
    data_vars = Some(Map("age" -> "xsd:integer")))

  private def annotationOn(axioms: Set[OWLAxiom], property: OWLAnnotationProperty): Set[OWLAnnotationAssertionAxiom] =
    axioms.collect {
      case a: OWLAnnotationAssertionAxiom if a.getProperty == property => a
    }

  /** Axiom annotations attached to logical axioms (not separate annotation-assertion axioms). */
  private def logicalAxiomAnnotationProps(axioms: Set[OWLAxiom]): Set[OWLAnnotationProperty] =
    axioms.collect {
      case sc: OWLSubClassOfAxiom         => sc.getAnnotations.asScala.toSet
      case eq: OWLEquivalentClassesAxiom  => eq.getAnnotations.asScala.toSet
      case dj: OWLDisjointClassesAxiom    => dj.getAnnotations.asScala.toSet
    }.flatten.map(_.getProperty)

  // -- Positive coverage: equivalentTo / disjointWith / GCI with axiom annotations -----------------

  private val axiomAnnotations =
    Some(List(PrintfAnnotation(None, "source", Some("annotated on %s"), Some(List("structure")), None)))

  private val equivWithAnn = basePattern.copy(
    equivalentTo = Some(PrintfOWLConvenience(
      annotations = axiomAnnotations,
      text = Some("'part_of' some %s"),
      vars = Some(List("structure")))))

  private val disjointWithAnn = basePattern.copy(
    disjointWith = Some(PrintfOWLConvenience(
      annotations = axiomAnnotations,
      text = Some("'part_of' some %s"),
      vars = Some(List("structure")))))

  private val gciWithAnn = basePattern.copy(
    GCI = Some(PrintfOWLConvenience(
      annotations = axiomAnnotations,
      text = Some("'part_of' some %s SubClassOf: 'thing'"),
      vars = Some(List("structure")))))

  private val boundRow = Map("defined_class" -> "EX:0001", "structure" -> "UBERON:0000001")
  private val unboundRow = Map("defined_class" -> "EX:0001")

  // -- Negative coverage: missing-binding annotation paths -----------------------------------------

  /** OBO `name` annotation referencing a missing class var must not emit. */
  private val oboNamePattern = basePattern.copy(
    name = Some(PrintfAnnotationOBO(None, None, Some("part of %s"), Some(List("structure")), None, None)))

  /** Custom annotation referencing a missing class var must not emit. */
  private val customAnnPattern = basePattern.copy(
    annotations = Some(List(PrintfAnnotation(None, "source", Some("about %s"), Some(List("structure")), None))))

  /** Custom annotation with permutations on a missing class var must not emit. */
  private val permutationAnnPattern = basePattern.copy(
    annotations = Some(List(PrintfAnnotation(
      annotations = None,
      annotationProperty = "exact_synonym",
      text = Some("%s, acute"),
      vars = Some(List("structure")),
      `override` = None,
      permutations = Some(List(Permutation("structure", List("exact_synonym"))))))))

  /** Annotation with both `override` and text+vars: if BOTH the override column and the
   *  template var are missing on the row, the annotation must drop entirely. */
  private val overrideAnnPattern = basePattern.copy(
    annotations = Some(List(PrintfAnnotation(
      annotations = None,
      annotationProperty = "source",
      text = Some("from %s"),
      vars = Some(List("structure")),
      `override` = Some("source_override")))))

  // -- Negative coverage: data_var missing in the three non-facet Manchester slots -----------------

  private def dataSlot(text: String): DOSDP =
    DOSDP.empty.copy(
      pattern_name = Some("data_slot_missing"),
      classes = Some(Map("thing" -> "owl:Thing")),
      dataProperties = Some(Map("has_age" -> "RO:0002000")),
      data_vars = Some(Map("age" -> "xsd:integer")),
      subClassOf = Some(PrintfOWLConvenience(None, Some(text), Some(List("age")))))

  private val dataValuePattern: DOSDP = dataSlot("'has_age' value %s")
  private val dataOneOfPattern: DOSDP = dataSlot("'has_age' some {%s}")
  private val dataCardinalityPattern: DOSDP = dataSlot("'has_age' min 1 xsd:integer[>= %s]")

  // -- Helpers --------------------------------------------------------------------------------------

  private def render(pattern: DOSDP, row: Map[String, String]) =
    Generate.renderPattern(pattern, OBOPrefixes, List(row),
      None, true, true, None, false, AxiomRestrictionsTest.OboInOwlSource, false, Map.empty)

  def spec = suite("Audit-by-shape coverage (Phase 0.3.3)")(

    // ----- Logical templates carrying axiom annotations (positive) -----

    testM("equivalentTo with an axiom annotation renders both the axiom and the annotation") {
      for {
        axioms <- render(equivWithAnn, boundRow)
        eqAxioms = axioms.collect { case e: OWLEquivalentClassesAxiom => e }
      } yield assert(eqAxioms)(isNonEmpty) &&
        assert(eqAxioms.flatMap(_.getAnnotations.asScala.map(_.getProperty)))(contains(DCSource)) &&
        Harness.assertNoPlaceholderIRIs(axioms) &&
        Harness.assertNoPlaceholderLiterals(axioms)
    },
    testM("disjointWith with an axiom annotation renders both the axiom and the annotation") {
      for {
        axioms <- render(disjointWithAnn, boundRow)
      } yield assert(logicalAxiomAnnotationProps(axioms))(contains(DCSource)) &&
        Harness.assertNoPlaceholderIRIs(axioms) &&
        Harness.assertNoPlaceholderLiterals(axioms)
    },
    testM("GCI with an axiom annotation renders both the axiom and the annotation") {
      for {
        axioms <- render(gciWithAnn, boundRow)
      } yield assert(logicalAxiomAnnotationProps(axioms))(contains(DCSource)) &&
        Harness.assertNoPlaceholderIRIs(axioms) &&
        Harness.assertNoPlaceholderLiterals(axioms)
    },

    // ----- Missing-binding negatives on annotation paths -----

    testM("OBO name annotation with missing class var emits no rdfs:label") {
      for {
        axioms <- render(oboNamePattern, unboundRow)
      } yield assert(annotationOn(axioms, OBORdfsLabel))(isEmpty) &&
        Harness.assertNoPlaceholderIRIs(axioms) &&
        Harness.assertNoPlaceholderLiterals(axioms)
    },
    testM("Custom annotation with missing class var emits no annotation assertion") {
      for {
        axioms <- render(customAnnPattern, unboundRow)
      } yield assert(annotationOn(axioms, DCSource))(isEmpty) &&
        Harness.assertNoPlaceholderIRIs(axioms) &&
        Harness.assertNoPlaceholderLiterals(axioms)
    },
    testM("Annotation with `permutations` and missing class var emits no permuted synonyms") {
      for {
        axioms <- render(permutationAnnPattern, unboundRow)
      } yield assert(annotationOn(axioms, OBOExactSynonym))(isEmpty) &&
        Harness.assertNoPlaceholderIRIs(axioms) &&
        Harness.assertNoPlaceholderLiterals(axioms)
    },
    testM("Annotation with `override`: missing both the override column and the template var drops the annotation") {
      // unboundRow has neither `source_override` nor `structure`; neither branch can produce a value.
      for {
        axioms <- render(overrideAnnPattern, unboundRow)
      } yield assert(annotationOn(axioms, DCSource))(isEmpty) &&
        Harness.assertNoPlaceholderIRIs(axioms) &&
        Harness.assertNoPlaceholderLiterals(axioms)
    },

    // ----- Missing-binding negatives on the three non-facet data_var slots -----

    testM("data_var missing in `value %s` slot drops the subClassOf axiom") {
      for {
        axioms <- render(dataValuePattern, Map("defined_class" -> "EX:0001"))
      } yield assert(axioms.exists(_.isInstanceOf[OWLSubClassOfAxiom]))(isFalse) &&
        Harness.assertNoPlaceholderIRIs(axioms) &&
        Harness.assertNoPlaceholderLiterals(axioms)
    },
    testM("data_var missing in `{%s}` DataOneOf slot drops the subClassOf axiom") {
      for {
        axioms <- render(dataOneOfPattern, Map("defined_class" -> "EX:0001"))
      } yield assert(axioms.exists(_.isInstanceOf[OWLSubClassOfAxiom]))(isFalse) &&
        Harness.assertNoPlaceholderIRIs(axioms) &&
        Harness.assertNoPlaceholderLiterals(axioms)
    },
    testM("data_var missing inside a cardinality facet filler drops the subClassOf axiom") {
      for {
        axioms <- render(dataCardinalityPattern, Map("defined_class" -> "EX:0001"))
      } yield assert(axioms.exists(_.isInstanceOf[OWLSubClassOfAxiom]))(isFalse) &&
        Harness.assertNoPlaceholderIRIs(axioms) &&
        Harness.assertNoPlaceholderLiterals(axioms)
    }

  ).provideCustomLayer(Logging.consoleErr())

}
