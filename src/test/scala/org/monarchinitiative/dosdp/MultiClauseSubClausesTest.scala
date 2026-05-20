package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.{Config, Generate}
import org.phenoscape.scowl._
import org.semanticweb.owlapi.model._
import zio.test.Assertion._
import zio.test._

import scala.jdk.CollectionConverters._

/**
 * `multi_clause` entries on logical templates (`subClassOf`, `equivalentTo`,
 * `disjointWith`) historically supported `sub_clauses` — the legacy row-time
 * renderer expanded them via `PrintfText.replaceMultiClause` and the
 * Manchester parser saw one assembled expression. Patterns relying on this
 * structure must still compile.
 */
object MultiClauseSubClausesTest extends ZIOSpecDefault {

  private val term: OWLClass = Class("http://purl.obolibrary.org/obo/EX_0001")
  private val anatomy: OWLClass = Class("http://purl.obolibrary.org/obo/UBERON_0000001")
  private val region: OWLClass = Class("http://purl.obolibrary.org/obo/UBERON_0000002")
  private val partOf: OWLObjectProperty = ObjectProperty("http://purl.obolibrary.org/obo/BFO_0000050")
  private val develops: OWLObjectProperty = ObjectProperty("http://purl.obolibrary.org/obo/RO_0002202")

  private def hasNamedClass(axiom: OWLSubClassOfAxiom, cls: OWLClass): Boolean =
    axiom.getSuperClass.getClassesInSignature.asScala.contains(cls)

  def spec = suite("multi_clause with sub_clauses on logical templates") (
    test("subClassOf multi_clause with one clause carrying a sub_clause compiles and renders") {
      val sub = PrintfClause("'develops_from' some %s", Some(List("origin")), None)
      val topClause = PrintfClause(
        "'part_of' some %s",
        Some(List("structure")),
        Some(List(MultiClausePrintf(Some(" and "), Some(List(sub))))))
      val multi = MultiClausePrintf(Some(" and "), Some(List(topClause)))
      val dosdp: DOSDP = DOSDP.empty.copy(
        pattern_name = Some("multi_clause_sub"),
        classes = Some(Map("thing" -> "owl:Thing")),
        relations = Some(Map(
          "part_of" -> "BFO:0000050",
          "develops_from" -> "RO:0002202")),
        vars = Some(Map("structure" -> "'thing'", "origin" -> "'thing'")),
        subClassOf = Some(PrintfOWLConvenience(None, None, None, Some(multi)))
      )
      val row = Map(
        "defined_class" -> "EX:0001",
        "structure" -> "UBERON:0000001",
        "origin" -> "UBERON:0000002")
      for {
        axioms <- Generate.renderPattern(dosdp, OBOPrefixes, List(row), None, true, false, None, false, AxiomRestrictionsTest.OboInOwlSource, false, Map.empty)
        subAxioms = axioms.collect { case sc: OWLSubClassOfAxiom => sc }
      } yield assert(subAxioms.exists(_.getSubClass == term))(isTrue) &&
        assert(subAxioms.exists(ax => hasNamedClass(ax, anatomy) && hasNamedClass(ax, region)))(isTrue) &&
        assert(axioms.flatMap(_.getObjectPropertiesInSignature.asScala))(contains(partOf)) &&
        assert(axioms.flatMap(_.getObjectPropertiesInSignature.asScala))(contains(develops))
    },
    test("missing nested sub_clause binding keeps the parent clause (drop only the sub-clause)") {
      // Parent clause `'part_of' some %s` is bound; nested sub_clause `'develops_from' some %s`
      // is unbound. Legacy `PrintfText.replaceMultiClause` kept the parent and dropped only the
      // sub-clause.
      val sub = PrintfClause("'develops_from' some %s", Some(List("origin")), None)
      val topClause = PrintfClause(
        "'part_of' some %s",
        Some(List("structure")),
        Some(List(MultiClausePrintf(Some(" and "), Some(List(sub))))))
      val multi = MultiClausePrintf(Some(" and "), Some(List(topClause)))
      val dosdp: DOSDP = DOSDP.empty.copy(
        pattern_name = Some("multi_clause_sub_partial"),
        classes = Some(Map("thing" -> "owl:Thing")),
        relations = Some(Map(
          "part_of" -> "BFO:0000050",
          "develops_from" -> "RO:0002202")),
        vars = Some(Map("structure" -> "'thing'", "origin" -> "'thing'")),
        subClassOf = Some(PrintfOWLConvenience(None, None, None, Some(multi)))
      )
      val row = Map(
        "defined_class" -> "EX:0001",
        "structure" -> "UBERON:0000001") // origin missing
      for {
        axioms <- Generate.renderPattern(dosdp, OBOPrefixes, List(row), None, true, false, None, false, AxiomRestrictionsTest.OboInOwlSource, false, Map.empty)
        subAxioms = axioms.collect { case sc: OWLSubClassOfAxiom => sc }
      } yield assert(subAxioms.exists(_.getSubClass == term))(isTrue) &&
        assert(subAxioms.exists(ax => hasNamedClass(ax, anatomy)))(isTrue) &&
        Harness.assertNoPlaceholderIRIs(axioms) &&
        assert(axioms.flatMap(_.getObjectPropertiesInSignature.asScala))(contains(partOf)) &&
        assert(axioms.flatMap(_.getObjectPropertiesInSignature.asScala).contains(develops))(isFalse)
    },
    test("missing parent clause binding drops the entire clause-group even if sub_clauses are bound") {
      val sub = PrintfClause("'develops_from' some %s", Some(List("origin")), None)
      val topClause = PrintfClause(
        "'part_of' some %s",
        Some(List("structure")),
        Some(List(MultiClausePrintf(Some(" and "), Some(List(sub))))))
      val multi = MultiClausePrintf(Some(" and "), Some(List(topClause)))
      val dosdp: DOSDP = DOSDP.empty.copy(
        pattern_name = Some("multi_clause_parent_miss"),
        classes = Some(Map("thing" -> "owl:Thing")),
        relations = Some(Map(
          "part_of" -> "BFO:0000050",
          "develops_from" -> "RO:0002202")),
        vars = Some(Map("structure" -> "'thing'", "origin" -> "'thing'")),
        subClassOf = Some(PrintfOWLConvenience(None, None, None, Some(multi)))
      )
      val row = Map(
        "defined_class" -> "EX:0001",
        "origin" -> "UBERON:0000002") // structure missing
      for {
        axioms <- Generate.renderPattern(dosdp, OBOPrefixes, List(row), None, true, false, None, false, AxiomRestrictionsTest.OboInOwlSource, false, Map.empty)
      } yield assert(axioms.exists(_.isInstanceOf[OWLSubClassOfAxiom]))(isFalse) &&
        Harness.assertNoPlaceholderIRIs(axioms)
    },
    test("a single-clause nested sub_clause may omit its separator") {
      // The nested sub_clause has exactly one clause and no `sep`; the operator
      // is never used to join anything, so compilation must not demand a separator.
      val sub = PrintfClause("'develops_from' some %s", Some(List("origin")), None)
      val topClause = PrintfClause(
        "'part_of' some %s",
        Some(List("structure")),
        Some(List(MultiClausePrintf(None, Some(List(sub))))))
      val multi = MultiClausePrintf(Some(" and "), Some(List(topClause)))
      val dosdp: DOSDP = DOSDP.empty.copy(
        pattern_name = Some("multi_clause_single_sub_no_sep"),
        classes = Some(Map("thing" -> "owl:Thing")),
        relations = Some(Map(
          "part_of" -> "BFO:0000050",
          "develops_from" -> "RO:0002202")),
        vars = Some(Map("structure" -> "'thing'", "origin" -> "'thing'")),
        subClassOf = Some(PrintfOWLConvenience(None, None, None, Some(multi)))
      )
      val row = Map(
        "defined_class" -> "EX:0001",
        "structure" -> "UBERON:0000001",
        "origin" -> "UBERON:0000002")
      for {
        axioms <- Generate.renderPattern(dosdp, OBOPrefixes, List(row), None, true, false, None, false, AxiomRestrictionsTest.OboInOwlSource, false, Map.empty)
        subAxioms = axioms.collect { case sc: OWLSubClassOfAxiom => sc }
      } yield assert(subAxioms.exists(ax => hasNamedClass(ax, anatomy) && hasNamedClass(ax, region)))(isTrue)
    },
    test("nested operator differs from parent: row-time and placeholder forms agree") {
      // Parent `and`, nested sub_clause `or`. The output must nest a union inside
      // the intersection — not flatten — for both the row-time axioms and the
      // placeholder-form axioms that Query/Terms consume.
      val subOr = MultiClausePrintf(Some(" or "), Some(List(
        PrintfClause("'develops_from' some %s", Some(List("origin_a")), None),
        PrintfClause("'develops_from' some %s", Some(List("origin_b")), None))))
      val topClause = PrintfClause(
        "'part_of' some %s",
        Some(List("structure")),
        Some(List(subOr)))
      val multi = MultiClausePrintf(Some(" and "), Some(List(topClause)))
      val dosdp: DOSDP = DOSDP.empty.copy(
        pattern_name = Some("multi_clause_mixed_operators"),
        classes = Some(Map("thing" -> "owl:Thing")),
        relations = Some(Map(
          "part_of" -> "BFO:0000050",
          "develops_from" -> "RO:0002202")),
        vars = Some(Map("structure" -> "'thing'", "origin_a" -> "'thing'", "origin_b" -> "'thing'")),
        subClassOf = Some(PrintfOWLConvenience(None, None, None, Some(multi)))
      )
      val row = Map(
        "defined_class" -> "EX:0001",
        "structure" -> "UBERON:0000001",
        "origin_a" -> "UBERON:0000002",
        "origin_b" -> "UBERON:0000003")
      // The superclass of a SubClassOf axiom must be an intersection that
      // *contains* a union as one of its operands (nested, not flattened).
      def superHasNestedUnion(ax: OWLSubClassOfAxiom): Boolean =
        ax.getSuperClass match {
          case inter: OWLObjectIntersectionOf =>
            inter.getOperands.asScala.exists(_.isInstanceOf[OWLObjectUnionOf])
          case _ => false
        }
      for {
        prefixes <- zio.ZIO.succeed(OBOPrefixes)
        compiled <- PatternCompiler.compile(dosdp, prefixes)
        rowAxioms <- Generate.renderPattern(dosdp, prefixes, List(row), None, true, false, None, false, AxiomRestrictionsTest.OboInOwlSource, false, Map.empty)
        rowSub = rowAxioms.collect { case sc: OWLSubClassOfAxiom => sc }
        placeholderSub = Expansion.placeholderAxioms(compiled, Config.LogicalAxioms).collect { case sc: OWLSubClassOfAxiom => sc }
      } yield assert(rowSub.exists(superHasNestedUnion))(isTrue) &&
        assert(placeholderSub.exists(superHasNestedUnion))(isTrue)
    }
  )

}
