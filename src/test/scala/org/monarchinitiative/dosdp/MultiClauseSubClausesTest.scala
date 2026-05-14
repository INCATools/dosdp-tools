package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.Generate
import org.phenoscape.scowl._
import org.semanticweb.owlapi.model._
import zio.logging._
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
object MultiClauseSubClausesTest extends DefaultRunnableSpec {

  private val term: OWLClass = Class("http://purl.obolibrary.org/obo/EX_0001")
  private val anatomy: OWLClass = Class("http://purl.obolibrary.org/obo/UBERON_0000001")
  private val region: OWLClass = Class("http://purl.obolibrary.org/obo/UBERON_0000002")
  private val partOf: OWLObjectProperty = ObjectProperty("http://purl.obolibrary.org/obo/BFO_0000050")
  private val develops: OWLObjectProperty = ObjectProperty("http://purl.obolibrary.org/obo/RO_0002202")

  private def hasNamedClass(axiom: OWLSubClassOfAxiom, cls: OWLClass): Boolean =
    axiom.getSuperClass.getClassesInSignature.asScala.contains(cls)

  def spec = suite("multi_clause with sub_clauses on logical templates") (
    testM("subClassOf multi_clause with one clause carrying a sub_clause compiles and renders") {
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
    }
  ).provideCustomLayer(Logging.consoleErr())

}
