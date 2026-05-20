package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.Generate
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import zio.test.Assertion._
import zio.test._

import scala.jdk.CollectionConverters._

/**
 * A `data_var` may appear in any Manchester slot that expects a literal — not
 * only in a datatype-facet position like `xsd:short[>= %s]`. Compile-time
 * parsing must accept those templates and the row-time substitution must
 * produce a literal of the declared datatype.
 */
object DataVarSlotTest extends ZIOSpecDefault {

  private val factory = OWLManager.getOWLDataFactory
  private val term: OWLClass = Class("http://purl.obolibrary.org/obo/EX_0001")
  private val hasAge: OWLDataProperty = factory.getOWLDataProperty(IRI.create("http://purl.obolibrary.org/obo/RO_0002000"))
  private val xsdInt: OWLDatatype = factory.getOWLDatatype(IRI.create("http://www.w3.org/2001/XMLSchema#integer"))

  private val dataPropertyValuePattern: DOSDP = DOSDP.empty.copy(
    pattern_name = Some("data_var_value_slot"),
    classes = Some(Map("thing" -> "owl:Thing")),
    dataProperties = Some(Map("has_age" -> "RO:0002000")),
    data_vars = Some(Map("age" -> "xsd:integer")),
    subClassOf = Some(PrintfOWLConvenience(None,
      Some("'has_age' value %s"),
      Some(List("age"))))
  )

  private val dataPropertyFacetPattern: DOSDP = DOSDP.empty.copy(
    pattern_name = Some("data_var_facet"),
    classes = Some(Map("thing" -> "owl:Thing")),
    dataProperties = Some(Map("has_age" -> "RO:0002000")),
    data_vars = Some(Map("age" -> "xsd:integer")),
    subClassOf = Some(PrintfOWLConvenience(None,
      Some("'has_age' some xsd:integer[>= %s]"),
      Some(List("age"))))
  )

  private def hasDataValueLiteral(axioms: Set[OWLAxiom], expectedLexical: String): Boolean =
    axioms.exists {
      case sc: OWLSubClassOfAxiom =>
        sc.getSuperClass match {
          case dhv: OWLDataHasValue =>
            dhv.getProperty == hasAge &&
              dhv.getFiller.getLiteral == expectedLexical &&
              dhv.getFiller.getDatatype == xsdInt
          case _ => false
        }
      case _ => false
    }

  def spec = suite("data_var in non-facet Manchester slots") (
    test("'value <data_var>' compiles and renders to a typed literal") {
      val row = Map("defined_class" -> "EX:0001", "age" -> "42")
      for {
        axioms <- Generate.renderPattern(dataPropertyValuePattern, OBOPrefixes, List(row), None, true, false, None, false, AxiomRestrictionsTest.OboInOwlSource, false, Map.empty)
      } yield assert(hasDataValueLiteral(axioms, "42"))(isTrue)
    },
    test("data_var inside a cardinality filler is substituted at row time") {
      // The facet literal lives inside an OWLDataMinCardinality; the walker must
      // descend into the cardinality's filler to identify the placeholder.
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("data_var_in_cardinality_filler"),
        classes = Some(Map("thing" -> "owl:Thing")),
        dataProperties = Some(Map("has_age" -> "RO:0002000")),
        data_vars = Some(Map("age" -> "xsd:integer")),
        subClassOf = Some(PrintfOWLConvenience(None,
          Some("'has_age' min 1 xsd:integer[>= %s]"),
          Some(List("age"))))
      )
      val row = Map("defined_class" -> "EX:0001", "age" -> "5")
      for {
        axioms <- Generate.renderPattern(pattern, OBOPrefixes, List(row), None, true, false, None, false, AxiomRestrictionsTest.OboInOwlSource, false, Map.empty)
        cardinalityFacetValues = axioms.flatMap(_.getNestedClassExpressions.asScala.collect {
          case dmc: OWLDataMinCardinality => dmc.getFiller
        }.collect {
          case dtr: OWLDatatypeRestriction => dtr.getFacetRestrictions.asScala.map(_.getFacetValue.getLiteral)
        }.flatten.toSet)
      } yield assert(cardinalityFacetValues)(contains("5"))
    },
    test("data_var inside a DataOneOf is substituted at row time") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("data_var_in_oneof"),
        classes = Some(Map("thing" -> "owl:Thing")),
        dataProperties = Some(Map("has_age" -> "RO:0002000")),
        data_vars = Some(Map("age" -> "xsd:integer")),
        subClassOf = Some(PrintfOWLConvenience(None,
          Some("'has_age' some {%s}"),
          Some(List("age"))))
      )
      val row = Map("defined_class" -> "EX:0001", "age" -> "5")
      for {
        axioms <- Generate.renderPattern(pattern, OBOPrefixes, List(row), None, true, false, None, false, AxiomRestrictionsTest.OboInOwlSource, false, Map.empty)
        oneOfLiterals = axioms.flatMap(_.getNestedClassExpressions.asScala.collect {
          case dsv: OWLDataSomeValuesFrom => dsv.getFiller
        }.collect {
          case doo: OWLDataOneOf => doo.getValues.asScala.map(_.getLiteral)
        }.flatten.toSet)
      } yield assert(oneOfLiterals)(contains("5"))
    },
    test("'xsd:integer[>= <data_var>]' facet (regression for issue #504) still works") {
      val row = Map("defined_class" -> "EX:0001", "age" -> "5")
      for {
        axioms <- Generate.renderPattern(dataPropertyFacetPattern, OBOPrefixes, List(row), None, true, false, None, false, AxiomRestrictionsTest.OboInOwlSource, false, Map.empty)
        facetValues = axioms.flatMap(_.getNestedClassExpressions.asScala.collect {
          case dsv: OWLDataSomeValuesFrom => dsv.getFiller
        }.collect {
          case dtr: OWLDatatypeRestriction => dtr.getFacetRestrictions.asScala.map(_.getFacetValue.getLiteral)
        }.flatten.toSet)
      } yield assert(facetValues)(contains("5"))
    }
  )

}
