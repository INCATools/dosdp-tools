package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.Generate
import org.monarchinitiative.dosdp.{AxiomType => DOSDPAxiomType}
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import zio.logging._
import zio.test.Assertion._
import zio.test._

import scala.jdk.CollectionConverters._

object DataPropertyAxiomTest extends DefaultRunnableSpec {

  private val factory = OWLManager.getOWLDataFactory

  private val term: OWLClass = Class("http://purl.obolibrary.org/obo/ONT_0000001")
  private val population: OWLClass = Class("http://purl.obolibrary.org/obo/ONT_0000002")
  private val hasPart: OWLObjectProperty = ObjectProperty("http://purl.obolibrary.org/obo/BFO_0000051")
  private val characteristicOf: OWLObjectProperty = ObjectProperty("http://purl.obolibrary.org/obo/RO_0000052")
  private val mortalityRate: OWLDataProperty = factory.getOWLDataProperty(IRI.create("http://purl.obolibrary.org/obo/RO_0002029"))

  // Regression for https://github.com/INCATools/dosdp-tools/issues/504
  // A data_var referenced in a logical_axioms text was not substituted because
  // dataVarBindings were missing from logicalBindings in Generate.renderPattern.
  private val dosdp: DOSDP = DOSDP.empty.copy(
    pattern_name = Some("data_property_axiom_test"),
    classes = Some(Map("population_of_drosophila" -> "ONT:0000002")),
    relations = Some(Map(
      "has_part" -> "BFO:0000051",
      "characteristic_of" -> "RO:0000052")),
    dataProperties = Some(Map("age_specific_mortality_rate" -> "RO:0002029")),
    data_vars = Some(Map("rate_min" -> "xsd:short")),
    logical_axioms = Some(List(PrintfOWL(
      annotations = None,
      axiom_type = DOSDPAxiomType.EquivalentTo,
      text = Some("'has_part' some (('characteristic_of' some 'population_of_drosophila') and ('age_specific_mortality_rate' some xsd:short[>= %s]))"),
      vars = Some(List("rate_min")))))
  )

  private val xsdShort = factory.getOWLDatatype(IRI.create("http://www.w3.org/2001/XMLSchema#short"))

  private def mortalityDataRestriction(axiom: OWLAxiom): Option[OWLDatatypeRestriction] = axiom match {
    case eq: OWLEquivalentClassesAxiom if eq.getClassesInSignature.asScala.contains(term) =>
      eq.getNestedClassExpressions.asScala.collectFirst {
        case dsv: OWLDataSomeValuesFrom if dsv.getProperty == mortalityRate =>
          dsv.getFiller
      }.collect { case r: OWLDatatypeRestriction => r }
    case _ => None
  }

  def spec = suite("Data property restrictions in logical_axioms (issue #504)") {
    testM("data_var fillers should be substituted and produce a well-typed data restriction") {
      val row = Map("defined_class" -> "ONT:0000001", "rate_min" -> "5")
      for {
        axioms <- Generate.renderPattern(dosdp, OBOPrefixes, List(row), None, outputLogicalAxioms = true, outputAnnotationAxioms = false, None, annotateAxiomSource = false, AxiomRestrictionsTest.OboInOwlSource, generateDefinedClass = false, Map.empty)
        restriction = axioms.flatMap(mortalityDataRestriction).headOption
        facetValues = restriction.toList.flatMap(_.getFacetRestrictions.asScala.map(_.getFacetValue))
      } yield assert(axioms.flatMap(_.getDataPropertiesInSignature.asScala))(contains(mortalityRate)) &&
        assert(axioms.flatMap(_.getObjectPropertiesInSignature.asScala))(contains(hasPart)) &&
        assert(axioms.flatMap(_.getObjectPropertiesInSignature.asScala))(contains(characteristicOf)) &&
        assert(restriction.map(_.getDatatype))(isSome(equalTo(xsdShort))) &&
        // Issue #504: lexical form must be "5", not "'5'" (the latter is ill-typed for xsd:short)
        assert(facetValues.map(_.getLiteral))(equalTo(List("5"))) &&
        assert(facetValues.map(_.getDatatype))(equalTo(List(xsdShort)))
    }
  }.provideCustomLayer(Logging.consoleErr())

}
