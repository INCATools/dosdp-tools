package org.monarchinitiative.dosdp

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{ClassExpressionType, DataRangeType, IRI, OWLClassExpression, OWLDataRange, OWLLiteral}
import org.semanticweb.owlapi.vocab.OWLFacet
import zio.test.Assertion._
import zio.test._

import scala.jdk.CollectionConverters._

/**
 * Pins coverage of `PatternCompiler.collectFromClassExpression` /
 * `collectFromDataRange` against the OWL API's `ClassExpressionType` and
 * `DataRangeType` enums. Phase 2b regressions originated in discovering
 * literal-bearing OWL constructs incrementally (DataHasValue, then
 * cardinalities, then DataOneOf, …); this audit closes that loop by
 * exercising every enum value with a sample expression containing a known
 * placeholder literal and asserting whether the walker recovers it.
 *
 * If the OWL API ever introduces a new `ClassExpressionType` or `DataRangeType`,
 * the first assertion fires ("test missing entry for X") — forcing a deliberate
 * decision about whether the new type carries literals and needs walker support.
 */
object LiteralWalkerAuditTest extends DefaultRunnableSpec {

  private val factory     = OWLManager.getOWLDataFactory
  private val placeholder = factory.getOWLLiteral("$placeholder")
  private val classA      = factory.getOWLClass(IRI.create("http://example.org/A"))
  private val individual  = factory.getOWLNamedIndividual(IRI.create("http://example.org/i"))
  private val objectProp  = factory.getOWLObjectProperty(IRI.create("http://example.org/op"))
  private val dataProp    = factory.getOWLDataProperty(IRI.create("http://example.org/dp"))
  private val xsdString   = factory.getOWLDatatype(IRI.create("http://www.w3.org/2001/XMLSchema#string"))
  private val placeholderRange: OWLDataRange = factory.getOWLDataOneOf(placeholder)

  private val classExpressionByType: Map[ClassExpressionType, OWLClassExpression] = Map(
    ClassExpressionType.OWL_CLASS                -> classA,
    ClassExpressionType.OBJECT_INTERSECTION_OF   -> factory.getOWLObjectIntersectionOf(classA, factory.getOWLThing),
    ClassExpressionType.OBJECT_UNION_OF          -> factory.getOWLObjectUnionOf(classA, factory.getOWLThing),
    ClassExpressionType.OBJECT_COMPLEMENT_OF     -> factory.getOWLObjectComplementOf(classA),
    ClassExpressionType.OBJECT_ONE_OF            -> factory.getOWLObjectOneOf(individual),
    ClassExpressionType.OBJECT_SOME_VALUES_FROM  -> factory.getOWLObjectSomeValuesFrom(objectProp, classA),
    ClassExpressionType.OBJECT_ALL_VALUES_FROM   -> factory.getOWLObjectAllValuesFrom(objectProp, classA),
    ClassExpressionType.OBJECT_HAS_VALUE         -> factory.getOWLObjectHasValue(objectProp, individual),
    ClassExpressionType.OBJECT_HAS_SELF          -> factory.getOWLObjectHasSelf(objectProp),
    ClassExpressionType.OBJECT_MIN_CARDINALITY   -> factory.getOWLObjectMinCardinality(1, objectProp, classA),
    ClassExpressionType.OBJECT_MAX_CARDINALITY   -> factory.getOWLObjectMaxCardinality(1, objectProp, classA),
    ClassExpressionType.OBJECT_EXACT_CARDINALITY -> factory.getOWLObjectExactCardinality(1, objectProp, classA),
    ClassExpressionType.DATA_SOME_VALUES_FROM    -> factory.getOWLDataSomeValuesFrom(dataProp, placeholderRange),
    ClassExpressionType.DATA_ALL_VALUES_FROM     -> factory.getOWLDataAllValuesFrom(dataProp, placeholderRange),
    ClassExpressionType.DATA_HAS_VALUE           -> factory.getOWLDataHasValue(dataProp, placeholder),
    ClassExpressionType.DATA_MIN_CARDINALITY     -> factory.getOWLDataMinCardinality(1, dataProp, placeholderRange),
    ClassExpressionType.DATA_MAX_CARDINALITY     -> factory.getOWLDataMaxCardinality(1, dataProp, placeholderRange),
    ClassExpressionType.DATA_EXACT_CARDINALITY   -> factory.getOWLDataExactCardinality(1, dataProp, placeholderRange)
  )

  private val literalBearingClassExpressionTypes: Set[ClassExpressionType] = Set(
    ClassExpressionType.DATA_SOME_VALUES_FROM,
    ClassExpressionType.DATA_ALL_VALUES_FROM,
    ClassExpressionType.DATA_HAS_VALUE,
    ClassExpressionType.DATA_MIN_CARDINALITY,
    ClassExpressionType.DATA_MAX_CARDINALITY,
    ClassExpressionType.DATA_EXACT_CARDINALITY
  )

  private val dataRangeByType: Map[DataRangeType, OWLDataRange] = Map(
    DataRangeType.DATATYPE              -> xsdString,
    DataRangeType.DATA_ONE_OF           -> factory.getOWLDataOneOf(placeholder),
    DataRangeType.DATATYPE_RESTRICTION  -> factory.getOWLDatatypeRestriction(xsdString, factory.getOWLFacetRestriction(OWLFacet.MIN_INCLUSIVE, placeholder)),
    DataRangeType.DATA_COMPLEMENT_OF    -> factory.getOWLDataComplementOf(factory.getOWLDataOneOf(placeholder)),
    DataRangeType.DATA_UNION_OF         -> factory.getOWLDataUnionOf(xsdString, factory.getOWLDataOneOf(placeholder)),
    DataRangeType.DATA_INTERSECTION_OF  -> factory.getOWLDataIntersectionOf(xsdString, factory.getOWLDataOneOf(placeholder))
  )

  private val literalBearingDataRangeTypes: Set[DataRangeType] = Set(
    DataRangeType.DATA_ONE_OF,
    DataRangeType.DATATYPE_RESTRICTION,
    DataRangeType.DATA_COMPLEMENT_OF,
    DataRangeType.DATA_UNION_OF,
    DataRangeType.DATA_INTERSECTION_OF
  )

  private def collectFromCE(ce: OWLClassExpression): Set[OWLLiteral] = {
    val builder = scala.collection.mutable.Set.empty[OWLLiteral]
    PatternCompiler.collectFromClassExpression(ce, builder)
    builder.toSet
  }

  private def collectFromDR(range: OWLDataRange): Set[OWLLiteral] = {
    val builder = scala.collection.mutable.Set.empty[OWLLiteral]
    PatternCompiler.collectFromDataRange(range, builder)
    builder.toSet
  }

  def spec = suite("Literal walker audit")(
    test("every ClassExpressionType in the OWL API enum has a fixture") {
      val missing = ClassExpressionType.values().toList.filterNot(classExpressionByType.contains)
      assert(missing)(isEmpty ?? "every ClassExpressionType has a fixture in classExpressionByType")
    },
    test("every DataRangeType in the OWL API enum has a fixture") {
      val missing = DataRangeType.values().toList.filterNot(dataRangeByType.contains)
      assert(missing)(isEmpty ?? "every DataRangeType has a fixture in dataRangeByType")
    },
    test("collectFromClassExpression recovers the placeholder literal from every literal-bearing ClassExpressionType") {
      val results = classExpressionByType.toList.map { case (t, ce) =>
        val found = collectFromCE(ce).contains(placeholder)
        val shouldFind = literalBearingClassExpressionTypes.contains(t)
        (t, shouldFind, found)
      }
      val wrong = results.filter { case (_, expected, actual) => expected != actual }
      assert(wrong)(isEmpty ?? s"per-type expectations match (mismatches: ${wrong.map(_._1).mkString(", ")})")
    },
    test("collectFromDataRange recovers the placeholder literal from every literal-bearing DataRangeType") {
      val results = dataRangeByType.toList.map { case (t, range) =>
        val found = collectFromDR(range).contains(placeholder)
        val shouldFind = literalBearingDataRangeTypes.contains(t)
        (t, shouldFind, found)
      }
      val wrong = results.filter { case (_, expected, actual) => expected != actual }
      assert(wrong)(isEmpty ?? s"per-type expectations match (mismatches: ${wrong.map(_._1).mkString(", ")})")
    },
    test("collectLiterals walks an OWLClassExpression through getNestedClassExpressions to its data restrictions") {
      val nested = factory.getOWLObjectIntersectionOf(
        classA,
        factory.getOWLObjectSomeValuesFrom(objectProp,
          factory.getOWLDataHasValue(dataProp, placeholder)))
      assert(PatternCompiler.collectLiterals(nested))(contains(placeholder))
    },
    test("collectLiterals walks an OWLAxiom through getNestedClassExpressions to its data restrictions") {
      val axiom = factory.getOWLSubClassOfAxiom(classA, factory.getOWLDataHasValue(dataProp, placeholder))
      assert(PatternCompiler.collectLiterals(axiom))(contains(placeholder))
    },
    test("collectLiterals picks up literal-valued top-level axiom annotations") {
      val annLit = factory.getOWLLiteral("$annvalue")
      val ann    = factory.getOWLAnnotation(factory.getRDFSLabel, annLit)
      val axiom  = factory.getOWLSubClassOfAxiom(classA, factory.getOWLThing, java.util.Collections.singleton(ann))
      assert(PatternCompiler.collectLiterals(axiom))(contains(annLit))
    }
  )

}
