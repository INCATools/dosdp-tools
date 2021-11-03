package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.Generate
import org.phenoscape.scowl.{not => _, _}
import org.semanticweb.owlapi.model._
import zio._
import zio.test.Assertion._
import zio.test._

object MultiClausePrintfTest extends DefaultRunnableSpec {

  def spec = suite("Print simple text")(
    test("Text should be replaced correctly") {
      val replacedText = PrintfText.replaced(Some("These cells also express %s."), Some(List("allen_makers_cat")), None, Some(Map("allen_makers_cat" -> SingleValue("my_value"))), quote = true).getOrElse("")
      assert(replacedText)(equalTo("These cells also express 'my_value'."))
    },
    test("Multi_clause with single clause should replaced correctly") {
      val simple_clause = PrintfClause("These cells also express %s.", Some(List("allen_makers_cat")), None)
      val simple_printf = MultiClausePrintf(Some(" "), Some(List(simple_clause)))
      val replacedText = PrintfText.replaced(None, None, Some(simple_printf), Some(Map("allen_makers_cat" -> SingleValue("my_value"))), quote = true).getOrElse("")
      assert(replacedText)(equalTo("These cells also express 'my_value'."))
    },
    test("Multi_clause with single clause (without variable) should replaced correctly") {
      val simple_clause = PrintfClause("Just plain text without variable.", None, None)
      val simple_printf = MultiClausePrintf(Some(" "), Some(List(simple_clause)))
      val replacedText = PrintfText.replaced(None, None, Some(simple_printf), None, quote = true).getOrElse("")
      assert(replacedText)(equalTo("Just plain text without variable."))
    },
    test("Multi_clause with two clauses should replaced correctly") {
      val simple_clause1 = PrintfClause("These cells also express %s.", Some(List("allen_makers_cat")), None)
      val simple_clause2 = PrintfClause("These cells have projection type %s.", Some(List("projection_type")), None)
      val simple_printf = MultiClausePrintf(Some("_"), Some(List(simple_clause1, simple_clause2)))
      val replacedText = PrintfText.replaced(None, None, Some(simple_printf), Some(Map("allen_makers_cat" -> SingleValue("my_value"), "projection_type" -> SingleValue("my_value2"))), quote = true).getOrElse("")
      assert(replacedText)(equalTo("These cells also express 'my_value'._These cells have projection type 'my_value2'."))
    },
    test("Multi_clause with three clauses, only two should replaced correctly") {
      val simple_clause1 = PrintfClause("These cells also express %s.", Some(List("allen_makers_cat")), None)
      val simple_clause2 = PrintfClause("These cells have projection type %s.", Some(List("not_existing_var")), None)
      val simple_clause3 = PrintfClause("These cells have some location %s.", Some(List("soma_location")), None)
      val simple_printf = MultiClausePrintf(Some("_"), Some(List(simple_clause1, simple_clause2, simple_clause3)))
      val replacedText = PrintfText.replaced(None, None, Some(simple_printf), Some(Map("allen_makers_cat" -> SingleValue("my_value"), "soma_location" -> SingleValue("my_value3"))), quote = true).getOrElse("")
      assert(replacedText)(equalTo("These cells also express 'my_value'._These cells have some location 'my_value3'."))
    },
    test("Multi_clause with sub clauses should replaced together") {
      val sub_clause1 = PrintfClause("Sub text %s end.", Some(List("sub_var1")), None)
      val sub_clause2 = PrintfClause("Sub text2 %s end2.", Some(List("sub_var2")), None)
      val sub_clauses = MultiClausePrintf(Some(" "), Some(List(sub_clause1, sub_clause2)))

      val clause_with_sub = PrintfClause("These cells also express %s.", Some(List("allen_makers_cat")), Some(List(sub_clauses)))

      val top_printf = MultiClausePrintf(Some("_"), Some(List(clause_with_sub)))
      val replacedText = PrintfText.replaced(None, None, Some(top_printf), Some(Map(
        "allen_makers_cat" -> SingleValue("my_value"),
        "sub_var1" -> SingleValue("sub_value1"),
        "sub_var2" -> SingleValue("sub_value2"))), quote = true).getOrElse("")

      assert(replacedText)(equalTo("These cells also express 'my_value'._Sub text 'sub_value1' end. Sub text2 'sub_value2' end2."))
    },
    test("Sub clauses ignored when parent fail") {
      val sub_clause1 = PrintfClause("Sub text %s end.", Some(List("sub_var1")), None)
      val sub_clause2 = PrintfClause("Sub text2 %s end2.", Some(List("sub_var2")), None)
      val sub_clauses = MultiClausePrintf(Some(" "), Some(List(sub_clause1, sub_clause2)))

      val clause_with_sub = PrintfClause("Top clause but fail %s.", Some(List("not existing")), Some(List(sub_clauses)))

      val top_printf = MultiClausePrintf(Some("_"), Some(List(clause_with_sub)))
      val replacedText = PrintfText.replaced(None, None, Some(top_printf), Some(Map(
        "sub_var1" -> SingleValue("sub_value1"),
        "sub_var2" -> SingleValue("sub_value2"))), quote = true).getOrElse("")

      assert(replacedText)(equalTo(""))
    },
    test("Some sub clauses may fail without issue") {
      val sub_clause1 = PrintfClause("Sub text %s end.", Some(List("sub_var1")), None)
      val sub_clause2 = PrintfClause("Sub text2 %s end2.", Some(List("NotExist")), None)
      val sub_clause3 = PrintfClause("Sub text3 %s end3.", Some(List("sub_var3")), None)
      val sub_clauses = MultiClausePrintf(Some(" "), Some(List(sub_clause1, sub_clause2, sub_clause3)))

      val clause_with_sub = PrintfClause("Top clause works fine %s.", Some(List("var1")), Some(List(sub_clauses)))

      val top_printf = MultiClausePrintf(Some("_"), Some(List(clause_with_sub)))
      val replacedText = PrintfText.replaced(None, None, Some(top_printf), Some(Map(
        "var1" -> SingleValue("value1"),
        "sub_var1" -> SingleValue("sub_value1"),
        "sub_var3" -> SingleValue("sub_value3"))), quote = false).getOrElse("")

      assert(replacedText)(equalTo("Top clause works fine value1._Sub text sub_value1 end. Sub text3 sub_value3 end3."))
    },
    test("Sub clauses ignored when parent fail, but siblings are ok") {
      val sub_clause1 = PrintfClause("Sub text %s end.", Some(List("sub_var1")), None)
      val sub_clause2 = PrintfClause("Sub text2 %s end2.", Some(List("sub_var2")), None)
      val sub_clauses = MultiClausePrintf(Some(" "), Some(List(sub_clause1, sub_clause2)))

      val clause_with_sub = PrintfClause("Second me but fail %s.", Some(List("not existing")), Some(List(sub_clauses)))
      val sibling_clause1 = PrintfClause("First me %s.", Some(List("sibling_var1")), None)
      val sibling_clause2 = PrintfClause("Third me %s.", Some(List("sibling_var3")), None)

      val top_printf = MultiClausePrintf(Some("_"), Some(List(sibling_clause1, clause_with_sub, sibling_clause2)))
      val replacedText = PrintfText.replaced(None, None, Some(top_printf), Some(Map(
        "sibling_var1" -> SingleValue("sibling_value1"),
        "sibling_var3" -> SingleValue("sibling_value3"),
        "sub_var1" -> SingleValue("sub_value1"),
        "sub_var2" -> SingleValue("sub_value2"))), quote = true).getOrElse("")

      assert(replacedText)(equalTo("First me 'sibling_value1'._Third me 'sibling_value3'."))
    },
    test("Depth2 clause nesting should be replaced successfully.") {
      val sub_sub_clause1 = PrintfClause("Sub sub text1 %s end.", Some(List("sub_sub_var1")), None)
      val sub_sub_clauses = MultiClausePrintf(Some("$"), Some(List(sub_sub_clause1)))

      val sub_sub_clause2 = PrintfClause("Sub sub text2 end.", None, None)
      val sub_sub_clauses2 = MultiClausePrintf(Some("#"), Some(List(sub_sub_clause2)))

      val sub_clause1 = PrintfClause("Sub text1 %s end.", Some(List("sub_var1")), None)
      val sub_clause2 = PrintfClause("Sub text2 %s end.", Some(List("sub_var2")), None)
      val sub_clause3 = PrintfClause("Sub text3 %s end.", Some(List("sub_var3")), Some(List(sub_sub_clauses, sub_sub_clauses2)))
      val sub_clauses = MultiClausePrintf(Some(" "), Some(List(sub_clause1, sub_clause2, sub_clause3)))

      val sibling_clause1 = PrintfClause("First me %s.", Some(List("sibling_var1")), None)
      val clause_with_sub = PrintfClause("Second me %s.", Some(List("sibling_var2")), Some(List(sub_clauses)))
      val sibling_clause2 = PrintfClause("Third me %s.", Some(List("sibling_var3")), None)
      val top_printf = MultiClausePrintf(Some("_"), Some(List(sibling_clause1, clause_with_sub, sibling_clause2)))

      val bindings = Some(Map(
        "sibling_var1" -> SingleValue("sibling_value1"),
        "sibling_var2" -> SingleValue("sibling_value2"),
        "sibling_var3" -> SingleValue("sibling_value3"),
        "sub_var1" -> SingleValue("sub_value1"),
        "sub_var2" -> SingleValue("sub_value2"),
        "sub_var3" -> SingleValue("sub_value3"),
        "sub_sub_var1" -> SingleValue("sub_sub_value1")))

      val replacedText = PrintfText.replaced(None, None, Some(top_printf), bindings, quote = false).getOrElse("")

      assert(replacedText)(equalTo("First me sibling_value1._Second me sibling_value2._Sub text1 sub_value1 end. Sub text2 sub_value2 end. Sub text3 sub_value3 end. Sub sub text1 sub_sub_value1 end. Sub sub text2 end._Third me sibling_value3."))
    },
    test("Multi_clause with multiValue (list_var) should replaced correctly") {
      val simple_clause = PrintfClause("These cells also express %s.", Some(List("allen_makers_cat")), None)
      val simple_printf = MultiClausePrintf(Some(" "), Some(List(simple_clause)))
      val replacedText = PrintfText.replaced(None, None, Some(simple_printf), Some(Map("allen_makers_cat" -> MultiValue(Set("my_value1", "my_value2")))), quote = true).getOrElse("")
      assert(replacedText)(equalTo("These cells also express 'my_value1'. These cells also express 'my_value2'."))
    },
    test("Multi_clause with two clauses (one multiValue) should replaced correctly") {
      val simple_clause1 = PrintfClause("These cells also express %s.", Some(List("allen_makers_cat")), None)
      val simple_clause2 = PrintfClause("These cells have projection type %s.", Some(List("projection_type")), None)
      val simple_printf = MultiClausePrintf(Some("_"), Some(List(simple_clause1, simple_clause2)))
      val replacedText = PrintfText.replaced(None, None, Some(simple_printf), Some(Map("allen_makers_cat" -> MultiValue(Set("my_valueX", "my_valueY")), "projection_type" -> SingleValue("my_value2"))), quote = true).getOrElse("")
      assert(replacedText)(equalTo("These cells also express 'my_valueX'._These cells also express 'my_valueY'._These cells have projection type 'my_value2'."))
    },
    test("Depth2 clause nesting with a multi_value should be replaced successfully.") {
      val sub_sub_clause1 = PrintfClause("Sub sub text1 %s end.", Some(List("sub_sub_var1")), None)
      val sub_sub_clauses = MultiClausePrintf(Some("$"), Some(List(sub_sub_clause1)))

      val sub_sub_clause2 = PrintfClause("Sub sub text2 end.", None, None)
      val sub_sub_clauses2 = MultiClausePrintf(Some("#"), Some(List(sub_sub_clause2)))

      val sub_clause1 = PrintfClause("Sub text1 %s end.", Some(List("sub_var1")), None)
      val sub_clause2 = PrintfClause("Sub text2 %s end.", Some(List("sub_var2")), None)
      val sub_clause3 = PrintfClause("Sub text3 %s end.", Some(List("sub_var3")), Some(List(sub_sub_clauses, sub_sub_clauses2)))
      val sub_clauses = MultiClausePrintf(Some(" "), Some(List(sub_clause1, sub_clause2, sub_clause3)))

      val sibling_clause1 = PrintfClause("First me %s.", Some(List("sibling_var1")), None)
      val clause_with_sub = PrintfClause("Second me %s.", Some(List("sibling_var2")), Some(List(sub_clauses)))
      val sibling_clause2 = PrintfClause("Third me %s.", Some(List("sibling_var3")), None)
      val top_printf = MultiClausePrintf(Some("_"), Some(List(sibling_clause1, clause_with_sub, sibling_clause2)))

      val bindings = Some(Map(
        "sibling_var1" -> SingleValue("sibling_value1"),
        "sibling_var2" -> SingleValue("sibling_value2"),
        "sibling_var3" -> SingleValue("sibling_value3"),
        "sub_var1" -> SingleValue("sub_value1"),
        // and one multiValue
        "sub_var2" -> MultiValue(Set("sub_value2_1", "sub_value2_2", "sub_value2_3")),
        "sub_var3" -> SingleValue("sub_value3"),
        "sub_sub_var1" -> SingleValue("sub_sub_value1")))

      val replacedText = PrintfText.replaced(None, None, Some(top_printf), bindings, quote = false).getOrElse("")

      assert(replacedText)(equalTo("First me sibling_value1._Second me sibling_value2._" +
        "Sub text1 sub_value1 end. Sub text2 sub_value2_1 end. Sub text2 sub_value2_2 end. Sub text2 sub_value2_3 end. Sub text3 sub_value3 end. " +
        "Sub sub text1 sub_sub_value1 end. Sub sub text2 end._Third me sibling_value3."))
    },
    testM("SubClassOf should be replaced correctly with list values (single value in it).") {
      val term: OWLClass = Class("http://purl.obolibrary.org/obo/ONT_0000001")
      val item: OWLClass = Class("http://purl.obolibrary.org/obo/ONT_0000002")
      val partOf: OWLObjectProperty = ObjectProperty("http://purl.obolibrary.org/obo/BFO_0000050")
      val OboInOwlSource: OWLAnnotationProperty = AnnotationProperty("http://www.geneontology.org/formats/oboInOwl#source")

      val exp_clause = PrintfClause("'part_of' some %s", Some(List("item")), None)
      val exp_printf = MultiClausePrintf(Some(" and "), Some(List(exp_clause)))
      val dosdp: DOSDP = DOSDP.empty.copy(
        pattern_name = Some("test_restrictions_pattern"),
        classes = Some(Map("thing" -> "owl:Thing", "cell" -> "CL:0000000")),
        relations = Some(Map("part_of" -> "BFO:0000050")),
        list_vars = Some(Map("item" -> "'thing'")),
        subClassOf = Some(PrintfOWLConvenience(None, None, None, Some(exp_printf)))
      )

      val logicalAxiom1: OWLSubClassOfAxiom = term SubClassOf (partOf some item)

      for {
        axioms <- Generate.renderPattern(dosdp, OBOPrefixes, List(Map("defined_class" -> "ONT:0000001", "item" -> "ONT:0000002", "axiom_filter" -> "all")), None, outputLogicalAxioms = true, outputAnnotationAxioms = true, None, annotateAxiomSource = false, OboInOwlSource, generateDefinedClass = false, Map.empty)
      } yield assert(axioms)(contains(logicalAxiom1))
    },
    testM("SubClassOf should be replaced correctly with list values (two values in it).") {
      val term: OWLClass = Class("http://purl.obolibrary.org/obo/ONT_0000001")
      val item: OWLClass = Class("http://purl.obolibrary.org/obo/ONT_0000002")
      val item2: OWLClass = Class("http://purl.obolibrary.org/obo/ONT_0000003")
      val partOf: OWLObjectProperty = ObjectProperty("http://purl.obolibrary.org/obo/BFO_0000050")
      val OboInOwlSource: OWLAnnotationProperty = AnnotationProperty("http://www.geneontology.org/formats/oboInOwl#source")

      val exp_clause = PrintfClause("'part_of' some %s", Some(List("item")), None)
      val exp_printf = MultiClausePrintf(Some(" and "), Some(List(exp_clause)))
      val dosdp: DOSDP = DOSDP.empty.copy(
        pattern_name = Some("test_restrictions_pattern"),
        classes = Some(Map("thing" -> "owl:Thing", "cell" -> "CL:0000000")),
        relations = Some(Map("part_of" -> "BFO:0000050")),
        list_vars = Some(Map("item" -> "'thing'")),
        subClassOf = Some(PrintfOWLConvenience(None, None, None, Some(exp_printf)))
      )

      val logicalAxiom1: OWLSubClassOfAxiom = term SubClassOf ((partOf some item) and (partOf some item2))

      for {
        axioms <- Generate.renderPattern(dosdp, OBOPrefixes, List(Map("defined_class" -> "ONT:0000001", "item" -> "ONT:0000002|ONT:0000003", "axiom_filter" -> "all")), None, outputLogicalAxioms = true, outputAnnotationAxioms = true, None, annotateAxiomSource = false, OboInOwlSource, generateDefinedClass = false, Map.empty)
      } yield assert(axioms)(contains(logicalAxiom1))
    },
    testM("SubClassOf with annotation should be replaced correctly.") {
      val term: OWLClass = Class("http://purl.obolibrary.org/obo/ONT_0000001")
      val item: OWLClass = Class("http://purl.obolibrary.org/obo/ONT_0000002")
      val OboInOwlSource: OWLAnnotationProperty = AnnotationProperty("http://www.geneontology.org/formats/oboInOwl#source")
      val rdfsComment: OWLAnnotationProperty = AnnotationProperty("http://www.w3.org/2000/01/rdf-schema#comment")

      val exp_clause = PrintfClause("%s", Some(List("comment_var")), None)
      val exp_printf = MultiClausePrintf(Some(" and "), Some(List(exp_clause)))
      val annotations = Some(List(PrintfAnnotation(None, "rdfsComment", None, None, None, Some(exp_printf))))
      val dosdp: DOSDP = DOSDP.empty.copy(
        pattern_name = Some("test_restrictions_pattern"),
        classes = Some(Map("thing" -> "owl:Thing", "cell" -> "CL:0000000")),
        annotationProperties = Some(Map("rdfsComment" -> "rdfs:comment")),
        vars = Some(Map("item" -> "'thing'")),
        data_list_vars = Some(Map("comment_var" -> "xsd:string")),
        subClassOf = Some(PrintfOWLConvenience(annotations, Some("%s"), Some(List("item")), None))
      )

      val logicalAxiom1: OWLSubClassOfAxiom = term SubClassOf item
      val annotationAxiom1: OWLAxiom = logicalAxiom1 Annotation(rdfsComment, "My comment1.")

      for {
        axioms <- Generate.renderPattern(dosdp, OBOPrefixes, List(Map("defined_class" -> "ONT:0000001", "item" -> "ONT:0000002", "axiom_filter" -> "all", "comment_var" -> "My comment1.")), None, outputLogicalAxioms = true, outputAnnotationAxioms = true, None, annotateAxiomSource = false, OboInOwlSource, generateDefinedClass = false, Map.empty)
        _ = println(axioms)
      } yield assert(axioms)(contains(annotationAxiom1))
    },
    testM("Annotation should be replaced correctly. Tests the default behaviour (without multiClauses and multiValues).") {
      val term: OWLClass = Class("http://purl.obolibrary.org/obo/ONT_0000001")
      val exacySynonym: OWLAnnotationProperty = AnnotationProperty("http://purl.obolibrary.org/obo/hasExactSynonym")
      val OboInOwlSource: OWLAnnotationProperty = AnnotationProperty("http://www.geneontology.org/formats/oboInOwl#source")

      val dosdp: DOSDP = DOSDP.empty.copy(
        pattern_name = Some("test_restrictions_pattern"),
        classes = Some(Map("disease" -> "MONDO:0000001")),
        vars = Some(Map("disease" -> "''''disease'''")),
        annotationProperties = Some(Map("exact_synonym" -> "http://purl.obolibrary.org/obo/hasExactSynonym")),
        annotations = Some(List(PrintfAnnotation(None, "exact_synonym", Some("'%s, acute'"), Some(List("disease")), None, None)))
      )

      val annotationAxiom: OWLAnnotationAssertionAxiom = term Annotation(exacySynonym, "'http://purl.obolibrary.org/obo/ONT_0000002, acute'")

      for {
        axioms <- Generate.renderPattern(dosdp, OBOPrefixes, List(Map("defined_class" -> "ONT:0000001", "disease" -> "ONT:0000002")), None, outputLogicalAxioms = true, outputAnnotationAxioms = true, None, annotateAxiomSource = false, OboInOwlSource, generateDefinedClass = false, Map.empty)
      } yield assert(axioms)(contains(annotationAxiom))
    },
    testM("Annotations should be replaced correctly with list values (two values in it).") {
      val term: OWLClass = Class("http://purl.obolibrary.org/obo/ONT_0000001")
      val exacySynonym: OWLAnnotationProperty = AnnotationProperty("http://purl.obolibrary.org/obo/hasExactSynonym")
      val OboInOwlSource: OWLAnnotationProperty = AnnotationProperty("http://www.geneontology.org/formats/oboInOwl#source")

      val exp_clause = PrintfClause("'%s, acute'", Some(List("disease")), None)
      val exp_printf = MultiClausePrintf(Some(" and "), Some(List(exp_clause)))
      val dosdp: DOSDP = DOSDP.empty.copy(
        pattern_name = Some("test_restrictions_pattern"),
        classes = Some(Map("disease" -> "MONDO:0000001")),
        list_vars = Some(Map("disease" -> "''''disease'''")),
        annotationProperties = Some(Map("exact_synonym" -> "http://purl.obolibrary.org/obo/hasExactSynonym")),
        annotations = Some(List(PrintfAnnotation(None, "exact_synonym", None, None, None, Some(exp_printf))))
      )

      val annotationAxiom1: OWLAnnotationAssertionAxiom = term Annotation(exacySynonym, "'http://purl.obolibrary.org/obo/ONT_0000002, acute'")
      val annotationAxiom2: OWLAnnotationAssertionAxiom = term Annotation(exacySynonym, "'http://purl.obolibrary.org/obo/ONT_0000003, acute'")

      for {
        axioms <- Generate.renderPattern(dosdp, OBOPrefixes, List(Map("defined_class" -> "ONT:0000001", "disease" -> "ONT:0000002|ONT:0000003")), None, outputLogicalAxioms = true, outputAnnotationAxioms = true, None, annotateAxiomSource = false, OboInOwlSource, generateDefinedClass = false, Map.empty)
        //        _ <- Utilities.saveAxiomsToOntology(axioms, "./dummy.owl")
      } yield assert(axioms)(contains(annotationAxiom1)) &&
        assert(axioms)(contains(annotationAxiom2))
    },
    testM("Logical axiom separators should be validated. Only and, or allowed for logical expressions.") {
      var exceptionOccurred = false

      try {
        val exp_clause = PrintfClause("'part_of' some %s", Some(List("item")), None)
        val exp_printf = MultiClausePrintf(Some(" BAD_SEPARATOR "), Some(List(exp_clause)))
        DOSDP.empty.copy(
          pattern_name = Some("test_restrictions_pattern"),
          classes = Some(Map("thing" -> "owl:Thing", "cell" -> "CL:0000000")),
          relations = Some(Map("part_of" -> "BFO:0000050")),
          list_vars = Some(Map("item" -> "'thing'")),
          subClassOf = Some(PrintfOWLConvenience(None, None, None, Some(exp_printf)))
        )
      } catch {
        case e: IllegalArgumentException => exceptionOccurred = true
      }
      assertM(ZIO.effect(exceptionOccurred))(isTrue)
    },
  )

}
