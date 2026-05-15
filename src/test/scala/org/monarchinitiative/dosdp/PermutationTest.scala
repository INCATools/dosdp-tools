package org.monarchinitiative.dosdp

import java.io.File
import com.github.tototoshi.csv.TSVFormat
import org.monarchinitiative.dosdp.cli.{Config, DOSDPError, Generate}
import org.phenoscape.scowl.{not => _, _}
import org.semanticweb.owlapi.model.OWLAnnotationProperty
import zio.test.Assertion._
import zio.test._
import zio.logging._

object PermutationTest extends DefaultRunnableSpec {

  val oioExactSynonym: OWLAnnotationProperty = AnnotationProperty("http://www.geneontology.org/formats/oboInOwl#hasExactSynonym")
  val oioRelatedSynonym: OWLAnnotationProperty = AnnotationProperty("http://www.geneontology.org/formats/oboInOwl#hasRelatedSynonym")

  def spec = suite("Permutation feature test")(
    testM("Generate synonyms from filler term synonyms using permutations") {
      for {
        ontology <- Utilities.loadOntology("src/test/resources/org/monarchinitiative/dosdp/permutation_test.ofn", None)
        dosdp <- Config.inputDOSDPFrom("src/test/resources/org/monarchinitiative/dosdp/permutation_test.yaml")
        columnsAndFillers <- Generate.readFillers(new File("src/test/resources/org/monarchinitiative/dosdp/permutation_test.tsv"), new TSVFormat {})
        (_, fillers) = columnsAndFillers
        axioms <- Generate.renderPattern(dosdp: DOSDP, OBOPrefixes, fillers, Some(ontology), outputLogicalAxioms = false, outputAnnotationAxioms = true, None, annotateAxiomSource = false, AxiomRestrictionsTest.OboInOwlSource, generateDefinedClass = false, Map.empty)
      } yield {
        Harness.assertMatchesGolden(axioms, "src/test/resources/org/monarchinitiative/dosdp/permutation_test.golden.ofn") &&
        Harness.assertNoPlaceholderIRIs(axioms) &&
        Harness.assertNoPlaceholderLiterals(axioms) &&
        // Test that the label-based name is generated
        assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/MONDO_0001001") Annotation(RDFSLabel, "acute heart disease"))) &&
        assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/MONDO_0001002") Annotation(RDFSLabel, "acute lung disease"))) &&
        // Test that label-based exact_synonym (via generated_synonyms) is generated
        assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/MONDO_0001001") Annotation(oioExactSynonym, "heart disease, acute"))) &&
        // Test that permutation-based exact_synonyms are generated from filler's exact_synonym
        assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/MONDO_0001001") Annotation(oioExactSynonym, "cardiac disease, acute"))) &&
        assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/MONDO_0001002") Annotation(oioExactSynonym, "pulmonary disease, acute"))) &&
        // Test that related_synonym annotations are generated with permutations from both exact and related synonyms
        assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/MONDO_0001001") Annotation(oioRelatedSynonym, "acute form of heart disease"))) &&
        assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/MONDO_0001001") Annotation(oioRelatedSynonym, "acute form of cardiac disease"))) &&
        assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/MONDO_0001001") Annotation(oioRelatedSynonym, "acute form of heart condition"))) &&
        // Lung disease has no related_synonym; only exact_synonym contributes a permutation.
        // Expect label-only + the single exact_synonym-derived value; the absent property must not error.
        assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/MONDO_0001002") Annotation(oioRelatedSynonym, "acute form of lung disease"))) &&
        assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/MONDO_0001002") Annotation(oioRelatedSynonym, "acute form of pulmonary disease")))
      }
    },
    testM("Generate annotations without ontology (no permutation values available)") {
      for {
        dosdp <- Config.inputDOSDPFrom("src/test/resources/org/monarchinitiative/dosdp/permutation_test.yaml")
        columnsAndFillers <- Generate.readFillers(new File("src/test/resources/org/monarchinitiative/dosdp/permutation_test.tsv"), new TSVFormat {})
        (_, fillers) = columnsAndFillers
        // Without ontology, permutation values won't be available - only label-based annotation should be generated
        axioms <- Generate.renderPattern(dosdp: DOSDP, OBOPrefixes, fillers, None, outputLogicalAxioms = false, outputAnnotationAxioms = true, None, annotateAxiomSource = false, AxiomRestrictionsTest.OboInOwlSource, generateDefinedClass = false, Map.empty)
      } yield {
        Harness.assertNoPlaceholderIRIs(axioms) &&
        Harness.assertNoPlaceholderLiterals(axioms) &&
        // Without ontology, the filler IRIs are used directly (no label lookup)
        assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/MONDO_0001001") Annotation(RDFSLabel, "acute http://purl.obolibrary.org/obo/MONDO_0005267"))) &&
        assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/MONDO_0001001") Annotation(oioExactSynonym, "http://purl.obolibrary.org/obo/MONDO_0005267, acute")))
      }
    },
    testM("Cartesian product of permutations across multiple vars") {
      val defined = Class("http://purl.obolibrary.org/obo/TEST_9999999")
      val fillers = List(Map(
        "defined_class" -> "TEST:9999999",
        "quality"       -> "TEST:0000001",
        "entity"        -> "TEST:0000002"
      ))
      for {
        ontology <- Utilities.loadOntology("src/test/resources/org/monarchinitiative/dosdp/permutation_cartesian.ofn", None)
        dosdp <- Config.inputDOSDPFrom("src/test/resources/org/monarchinitiative/dosdp/permutation_cartesian.yaml")
        axioms <- Generate.renderPattern(dosdp, OBOPrefixes, fillers, Some(ontology), outputLogicalAxioms = false, outputAnnotationAxioms = true, None, annotateAxiomSource = false, AxiomRestrictionsTest.OboInOwlSource, generateDefinedClass = false, Map.empty)
      } yield {
        Harness.assertMatchesGolden(axioms, "src/test/resources/org/monarchinitiative/dosdp/permutation_cartesian.golden.ofn") &&
        Harness.assertNoPlaceholderIRIs(axioms) &&
        Harness.assertNoPlaceholderLiterals(axioms) &&
        // 1 label-based + 8 permutation-based = 9 expected exact_synonym annotations on the defined class.
        // Quality values: enlarged (label), big, hypertrophic.
        // Entity values:  heart (label), cardiac organ, pump.
        assert(axioms)(contains(defined Annotation(RDFSLabel, "enlarged heart"))) &&
        assert(axioms)(contains(defined Annotation(oioExactSynonym, "enlarged heart"))) &&
        assert(axioms)(contains(defined Annotation(oioExactSynonym, "big heart"))) &&
        assert(axioms)(contains(defined Annotation(oioExactSynonym, "hypertrophic heart"))) &&
        assert(axioms)(contains(defined Annotation(oioExactSynonym, "enlarged cardiac organ"))) &&
        assert(axioms)(contains(defined Annotation(oioExactSynonym, "enlarged pump"))) &&
        assert(axioms)(contains(defined Annotation(oioExactSynonym, "big cardiac organ"))) &&
        assert(axioms)(contains(defined Annotation(oioExactSynonym, "big pump"))) &&
        assert(axioms)(contains(defined Annotation(oioExactSynonym, "hypertrophic cardiac organ"))) &&
        assert(axioms)(contains(defined Annotation(oioExactSynonym, "hypertrophic pump")))
      }
    },
    testM("Permutation var not in annotation vars list fails validation") {
      val fillers = List(Map("defined_class" -> "TEST:9999999", "disease" -> "TEST:0000001"))
      val program = for {
        dosdp <- Config.inputDOSDPFrom("src/test/resources/org/monarchinitiative/dosdp/permutation_invalid_var.yaml")
        axioms <- Generate.renderPattern(dosdp, OBOPrefixes, fillers, None, outputLogicalAxioms = false, outputAnnotationAxioms = true, None, annotateAxiomSource = false, AxiomRestrictionsTest.OboInOwlSource, generateDefinedClass = false, Map.empty)
      } yield axioms
      assertM(program.flip.map(_.msg))(containsString("not_a_real_var"))
    },
    testM("Permutation referencing an undeclared annotation property fails validation") {
      val fillers = List(Map("defined_class" -> "TEST:9999999", "disease" -> "TEST:0000001"))
      val program = for {
        dosdp <- Config.inputDOSDPFrom("src/test/resources/org/monarchinitiative/dosdp/permutation_invalid_property.yaml")
        axioms <- Generate.renderPattern(dosdp, OBOPrefixes, fillers, None, outputLogicalAxioms = false, outputAnnotationAxioms = true, None, annotateAxiomSource = false, AxiomRestrictionsTest.OboInOwlSource, generateDefinedClass = false, Map.empty)
      } yield axioms
      assertM(program.flip.map(_.msg))(containsString("undeclared_property"))
    },
    // A non-empty `override:` value short-circuits `permutations:` on the same annotation:
    // the row emits exactly the override string, and permutation expansion is skipped.
    testM("Override value short-circuits permutations for that row") {
      val overridden  = Class("http://purl.obolibrary.org/obo/TEST_9999992")
      val notOverridden = Class("http://purl.obolibrary.org/obo/TEST_9999991")
      val fillers = List(
        Map("defined_class" -> "TEST:9999991", "disease" -> "MONDO:0005267", "name_override" -> ""),
        Map("defined_class" -> "TEST:9999992", "disease" -> "MONDO:0005267", "name_override" -> "hand-written value")
      )
      for {
        ontology <- Utilities.loadOntology("src/test/resources/org/monarchinitiative/dosdp/permutation_test.ofn", None)
        dosdp <- Config.inputDOSDPFrom("src/test/resources/org/monarchinitiative/dosdp/permutation_override.yaml")
        axioms <- Generate.renderPattern(dosdp, OBOPrefixes, fillers, Some(ontology), outputLogicalAxioms = false, outputAnnotationAxioms = true, None, annotateAxiomSource = false, AxiomRestrictionsTest.OboInOwlSource, generateDefinedClass = false, Map.empty)
      } yield {
        Harness.assertNoPlaceholderIRIs(axioms) &&
        Harness.assertNoPlaceholderLiterals(axioms) &&
        // Non-overridden row: label-based + permutation-based annotations are produced as normal.
        assert(axioms)(contains(notOverridden Annotation(oioExactSynonym, "heart disease, acute"))) &&
        assert(axioms)(contains(notOverridden Annotation(oioExactSynonym, "cardiac disease, acute"))) &&
        // Overridden row: only the override value appears; neither the template nor the permutation runs.
        assert(axioms)(contains(overridden Annotation(oioExactSynonym, "hand-written value"))) &&
        assert(axioms)(not(contains(overridden Annotation(oioExactSynonym, "heart disease, acute")))) &&
        assert(axioms)(not(contains(overridden Annotation(oioExactSynonym, "cardiac disease, acute"))))
      }
    }
  ).provideCustomLayer(Logging.consoleErr())

}
