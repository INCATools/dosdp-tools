package org.monarchinitiative.dosdp

import java.io.File
import com.github.tototoshi.csv.TSVFormat
import org.monarchinitiative.dosdp.cli.{Config, Generate}
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
        assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/MONDO_0001001") Annotation(oioRelatedSynonym, "acute form of heart condition")))
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
        // Without ontology, the filler IRIs are used directly (no label lookup)
        assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/MONDO_0001001") Annotation(RDFSLabel, "acute http://purl.obolibrary.org/obo/MONDO_0005267"))) &&
        assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/MONDO_0001001") Annotation(oioExactSynonym, "http://purl.obolibrary.org/obo/MONDO_0005267, acute")))
      }
    }
  ).provideCustomLayer(Logging.consoleErr())

}
