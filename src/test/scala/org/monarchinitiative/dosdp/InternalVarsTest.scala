package org.monarchinitiative.dosdp

import java.io.File
import com.github.tototoshi.csv.TSVFormat
import org.monarchinitiative.dosdp.cli.{Config, Generate}
import org.phenoscape.scowl.{not => _, _}
import org.semanticweb.owlapi.model.{IRI, OWLAnnotationProperty}
import zio.test.Assertion._
import zio.test._

object InternalVarsTest extends DefaultRunnableSpec {

  val oboIAO115: OWLAnnotationProperty = AnnotationProperty("http://purl.obolibrary.org/obo/IAO_0000115")

  def spec = suite("Internal vars and multi_value clauses test") (
    testM("Utilize internal_vars join function with data_list_vars correctly.") {
      for {
        dosdp <- Config.inputDOSDPFrom("src/test/resources/org/monarchinitiative/dosdp/internal_vars_test.yaml")
        columnsAndFillers <- Generate.readFillers(new File("src/test/resources/org/monarchinitiative/dosdp/internal_vars_test.tsv"), new TSVFormat {})
        (_, fillers) = columnsAndFillers
        axioms <- Generate.renderPattern(dosdp: DOSDP, OBOPrefixes, fillers, None, outputLogicalAxioms = true, outputAnnotationAxioms = true, None, annotateAxiomSource = false, AxiomRestrictionsTest.OboInOwlSource, generateDefinedClass = false, Map.empty)
      } yield assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/AllenDendClass_CS202002013_159") Annotation(oboIAO115, "A http://purl.obolibrary.org/obo/CL_0011005 of the http://purl.obolibrary.org/obo/NCBITaxon_10090 http://purl.obolibrary.org/obo/UBERON_0001384. These cells can be distinguished from other cells in the http://purl.obolibrary.org/obo/UBERON_0001384 by their expression of http://purl.obolibrary.org/obo/ensembl_ENSMUSG00000042453,http://purl.obolibrary.org/obo/ensembl_ENSMUSG00000004366. These cells have projection type http://purl.obolibrary.org/obo/CL_0011005. The soma of these cells in located in: http://purl.obolibrary.org/obo/CL_0000359 or http://purl.obolibrary.org/obo/CL_4023051 or http://purl.obolibrary.org/obo/CL_0000128."))) &&
        assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/AllenDendClass_CS202002013_40") Annotation(oboIAO115, "A http://purl.obolibrary.org/obo/CL_0000679 of the http://purl.obolibrary.org/obo/NCBITaxon_10090 http://purl.obolibrary.org/obo/UBERON_0001384. These cells can be distinguished from other cells in the http://purl.obolibrary.org/obo/UBERON_0001384 by their expression of http://purl.obolibrary.org/obo/ensembl_ENSMUSG00000098760,http://purl.obolibrary.org/obo/ensembl_ENSMUSG00000043659. These cells also express http://purl.obolibrary.org/obo/ensembl_ENSMUSG00000043441."))) &&
        assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/AllenDendClass_CS202002013_35") Annotation(oboIAO115, "A http://purl.obolibrary.org/obo/CL_0011005 of the http://purl.obolibrary.org/obo/NCBITaxon_10090 http://purl.obolibrary.org/obo/UBERON_0001384. These cells can be distinguished from other cells in the http://purl.obolibrary.org/obo/UBERON_0001384 by their expression of http://purl.obolibrary.org/obo/ensembl_ENSMUSG00000025537,http://purl.obolibrary.org/obo/ensembl_ENSMUSG00000030235. These cells also express http://purl.obolibrary.org/obo/ensembl_ENSMUSG00000053930, http://purl.obolibrary.org/obo/ensembl_ENSMUSG00000035105. These cells have projection type http://purl.obolibrary.org/obo/CL_0011003. The soma of these cells in located in: http://purl.obolibrary.org/obo/CL_0000359."))) &&
        assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/AllenDendClass_CS202002013_34") Annotation(oboIAO115, "A http://purl.obolibrary.org/obo/CL_0011005 of the http://purl.obolibrary.org/obo/NCBITaxon_10090 http://purl.obolibrary.org/obo/UBERON_0001384. These cells can be distinguished from other cells in the http://purl.obolibrary.org/obo/UBERON_0001384 by their expression of http://purl.obolibrary.org/obo/ensembl_ENSMUSG00000019772. These cells also express http://purl.obolibrary.org/obo/ensembl_ENSMUSG00000055214. These cells have projection type http://purl.obolibrary.org/obo/CL_0011006. The soma of these cells in located in: http://purl.obolibrary.org/obo/CL_4023051.")))
    },
    testM("Utilize internal_vars join function with list_vars correctly.") {
      for {
        dosdp <- Config.inputDOSDPFrom("src/test/resources/org/monarchinitiative/dosdp/internal_vars_test.yaml")
        columnsAndFillers <- Generate.readFillers(new File("src/test/resources/org/monarchinitiative/dosdp/internal_vars_test.tsv"), new TSVFormat {})
        (_, fillers) = columnsAndFillers
        axioms <- Generate.renderPattern(dosdp: DOSDP, OBOPrefixes, fillers, None, outputLogicalAxioms = true, outputAnnotationAxioms = true, None, annotateAxiomSource = false, AxiomRestrictionsTest.OboInOwlSource, generateDefinedClass = false, Map.empty)
      } yield assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/AllenDendClass_CS202002013_159") Annotation(RDFSLabel, "Sample name with type list: http://purl.obolibrary.org/obo/CL_0011005."))) &&
        assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/AllenDendClass_CS202002013_35") Annotation(RDFSLabel, "Sample name with type list: http://purl.obolibrary.org/obo/CL_0000679 or http://purl.obolibrary.org/obo/CL_0011005."))) &&
        assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/AllenDendClass_CS202002013_37") Annotation(RDFSLabel, "Sample name with type list: http://purl.obolibrary.org/obo/CL_0011003 or http://purl.obolibrary.org/obo/CL_0011002 or http://purl.obolibrary.org/obo/CL_0011001.")))
    },
    testM("Utilize internal_vars and list_vars expanded with the ontology labels correctly.") {
      for {
        ontology <- Utilities.loadOntology("src/test/resources/org/monarchinitiative/dosdp/internal_vars_test.ofn", None)
        dosdp <- Config.inputDOSDPFrom("src/test/resources/org/monarchinitiative/dosdp/internal_vars_test.yaml")
        columnsAndFillers <- Generate.readFillers(new File("src/test/resources/org/monarchinitiative/dosdp/internal_vars_test.tsv"), new TSVFormat {})
        (_, fillers) = columnsAndFillers
        axioms <- Generate.renderPattern(dosdp: DOSDP, OBOPrefixes, fillers, Some(ontology), outputLogicalAxioms = true, outputAnnotationAxioms = true, None, annotateAxiomSource = false, AxiomRestrictionsTest.OboInOwlSource, generateDefinedClass = false, Map.empty)
      } yield assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/AllenDendClass_CS202002013_159") Annotation(RDFSComment, "Entity label expansion test: MyEntity1_Label."))) &&
        assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/AllenDendClass_CS202002013_34") Annotation(RDFSComment, "Entity label expansion test: MyEntity2_Label."))) &&
        assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/AllenDendClass_CS202002013_35") Annotation(RDFSComment, "Entity label expansion test: MyEntity2_Label or MyEntity1_Label."))) &&
        assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/AllenDendClass_CS202002013_36") Annotation(RDFSComment, "Entity label expansion test: http://purl.obolibrary.org/obo/XX_00001."))) &&
        assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/AllenDendClass_CS202002013_161") Annotation(RDFSComment, "Entity label expansion test: http://purl.obolibrary.org/obo/CL_NotExists or MyEntity2_Label.")))
    },
    testM("Internal_vars and prefixes expanded with the ontology labels correctly.") {
      val specifiedPrefixes = Map("ensembl" -> "http://identifiers.org/ensembl/")
      val prefixes = specifiedPrefixes.orElse(OBOPrefixes)
      for {
        ontology <- Utilities.loadOntology("src/test/resources/org/monarchinitiative/dosdp/internal_vars_test.ofn", None)
        dosdp <- Config.inputDOSDPFrom("src/test/resources/org/monarchinitiative/dosdp/internal_vars_test.yaml")
        columnsAndFillers <- Generate.readFillers(new File("src/test/resources/org/monarchinitiative/dosdp/internal_vars_test.tsv"), new TSVFormat {})
        (_, fillers) = columnsAndFillers
        axioms <- Generate.renderPattern(dosdp: DOSDP, prefixes, fillers, Some(ontology), outputLogicalAxioms = true, outputAnnotationAxioms = true, None, annotateAxiomSource = false, AxiomRestrictionsTest.OboInOwlSource, generateDefinedClass = false, Map.empty)
      } yield assert(axioms)(contains(Class("http://purl.obolibrary.org/obo/AllenDendClass_CS202002013_35") Annotation(oboIAO115, "A MyEntity1_Label of the http://purl.obolibrary.org/obo/NCBITaxon_10090 http://purl.obolibrary.org/obo/UBERON_0001384. These cells can be distinguished from other cells in the http://purl.obolibrary.org/obo/UBERON_0001384 by their expression of Phkg1,Slco1c1. These cells also express http://identifiers.org/ensembl/ENSMUSG00000053930, http://identifiers.org/ensembl/ENSMUSG00000035105. These cells have projection type http://purl.obolibrary.org/obo/CL_0011003. The soma of these cells in located in: http://purl.obolibrary.org/obo/CL_0000359."
      )))
    }

  )

}
