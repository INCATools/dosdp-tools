package org.monarchinitiative.dosdp

import java.io.File

import com.github.tototoshi.csv.{DefaultCSVFormat, TSVFormat}
import org.monarchinitiative.dosdp.cli.{Config, Generate}
import org.semanticweb.owlapi.model.{OWLAnnotationAssertionAxiom, OWLClass}
import zio.test.Assertion._
import zio.test._
import org.phenoscape.scowl._

object MissingValuesTest extends DefaultRunnableSpec {

  val term: OWLClass = Class("http://purl.obolibrary.org/obo/EX_0001")
  val term0001: OWLAnnotationAssertionAxiom = term Annotation(RDFSLabel, "Term 0001")

  val testMissingColumnsAndCellValuesFromTSV = suite("Missing columns and cell values from TSV") {
    testM("Missing columns and cell values should be handled by dropping outputs") {
      for {
        dosdp <- Config.inputDOSDPFrom("src/test/resources/org/monarchinitiative/dosdp/missing_values_test.yaml")
        columnsAndFillers <- Generate.readFillers(new File("src/test/resources/org/monarchinitiative/dosdp/missing_values_test.tsv"), new TSVFormat {})
        (_, fillers) = columnsAndFillers
        // should not fail from missing values
        axioms <- Generate.renderPattern(dosdp: DOSDP, OBOPrefixes, fillers, None, true, true, None, false, AxiomRestrictionsTest.OboInOwlSource, false)
        //_ = axioms.foreach(a => println(a))
      } yield assert(axioms)(isNonEmpty) && assert(axioms)(contains(term0001))
    }
  }

  val testMissingColumnsAndCellValuesFromCSV = suite("Missing columns and cell values from CSV") {
    testM("Missing columns and cell values should be handled by dropping outputs") {
      for {
        dosdp <- Config.inputDOSDPFrom("src/test/resources/org/monarchinitiative/dosdp/missing_values_test.yaml")
        columnsAndFillers <- Generate.readFillers(new File("src/test/resources/org/monarchinitiative/dosdp/missing_values_test.csv"), new DefaultCSVFormat {})
        (_, fillers) = columnsAndFillers
        // should not fail from missing values
        axioms <- Generate.renderPattern(dosdp: DOSDP, OBOPrefixes, fillers, None, true, true, None, false, AxiomRestrictionsTest.OboInOwlSource, false)
        //_ = axioms.foreach(a => println(a))
      } yield assert(axioms)(isNonEmpty) && assert(axioms)(contains(term0001))
    }
  }

  def spec = suite("All tests")(testMissingColumnsAndCellValuesFromTSV, testMissingColumnsAndCellValuesFromCSV)

}
