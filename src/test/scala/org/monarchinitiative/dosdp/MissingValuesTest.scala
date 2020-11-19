package org.monarchinitiative.dosdp

import java.io.File

import com.github.tototoshi.csv.TSVFormat
import org.monarchinitiative.dosdp.cli.{Config, Generate}
import zio.test.Assertion._
import zio.test._

object MissingValuesTest extends DefaultRunnableSpec {

  def spec = suite("Missing columns and cell values") {
    testM("Missing columns and cell values should be handled by dropping outputs") {
      for {
        dosdp <- Config.inputDOSDPFrom("src/test/resources/org/monarchinitiative/dosdp/missing_values_test.yaml")
        columnsAndFillers <- Generate.readFillers(new File("src/test/resources/org/monarchinitiative/dosdp/missing_values_test.tsv"), new TSVFormat {})
        (_, fillers) = columnsAndFillers
        // should not fail from missing values
        axioms <- Generate.renderPattern(dosdp: DOSDP, OBOPrefixes, fillers, None, true, true, None, false, AxiomRestrictionsTest.OboInOwlSource, false)
      } yield assert(axioms)(isNonEmpty)
    }
  }

}
