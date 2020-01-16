package org.monarchinitiative.dosdp

import java.io.File

import com.github.tototoshi.csv.TSVFormat
import org.monarchinitiative.dosdp.cli.Generate

class MissingValuesTest extends UnitSpec {

  "Missing columns and cell values" should "be handled by dropping outputs" in {
    val dosdp = Generate.inputDOSDPFrom("src/test/resources/org/monarchinitiative/dosdp/missing_values_test.yaml")
    val fillers = Generate.readFillers(new File("src/test/resources/org/monarchinitiative/dosdp/missing_values_test.tsv"), new TSVFormat {})
    val axioms = Generate.renderPattern(dosdp: DOSDP, OBOPrefixes, fillers, None, true, true, None, false)
    // No exceptions should be thrown
  }

}
