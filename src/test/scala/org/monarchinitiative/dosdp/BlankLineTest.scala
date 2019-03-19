package org.monarchinitiative.dosdp

import java.io.File

import com.github.tototoshi.csv.TSVFormat
import org.monarchinitiative.dosdp.cli.Generate
import org.phenoscape.scowl._

class BlankLineTest extends UnitSpec {

  val dosdp = Generate.inputDOSDPFrom("src/test/resources/org/monarchinitiative/dosdp/test_blank_lines.yaml")
  val fillers = Generate.readFillers(new File("src/test/resources/org/monarchinitiative/dosdp/test_blank_lines.tsv"), new TSVFormat {})
  val axioms = Generate.renderPattern(dosdp: DOSDP, OBOPrefixes, fillers: Iterator[Map[String, String]], None, true, true, None, false)

  "Blank lines" should "not cause errors" in {
    axioms should contain(Class("http://ex.org/1") Annotation(RDFSLabel, "http://example.org/Entity1 thing"))
    axioms should contain(Class("http://ex.org/2") Annotation(RDFSLabel, "Entity 2 TSV"))
    axioms should contain(Class("http://ex.org/3") Annotation(RDFSLabel, "http://example.org/Entity3 thing"))
  }

}
