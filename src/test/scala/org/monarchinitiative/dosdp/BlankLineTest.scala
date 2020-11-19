package org.monarchinitiative.dosdp

import java.io.File

import com.github.tototoshi.csv.TSVFormat
import org.monarchinitiative.dosdp.cli.{Config, Generate}
import org.phenoscape.scowl.{not => _, _}
import zio.test.Assertion._
import zio.test._

object BlankLineTest extends DefaultRunnableSpec {

  def spec = suite("Blank lines test") {
    testM("Blank lines should not cause errors") {
      for {
        dosdp <- Config.inputDOSDPFrom("src/test/resources/org/monarchinitiative/dosdp/test_blank_lines.yaml")
        columnsAndFillers <- Generate.readFillers(new File("src/test/resources/org/monarchinitiative/dosdp/test_blank_lines.tsv"), new TSVFormat {})
        (_, fillers) = columnsAndFillers
        axioms <- Generate.renderPattern(dosdp: DOSDP, OBOPrefixes, fillers, None, true, true, None, false, AxiomRestrictionsTest.OboInOwlSource, false)
      } yield assert(axioms)(contains(Class("http://ex.org/1") Annotation(RDFSLabel, "http://example.org/Entity1 thing"))) &&
        assert(axioms)(contains(Class("http://ex.org/2") Annotation(RDFSLabel, "Entity 2 TSV"))) &&
        assert(axioms)(contains(Class("http://ex.org/3") Annotation(RDFSLabel, "http://example.org/Entity3 thing")))
    }
  }

}
