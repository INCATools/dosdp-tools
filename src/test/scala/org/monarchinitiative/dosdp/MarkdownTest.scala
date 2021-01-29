package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.{Config, Markdown}
import org.phenoscape.scowl.{not => _}
import zio.ZIO
import zio.test.Assertion._
import zio.test._

import java.nio.file.{Files, Paths}
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

object MarkdownTest extends DefaultRunnableSpec {

  def spec = suite("Markdown test") {
    testM("simple test") {
      for {
//        dosdp <- Config.inputDOSDPFrom("src/test/resources/org/monarchinitiative/dosdp/missing_values_test.yaml")
        dosdp <- Config.inputDOSDPFrom("src/test/resources/tutorial/exposure_with_input.yaml")
        _ = scribe.info(s"${dosdp.pattern_name}")

        out = new ListBuffer[String]()

        patternName <- ZIO.fromOption(dosdp.pattern_name).orElseSucceed("No Pattern Name")
        _ = out += s"## $patternName \n"
        _ = out += Markdown.processPrintfText("name", dosdp.name)
        _ = out += Markdown.processPrintfText("def", dosdp.`def`)
        _ = out += Markdown.processPrintfText("equivalentTo", dosdp.equivalentTo)
        _ = out += Markdown.processPrintfText("subClassOf", dosdp.subClassOf)
        _ = out += Markdown.processPrintfText("GCI", dosdp.GCI)

        _ = Files.write(Paths.get("markdown.md"), out.toList.asJava)
      } yield assert(out)(isNonEmpty)
    }
  }

}
