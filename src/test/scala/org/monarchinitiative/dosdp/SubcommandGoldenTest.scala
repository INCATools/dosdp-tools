package org.monarchinitiative.dosdp

import org.apache.jena.sys.JenaSystem
import org.monarchinitiative.dosdp.cli._
import zio._
import zio.logging._
import zio.test._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._

/**
 * Golden snapshots for the non-`generate` subcommands. The SPARQL emitted by
 * `query`, the IRI list dumped by `terms`, the prototype OFN, and the Markdown
 * doc page are all checked against goldens generated from the pre-refactor
 * `faddaf8` build via the matching CLI subcommand. SPARQL is compared by
 * parsed structure (whitespace is incidental); terms output is compared as a
 * set of lines (the underlying collection is unordered); prototype and docs
 * are compared exactly (OFN via axiom-set equality, Markdown byte-for-byte).
 */
object SubcommandGoldenTest extends DefaultRunnableSpec {

  JenaSystem.init()

  private val R = "src/test/resources/org/monarchinitiative/dosdp"

  private def commonFor(template: String, outfile: String): CommonOptions =
    CommonOptions(
      ontology = None,
      catalog = None,
      template = template,
      prefixes = None,
      oboPrefixes = Config.TrueValue,
      outfile = outfile,
      tableFormat = "tsv",
      batchPatterns = Config.MultiArgList(Nil),
      verbose = false
    )

  private def tempFile(prefix: String, suffix: String): String = {
    val p = Files.createTempFile(prefix, suffix)
    p.toFile.deleteOnExit()
    p.toString
  }

  private def queryTest(name: String) = testM(s"query: $name") {
    for {
      dosdp <- Config.inputDOSDPFrom(s"$R/$name.yaml")
      sparql <- Query.makeProcessedQuery(dosdp, OBOPrefixes, Config.LogicalAxioms, None)
    } yield Harness.assertSPARQLMatchesGolden(sparql, s"$R/$name.query.golden.rq")
  }

  private def termsTest(name: String) = testM(s"terms: $name") {
    val out = tempFile("terms-", ".txt")
    val cfg = TermsConfig(commonFor(s"$R/$name.yaml", out), infile = s"$R/$name.tsv")
    for {
      _ <- Terms.run(cfg)
      lines <- ZIO.effect(Files.readAllLines(Paths.get(out)).asScala.toList)
    } yield Harness.assertLineSetMatchesGolden(lines, s"$R/$name.terms.golden.txt")
  }

  private def prototypeTest(name: String) = testM(s"prototype: $name") {
    val out = tempFile("proto-", ".ofn")
    val cfg = PrototypeConfig(commonFor(s"$R/$name.yaml", out))
    for {
      _ <- Prototype.run(cfg)
      ofn <- ZIO.effect(new String(Files.readAllBytes(Paths.get(out)), StandardCharsets.UTF_8))
      axioms = Harness.parse(ofn)
    } yield Harness.assertMatchesGolden(axioms, s"$R/$name.prototype.golden.ofn") &&
      Harness.assertNoPlaceholderIRIs(axioms) &&
      Harness.assertNoPlaceholderLiterals(axioms)
  }

  private def docsTest(name: String) = testM(s"docs: $name") {
    val out = tempFile("docs-", ".md")
    val cfg = DocsConfig(commonFor(s"$R/$name.yaml", out), infile = s"$R/$name.tsv")
    for {
      _ <- Docs.run(cfg)
      md <- ZIO.effect(new String(Files.readAllBytes(Paths.get(out)), StandardCharsets.UTF_8))
    } yield Harness.assertTextMatchesGolden(md, s"$R/$name.docs.golden.md")
  }

  // Query coverage is currently narrow: only fixtures whose Manchester
  // templates compile cleanly under faddaf8's placeholder-form parser
  // (no GCI, no logical_axioms with multi_clause, no annotated subClassOf,
  // no data_vars in expressions) can be baselined against faddaf8.
  // Most new complex fixtures fall outside that envelope. Extending query
  // coverage either anchors on refactoring-3 or waits on a richer baseline.
  def spec = suite("Subcommand golden snapshots")(
    queryTest("OverrideTest"),
    queryTest("test_blank_lines"),
    termsTest("axiom_kinds"),
    termsTest("multi_clause_sub"),
    prototypeTest("annotated_axioms"),
    prototypeTest("axiom_kinds"),
    docsTest("annotated_axioms")
  ).provideCustomLayer(Logging.consoleErr())

}
