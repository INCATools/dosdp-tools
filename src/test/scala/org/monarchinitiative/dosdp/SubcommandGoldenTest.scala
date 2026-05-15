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

  // Query coverage on the complex fixtures (axiom_kinds, list_var_logical,
  // annotated_axioms) is anchored on refactoring-3, not on faddaf8: the legacy
  // placeholder-form Manchester parser errors on GCI, multi_clause-on-logical,
  // annotated subClassOf, and list_vars in expressions, so faddaf8 has nothing
  // to baseline against. These goldens lock the current behavior and will be
  // re-verified end-to-end once Phase 3.0 collapses the two expansion paths.
  //
  // Two complex fixtures are intentionally absent from query coverage:
  //
  //   * `data_var_slots` — `SPARQL.triplesForClassExpression` has no case for
  //     `OWLDataHasValue` / `OWLDataSomeValuesFrom` / data cardinalities, so
  //     the query generator MatchErrors on data_var-bearing templates. That
  //     gap pre-dates the refactor — the refactor just exposed it by letting
  //     these templates compile. Fixing it is out of scope for Phase 0.3.
  //
  //   * `multi_clause_sub` — nested intersection/union operand ordering comes
  //     from OWL API `Set` iteration, which isn't stable across runs. The
  //     SPARQL is semantically equivalent (constraints are unordered) but the
  //     UUID-normalized text golden flakes when operands swap. Stabilizing
  //     this would mean sorting operands in `SPARQL.triplesForClassExpression`
  //     — a user-visible output-ordering change reserved for Phase 3.0.
  def spec = suite("Subcommand golden snapshots")(
    queryTest("OverrideTest"),
    queryTest("test_blank_lines"),
    queryTest("axiom_kinds"),
    queryTest("list_var_logical"),
    queryTest("annotated_axioms"),
    termsTest("axiom_kinds"),
    termsTest("multi_clause_sub"),
    prototypeTest("annotated_axioms"),
    prototypeTest("axiom_kinds"),
    docsTest("annotated_axioms")
  ).provideCustomLayer(Logging.consoleErr())

}
