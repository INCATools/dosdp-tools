package org.monarchinitiative.dosdp

import org.apache.jena.query.QueryFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.FunctionalSyntaxDocumentFormat
import org.semanticweb.owlapi.io.{StringDocumentSource, StringDocumentTarget}
import org.semanticweb.owlapi.model.{OWLAxiom, OWLDeclarationAxiom}
import zio.test.Assertion._
import zio.test._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._

/**
 * Shared helpers for the behavior-preservation harness (REFACTOR_PLAN.md Phase 0.3).
 *
 * Golden comparison is by OWL axiom-set equality, not raw bytes: functional-syntax
 * serialization does not guarantee a stable axiom order, so a byte diff would be
 * flaky. Serialization is still used to write/refresh golden files and to render a
 * human-readable diff when a comparison fails.
 *
 * Start here: `assertMatchesGolden` for whole-output snapshots, `assertNoPlaceholderIRIs`
 * / `assertNoPlaceholderLiterals` for the invariant that no `urn:dosdp:` placeholder
 * survives into rendered output.
 */
object Harness {

  /**
   * Set the `DOSDP_UPDATE_GOLDEN` environment variable when running the suite to
   * (re)write every golden file from current output instead of asserting against it.
   * A missing golden file is always written rather than failing, so a brand-new
   * fixture locks its golden on first run.
   */
  private val UpdateGoldenEnvVar = "DOSDP_UPDATE_GOLDEN"

  /** Serialize axioms to OWL functional syntax — used to write and refresh golden files. */
  def serialize(axioms: Set[OWLAxiom]): String = {
    val manager = OWLManager.createOWLOntologyManager()
    val ont = manager.createOntology(axioms.asJava)
    val target = new StringDocumentTarget()
    manager.saveOntology(ont, new FunctionalSyntaxDocumentFormat(), target)
    target.toString
  }

  /** Parse a functional-syntax document back into its axiom set. */
  def parse(ofn: String): Set[OWLAxiom] = {
    val manager = OWLManager.createOWLOntologyManager()
    val ont = manager.loadOntologyFromOntologyDocument(new StringDocumentSource(ofn))
    ont.getAxioms().asScala.toSet
  }

  // Functional-syntax serialization injects Declaration axioms for every entity it
  // mentions, whether or not the input set contained them. renderPattern does not
  // emit declarations, so they are dropped from both sides before comparison to
  // avoid spurious diffs on the round trip.
  private def withoutDeclarations(axioms: Set[OWLAxiom]): Set[OWLAxiom] =
    axioms.filterNot(_.isInstanceOf[OWLDeclarationAxiom])

  /**
   * Assert that `actual` equals the axiom set stored in the golden file at `goldenPath`.
   * If the golden file is missing, or `DOSDP_UPDATE_GOLDEN` is set, the file is written
   * from `actual` and the assertion passes. On mismatch the failure message lists the
   * axioms present on only one side.
   */
  def assertMatchesGolden(actual: Set[OWLAxiom], goldenPath: String): TestResult = {
    val path = Paths.get(goldenPath)
    if (sys.env.contains(UpdateGoldenEnvVar) || !Files.exists(path)) {
      Files.write(path, serialize(actual).getBytes(StandardCharsets.UTF_8))
      assertCompletes
    } else {
      val expected = withoutDeclarations(parse(new String(Files.readAllBytes(path), StandardCharsets.UTF_8)))
      val produced = withoutDeclarations(actual)
      val onlyExpected = expected diff produced
      val onlyProduced = produced diff expected
      val summary =
        if (onlyExpected.isEmpty && onlyProduced.isEmpty) ""
        else
          s"""|
              |${onlyExpected.size} axiom(s) in golden but not produced:
              |${onlyExpected.toSeq.map(_.toString).sorted.map("  - " + _).mkString("\n")}
              |${onlyProduced.size} axiom(s) produced but not in golden:
              |${onlyProduced.toSeq.map(_.toString).sorted.map("  + " + _).mkString("\n")}
              |""".stripMargin
      assert(summary)(isEmptyString ?? s"axiom set matches golden file $goldenPath")
    }
  }

  /**
   * Every `urn:dosdp:` IRI present anywhere in the axiom tree — entity IRIs as
   * well as raw `IRI`s used as annotation-assertion subjects or IRI-valued
   * annotations. `OWLAxiom.getSignature` covers only named entities, and the
   * OWL API's component-collector doesn't reliably expose bare-IRI annotation
   * values either; scanning the canonical functional-syntax serialization
   * enumerates every IRI by construction and closes both gaps.
   */
  def placeholderIRIs(axioms: Set[OWLAxiom]): Set[String] =
    PlaceholderIRIPattern.findAllIn(serialize(axioms)).toSet

  /**
   * All literal lexical forms with a placeholder shape (`$name` or `'$name'`).
   * Functional syntax writes each literal as `"<lexical>"^^<datatype>`, so a
   * regex over the serialization catches placeholders the OWL API leaf-level
   * walker can miss.
   */
  def placeholderLiterals(axioms: Set[OWLAxiom]): Set[String] =
    PlaceholderLiteralPattern.findAllMatchIn(serialize(axioms)).map(_.group(1)).toSet

  private val PlaceholderIRIPattern = s"""${java.util.regex.Pattern.quote(DOSDP.variablePrefix)}[^>"\\s]+""".r
  private val PlaceholderLiteralPattern = """"('?\$[^"]*)"""".r

  /**
   * Assert that no rendered axiom mentions a `urn:dosdp:` placeholder IRI. A regression
   * that leaks a placeholder fails this at every test site exercising the broken path,
   * not only the one asserting on the specific axiom.
   */
  def assertNoPlaceholderIRIs(axioms: Set[OWLAxiom]): TestResult =
    assert(placeholderIRIs(axioms))(isEmpty ?? "no urn:dosdp: placeholder IRIs in rendered output")

  /**
   * Assert that no rendered literal has a placeholder lexical form (`$name` or `'$name'`).
   * Mirrors `assertNoPlaceholderIRIs` for the data-var substitution path.
   */
  def assertNoPlaceholderLiterals(axioms: Set[OWLAxiom]): TestResult =
    assert(placeholderLiterals(axioms))(isEmpty ?? "no $placeholder literals in rendered output")

  /**
   * Assert that `actual` parses to a SPARQL query structurally equal to the one
   * stored at `goldenPath`. Jena's `Query.equals` compares parsed structure, so
   * differences in whitespace or prefix ordering do not cause spurious failures.
   */
  def assertSPARQLMatchesGolden(actual: String, goldenPath: String): TestResult = {
    val path = Paths.get(goldenPath)
    if (sys.env.contains(UpdateGoldenEnvVar) || !Files.exists(path)) {
      Files.write(path, actual.getBytes(StandardCharsets.UTF_8))
      assertCompletes
    } else {
      val expectedText = new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
      val expected = QueryFactory.create(expectedText)
      val produced = QueryFactory.create(actual)
      val summary =
        if (expected == produced) ""
        else
          s"""|
              |golden SPARQL ($goldenPath):
              |$expectedText
              |produced SPARQL:
              |$actual
              |""".stripMargin
      assert(summary)(isEmptyString ?? s"SPARQL matches golden file $goldenPath")
    }
  }

  /** Byte-exact text comparison against a golden file. */
  def assertTextMatchesGolden(actual: String, goldenPath: String): TestResult = {
    val path = Paths.get(goldenPath)
    if (sys.env.contains(UpdateGoldenEnvVar) || !Files.exists(path)) {
      Files.write(path, actual.getBytes(StandardCharsets.UTF_8))
      assertCompletes
    } else {
      val expected = new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
      assert(actual)(equalTo(expected) ?? s"text matches golden file $goldenPath")
    }
  }

  /**
   * Set-equality of newline-separated lines (empty lines ignored). Used where
   * the order in which the producer emits lines is not part of the contract
   * (e.g. `terms` output, which is an unordered set of IRIs).
   */
  def assertLineSetMatchesGolden(actualLines: Iterable[String], goldenPath: String): TestResult = {
    val path = Paths.get(goldenPath)
    if (sys.env.contains(UpdateGoldenEnvVar) || !Files.exists(path)) {
      Files.write(path, actualLines.filter(_.nonEmpty).toSeq.sorted.mkString("\n").getBytes(StandardCharsets.UTF_8))
      assertCompletes
    } else {
      val expected = new String(Files.readAllBytes(path), StandardCharsets.UTF_8).linesIterator.filter(_.nonEmpty).toSet
      val produced = actualLines.filter(_.nonEmpty).toSet
      val onlyExpected = expected diff produced
      val onlyProduced = produced diff expected
      val summary =
        if (onlyExpected.isEmpty && onlyProduced.isEmpty) ""
        else
          s"""|
              |${onlyExpected.size} line(s) in golden but not produced:
              |${onlyExpected.toSeq.sorted.map("  - " + _).mkString("\n")}
              |${onlyProduced.size} line(s) produced but not in golden:
              |${onlyProduced.toSeq.sorted.map("  + " + _).mkString("\n")}
              |""".stripMargin
      assert(summary)(isEmptyString ?? s"line set matches golden file $goldenPath")
    }
  }

}
