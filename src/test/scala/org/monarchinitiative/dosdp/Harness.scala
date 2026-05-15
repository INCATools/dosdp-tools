package org.monarchinitiative.dosdp

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.FunctionalSyntaxDocumentFormat
import org.semanticweb.owlapi.io.{StringDocumentSource, StringDocumentTarget}
import org.semanticweb.owlapi.model.{OWLAxiom, OWLDeclarationAxiom, OWLLiteral}
import org.semanticweb.owlapi.util.OWLObjectComponentCollector
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
   * Assert that no rendered axiom mentions a `urn:dosdp:` placeholder IRI. A regression
   * that leaks a placeholder fails this at every test site exercising the broken path,
   * not only the one asserting on the specific axiom.
   */
  def assertNoPlaceholderIRIs(axioms: Set[OWLAxiom]): TestResult = {
    val leaked = axioms
      .flatMap(_.getSignature.asScala)
      .map(_.getIRI.toString)
      .filter(_.startsWith(DOSDP.variablePrefix))
    assert(leaked)(isEmpty ?? "no urn:dosdp: placeholder IRIs in rendered output")
  }

  /**
   * Assert that no rendered literal has a placeholder lexical form (`$name` or `'$name'`).
   * Mirrors `assertNoPlaceholderIRIs` for the data-var substitution path.
   */
  def assertNoPlaceholderLiterals(axioms: Set[OWLAxiom]): TestResult = {
    val leaked = collectLiterals(axioms)
      .map(_.getLiteral)
      .filter(lex => lex.startsWith("$") || (lex.startsWith("'$") && lex.endsWith("'")))
    assert(leaked)(isEmpty ?? "no $placeholder literals in rendered output")
  }

  private def collectLiterals(axioms: Set[OWLAxiom]): Set[OWLLiteral] = {
    val collector = new OWLObjectComponentCollector()
    axioms.flatMap(ax => collector.getComponents(ax).asScala).collect { case l: OWLLiteral => l }
  }

}
