package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.Config.AllAxioms
import org.monarchinitiative.dosdp.cli.{Config, DOSDPError}
import org.semanticweb.owlapi.model.OWLAxiom
import zio.ZIO
import zio.blocking.Blocking
import zio.logging._
import zio.test._

/**
 * Pins the exact axiom set produced by `Expansion.placeholderAxioms` (driven
 * by `Expansion.expandRow` on a synthetic placeholder row) for a spread of
 * fixtures covering the axiom kinds, list-var folds, multi-clause nesting,
 * data-var slots, axiom + sub-annotations, and permutations.
 *
 * The SPARQL and `terms` goldens in `SubcommandGoldenTest` provide the
 * end-to-end byte-equivalence proof; this suite localizes regressions to a
 * fixture and a structural shape.
 */
object PlaceholderRowTest extends DefaultRunnableSpec {

  private val resourceDir = "src/test/resources/org/monarchinitiative/dosdp"

  private def loadAndCompile(name: String): ZIO[Blocking with Logging, DOSDPError, CompiledPattern] =
    for {
      dosdp    <- Config.inputDOSDPFrom(s"$resourceDir/$name.yaml")
      compiled <- PatternCompiler.compile(dosdp, OBOPrefixes)
    } yield compiled

  private def placeholderTest(name: String) =
    testM(name) {
      for {
        compiled <- loadAndCompile(name)
      } yield {
        val produced: Set[OWLAxiom] = Expansion.placeholderAxioms(compiled, AllAxioms)
        Harness.assertMatchesGolden(produced, s"$resourceDir/$name.placeholder.golden.ofn")
      }
    }

  def spec = suite("Expansion.placeholderAxioms")(
    placeholderTest("axiom_kinds"),
    placeholderTest("list_var_logical"),
    placeholderTest("multi_clause_sub"),
    placeholderTest("data_var_slots"),
    placeholderTest("annotated_axioms"),
    placeholderTest("permutation_test"),
    placeholderTest("list_annotations")
  ).provideCustomLayer(Logging.consoleErr())

}
