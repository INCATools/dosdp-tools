package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.Generate.RowBindings
import org.monarchinitiative.dosdp.cli.{Config, DOSDPError, Generate}
import org.semanticweb.owlapi.model.{IRI, OWLAxiom}
import zio.ZIO
import zio.blocking.Blocking
import zio.logging._
import zio.test.Assertion._
import zio.test._

import scala.jdk.CollectionConverters._

/**
 * Pins the behavior of `RowBindings.placeholder` + `Expansion.expandRow`:
 * feeding the synthetic placeholder row through `expandRow` produces axioms
 * structurally equivalent to the legacy `ExpandedDOSDP.filledLogicalAxioms`
 * path that `Query` and `Terms` consume today.
 *
 * Equivalence asserted here is signature-level (every named entity present
 * in legacy logical axioms is also present in placeholder-row output) plus a
 * snapshot golden so the exact axiom set is locked. The downstream SPARQL /
 * Terms golden suite (`SubcommandGoldenTest`) will pin byte-equivalence at
 * the caller layer once those callers are switched (Phase 3.0.2).
 */
object PlaceholderRowTest extends DefaultRunnableSpec {

  private val resourceDir = "src/test/resources/org/monarchinitiative/dosdp"

  private def loadAndCompile(name: String): ZIO[Blocking with Logging, DOSDPError, (DOSDP, CompiledPattern)] =
    for {
      dosdp    <- Config.inputDOSDPFrom(s"$resourceDir/$name.yaml")
      compiled <- PatternCompiler.compile(dosdp, OBOPrefixes)
    } yield (dosdp, compiled)

  private def expandPlaceholder(dosdp: DOSDP, compiled: CompiledPattern): Set[OWLAxiom] = {
    val context = Expansion.ExpansionContext(
      readableIDIndex = Map.empty,
      permutationIndex = Map.empty,
      outputLogicalAxioms = true,
      outputAnnotationAxioms = true,
      restrictAxiomsColumn = None,
      generateDefinedClass = false,
      readableIdentifiers = compiled.readableIdentifierProperties,
      localLabelProperty = Generate.LocalLabelProperty)
    Expansion.expandRow(compiled, RowBindings.placeholder(dosdp), RowBindings.placeholderRow, context)
      .fold(err => throw new AssertionError(s"placeholder expansion failed: ${err.message}"), identity)
  }

  private def signature(axioms: Set[OWLAxiom]): Set[IRI] =
    axioms.flatMap(_.getSignature.asScala.map(_.getIRI))

  private def placeholderTest(name: String) =
    testM(name) {
      for {
        pair <- loadAndCompile(name)
        (dosdp, compiled) = pair
      } yield {
        val produced = expandPlaceholder(dosdp, compiled)
        val edosdp = ExpandedDOSDP(dosdp, OBOPrefixes, compiled)
        val legacyLogical: Set[OWLAxiom] = edosdp.filledLogicalAxioms
        // Terms.run filters out `urn:dosdp:` entries; the non-placeholder
        // signature is what Terms emits, so the equivalence Terms cares about
        // is exactly this subset.
        val legacySig = signature(legacyLogical).filterNot(_.toString.startsWith(DOSDP.variablePrefix))
        val producedSig = signature(produced).filterNot(_.toString.startsWith(DOSDP.variablePrefix))
        Harness.assertMatchesGolden(produced, s"$resourceDir/$name.placeholder.golden.ofn") &&
          assert(legacySig diff producedSig)(isEmpty ?? s"$name: every non-placeholder IRI in filledLogicalAxioms also appears in placeholder-row expansion")
      }
    }

  def spec = suite("RowBindings.placeholder + Expansion.expandRow")(
    placeholderTest("axiom_kinds"),
    placeholderTest("list_var_logical"),
    placeholderTest("multi_clause_sub"),
    placeholderTest("data_var_slots"),
    placeholderTest("annotated_axioms"),
    placeholderTest("permutation_test")
  ).provideCustomLayer(Logging.consoleErr())

}
