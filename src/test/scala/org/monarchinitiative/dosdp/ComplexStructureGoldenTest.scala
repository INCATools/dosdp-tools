package org.monarchinitiative.dosdp

import com.github.tototoshi.csv.TSVFormat
import org.monarchinitiative.dosdp.cli.{Config, DOSDPError, Generate}
import org.semanticweb.owlapi.model.OWLAxiom
import zio.test._

import java.io.File

/**
 * Golden snapshots for complex pattern structures: multi_clause with nested
 * sub_clauses, data_var fillers in assorted Manchester slots, the four axiom
 * kinds in one pattern, list_var folding into intersections/unions, and
 * axiom/sub annotations.
 *
 * Goldens were generated from the pre-refactor `faddaf8` build via its
 * `generate` CLI, so a passing test means the refactor preserved behavior for
 * that structure. See `Harness` for the set-equality comparison.
 */
object ComplexStructureGoldenTest extends ZIOSpecDefault {

  private val resourceDir = "src/test/resources/org/monarchinitiative/dosdp"

  private def renderFixture(name: String): zio.IO[DOSDPError, Set[OWLAxiom]] =
    for {
      dosdp <- Config.inputDOSDPFrom(s"$resourceDir/$name.yaml")
      columnsAndFillers <- Generate.readFillers(new File(s"$resourceDir/$name.tsv"), new TSVFormat {})
      (_, fillers) = columnsAndFillers
      axioms <- Generate.renderPattern(dosdp, OBOPrefixes, fillers, None, true, true, None, false, AxiomRestrictionsTest.OboInOwlSource, false, Map.empty)
    } yield axioms

  private def goldenTest(name: String) =
    test(name) {
      renderFixture(name).map { axioms =>
        Harness.assertMatchesGolden(axioms, s"$resourceDir/$name.golden.ofn") &&
          Harness.assertNoPlaceholderIRIs(axioms) &&
          Harness.assertNoPlaceholderLiterals(axioms)
      }
    }

  def spec = suite("Complex structure golden snapshots")(
    // `multi_clause_sub` is the one fixture whose golden is NOT the faddaf8
    // baseline. With a parent clause separator of " and " and a nested
    // sub_clause separator of " or ", the legacy `replaceMultiClause` produced
    // a flat string `A and B or C` that Manchester precedence parsed as
    // `(A and B) or C` — the `or` escaped the sub_clause. The refactor nests
    // sub_clauses structurally, yielding `A and (B or C)`. This is intentional
    // (see `MultiClauseSubClausesTest` "nested operator differs from parent"
    // and the third-review-round fix in commit b24cee4); the golden captures
    // the corrected behavior.
    goldenTest("multi_clause_sub"),
    goldenTest("data_var_slots"),
    goldenTest("axiom_kinds"),
    goldenTest("list_var_logical"),
    goldenTest("annotated_axioms")
  )

}
