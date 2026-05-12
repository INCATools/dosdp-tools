package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.Generate
import org.phenoscape.scowl.{not => _, _}
import org.semanticweb.owlapi.model.{OWLAnnotationAssertionAxiom, OWLAxiom, OWLClass}
import zio.test.Assertion._
import zio.test._
import zio.logging._

// Pins the substitutions (regex sub) pipeline applied to annotation bindings.
// RegexTest already covers ExpandedRegexSub.substitute as a unit; this fills the
// gap on the Generate.renderPattern integration: a `substitutions` entry reads a
// binding from `in:`, runs the regex/sub, and exposes the result under `out:` so
// that downstream annotation templates can reference it. A non-matching value
// must pass through unchanged.
object SubstitutionsTest extends DefaultRunnableSpec {

  private val term: OWLClass = Class("http://purl.obolibrary.org/obo/ONT_0000001")

  private val dosdp: DOSDP = DOSDP.empty.copy(
    pattern_name = Some("substitutions_test"),
    classes = Some(Map("thing" -> "owl:Thing")),
    vars = Some(Map("regulated_activity" -> "'thing'")),
    substitutions = Some(List(
      RegexSub(in = "regulated_activity", out = "regulated_activity_munged",
        `match` = "(.+) activity", sub = raw"\1")
    )),
    name = Some(PrintfAnnotationOBO(
      annotations = None,
      xrefs = None,
      text = Some("regulation of %s"),
      vars = Some(List("regulated_activity_munged"))))
  )

  def spec = suite("substitutions (regex sub) integration")(
    testM("regex sub on a var binding feeds a downstream annotation template") {
      // The ontology supplies a label for the filler so that the regex runs on the
      // label "kinase activity" rather than on the raw IRI.
      for {
        ontology <- Utilities.loadOntology(
          "src/test/resources/org/monarchinitiative/dosdp/substitutions_test.ofn", None)
        axioms <- Generate.renderPattern(dosdp, OBOPrefixes,
          List(Map("defined_class" -> "ONT:0000001", "regulated_activity" -> "ONT:0000100")),
          Some(ontology),
          outputLogicalAxioms = false, outputAnnotationAxioms = true, None,
          annotateAxiomSource = false, AxiomRestrictionsTest.OboInOwlSource,
          generateDefinedClass = false, Map.empty)
        expected: OWLAnnotationAssertionAxiom = term Annotation(RDFSLabel, "regulation of kinase")
      } yield assert(axioms)(contains[OWLAxiom](expected))
    },
    testM("non-matching value passes through unchanged into the downstream template") {
      // The label "binding" does not match "(.+) activity"; substitute returns the input,
      // so the annotation renders with the original label.
      for {
        ontology <- Utilities.loadOntology(
          "src/test/resources/org/monarchinitiative/dosdp/substitutions_test.ofn", None)
        axioms <- Generate.renderPattern(dosdp, OBOPrefixes,
          List(Map("defined_class" -> "ONT:0000001", "regulated_activity" -> "ONT:0000200")),
          Some(ontology),
          outputLogicalAxioms = false, outputAnnotationAxioms = true, None,
          annotateAxiomSource = false, AxiomRestrictionsTest.OboInOwlSource,
          generateDefinedClass = false, Map.empty)
        expected: OWLAnnotationAssertionAxiom = term Annotation(RDFSLabel, "regulation of binding")
      } yield assert(axioms)(contains[OWLAxiom](expected))
    }
  ).provideCustomLayer(Logging.consoleErr())

}