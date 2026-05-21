package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.DOSDPError
import org.phenoscape.scowl._
import zio.test.Assertion._
import zio.test._

object PatternCompilerTest extends ZIOSpecDefault {

  private val labelAnnotated: DOSDP = DOSDP.empty.copy(
    pattern_name = Some("trivial"),
    pattern_iri = Some("http://example.org/pattern/trivial"),
    classes = Some(Map("thing" -> "owl:Thing")),
    vars = Some(Map("x" -> "'thing'")),
    name = Some(PrintfAnnotationOBO(
      annotations = None,
      xrefs = None,
      text = Some("name of %s"),
      vars = Some(List("x")))))

  def spec = suite("PatternCompiler")(
    test("compiles a minimal pattern with an OBO name annotation") {
      for {
        compiled <- PatternCompiler.compile(labelAnnotated, OBOPrefixes)
      } yield {
        assert(compiled.source)(equalTo(labelAnnotated)) &&
          assert(compiled.patternIRI.map(_.toString))(isSome(equalTo("http://example.org/pattern/trivial"))) &&
          assert(compiled.oboAnnotations.exists {
            case NormalizedPrintfAnnotation(prop, _, _, _, _, _, _) => prop == PrintfAnnotationOBO.Name
            case _                                                  => false
          })(isTrue) &&
          assert(compiled.readableIdentifierProperties)(equalTo(List(RDFSLabel))) &&
          assert(compiled.dataVarNames)(isEmpty)
      }
    },
    test("rejects a custom annotation that names an undeclared property") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("bad-custom-ann"),
        annotations = Some(List(PrintfAnnotation(
          annotations = None,
          annotationProperty = "made_up_property",
          text = Some("x"),
          vars = None,
          `override` = None))))
      compileError(pattern).map(msg => assert(msg)(containsString("made_up_property")))
    },
    test("rejects a permutation that names an undeclared property") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("bad-permutation-prop"),
        vars = Some(Map("x" -> "owl:Thing")),
        name = Some(PrintfAnnotationOBO(
          annotations = None,
          xrefs = None,
          text = Some("%s"),
          vars = Some(List("x")),
          permutations = Some(List(Permutation("x", List("made_up_property")))))))
      compileError(pattern).map(msg => assert(msg)(containsString("made_up_property")))
    },
    test("rejects a permutation whose var is not in the annotation's vars list") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("bad-permutation-var"),
        annotationProperties = Some(Map("exact_synonym" -> "http://www.geneontology.org/formats/oboInOwl#hasExactSynonym")),
        vars = Some(Map("x" -> "owl:Thing", "y" -> "owl:Thing")),
        name = Some(PrintfAnnotationOBO(
          annotations = None,
          xrefs = None,
          text = Some("%s"),
          vars = Some(List("x")),
          permutations = Some(List(Permutation("y", List("exact_synonym")))))))
      compileError(pattern).map(msg => assert(msg)(containsString("Permutation vars not found")))
    },
    test("rejects a readable_identifiers entry that names an undeclared property") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("bad-readable-id"),
        readable_identifiers = Some(List("made_up_property")))
      compileError(pattern).map(msg => assert(msg)(containsString("made_up_property")))
    },
    test("populates dataVarNames from data_vars and data_list_vars") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("data-vars"),
        data_vars = Some(Map("rate_min" -> "xsd:short")),
        data_list_vars = Some(Map("rates" -> "xsd:short")))
      for {
        compiled <- PatternCompiler.compile(pattern, OBOPrefixes)
      } yield assert(compiled.dataVarNames)(equalTo(Set("rate_min", "rates")))
    },
    test("rejects declared variable names outside letters, numbers, and underscores") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("bad-var-name"),
        vars = Some(Map("cell-type" -> "owl:Thing")),
        data_list_vars = Some(Map("xref.value" -> "xsd:string")))
      compileError(pattern).map(msg =>
        assert(msg)(containsString("cell-type")) &&
          assert(msg)(containsString("xref.value")) &&
          assert(msg)(containsString("letters, numbers, and underscores")))
    },
    test("rejects undeclared template variable names outside letters, numbers, and underscores") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("bad-template-var-name"),
        classes = Some(Map("thing" -> "owl:Thing")),
        name = Some(PrintfAnnotationOBO(
          annotations = None,
          xrefs = None,
          text = Some("name of %s"),
          vars = Some(List("cell type")))))
      compileError(pattern).map(msg => assert(msg)(containsString("cell type")))
    },
    test("rejects bad name in a permutation var") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("bad-permutation-var-name"),
        annotationProperties = Some(Map("exact_synonym" -> "http://www.geneontology.org/formats/oboInOwl#hasExactSynonym")),
        vars = Some(Map("x" -> "owl:Thing")),
        name = Some(PrintfAnnotationOBO(
          annotations = None,
          xrefs = None,
          text = Some("%s"),
          vars = Some(List("x")),
          permutations = Some(List(Permutation("bad-var", List("exact_synonym")))))))
      compileError(pattern).map(msg => assert(msg)(containsString("bad-var")))
    },
    test("rejects bad name in an internal_vars input reference") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("bad-internal-input"),
        data_list_vars = Some(Map("items" -> "xsd:string")),
        internal_vars = Some(List(InternalVariable(
          var_name = "joined",
          apply = Some(JoinFunction(Join(","))),
          input = "bad input"))))
      compileError(pattern).map(msg => assert(msg)(containsString("bad input")))
    },
    test("rejects bad name nested inside a multi_clause sub_clause") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("bad-sub-clause-var"),
        classes = Some(Map("thing" -> "owl:Thing")),
        vars = Some(Map("x" -> "'thing'")),
        equivalentTo = Some(PrintfOWLConvenience(
          annotations = None,
          text = None,
          vars = None,
          multi_clause = Some(MultiClausePrintf(
            sep = Some(" and "),
            clauses = Some(List(PrintfClause(
              text = "%s",
              vars = Some(List("x")),
              sub_clauses = Some(List(MultiClausePrintf(
                sep = Some(" or "),
                clauses = Some(List(PrintfClause(
                  text = "%s",
                  vars = Some(List("nested.bad")),
                  sub_clauses = None))))))))))))))
      compileError(pattern).map(msg => assert(msg)(containsString("nested.bad")))
    },
    test("rejects bad name in an IRIValueAnnotation var") {
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("bad-iri-annotation-var"),
        annotationProperties = Some(Map("seeAlso" -> "rdfs:seeAlso")),
        vars = Some(Map("x" -> "owl:Thing")),
        annotations = Some(List(IRIValueAnnotation(
          annotations = None,
          annotationProperty = "seeAlso",
          `var` = "bad-iri-var"))))
      compileError(pattern).map(msg => assert(msg)(containsString("bad-iri-var")))
    },
    test("rejects a data_var in a cardinality slot (`min %s`) — known capability regression") {
      // Manchester requires a bare integer at the cardinality, but the compile-time
      // placeholder for a data_var is a typed-literal token `"$name"^^xsd:integer`,
      // which the parser does not accept in that position. Pinning this as an
      // explicit non-support: if a real pattern needs cardinality data_vars, the
      // fix is a row-time-parse fallback rather than relaxing this check.
      val pattern = DOSDP.empty.copy(
        pattern_name = Some("data-var-cardinality"),
        classes = Some(Map("thing" -> "owl:Thing")),
        dataProperties = Some(Map("has_age" -> "RO:0002000")),
        data_vars = Some(Map("count" -> "xsd:integer")),
        subClassOf = Some(PrintfOWLConvenience(None, Some("'has_age' min %s xsd:integer"), Some(List("count")))))
      compileError(pattern).map(msg => assert(msg)(containsString("Failed to parse class expression")))
    }
  )

  private def compileError(pattern: DOSDP) =
    PatternCompiler.compile(pattern, OBOPrefixes).either.map {
      case Left(err) => err.msg
      case Right(_)  => "compilation unexpectedly succeeded"
    }

}
