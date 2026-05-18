package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.Config.{AnnotationAxioms, AxiomKind, LogicalAxioms}
import org.monarchinitiative.dosdp.cli.DOSDPError
import org.monarchinitiative.dosdp.cli.DOSDPError.logError
import org.monarchinitiative.dosdp.cli.Generate
import org.monarchinitiative.dosdp.cli.Generate.RowBindings
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntaxClassExpressionParser
import org.semanticweb.owlapi.model._
import zio._
import zio.logging._

import scala.util.matching.Regex.Match

/**
 * Thin wrapper that carries a `CompiledPattern` alongside the var-range
 * Manchester parser used by `SPARQL` and `Docs` to expand the bounds declared
 * on `vars` / `list_vars`. Placeholder-form axioms — what `Query` and `Terms`
 * consume — come from `placeholderAxioms`, which delegates to
 * `Expansion.expandRow` with a synthetic placeholder row so the single pure
 * expansion path owns OWL assembly for both row-time and template-time use.
 */
final case class ExpandedDOSDP(dosdp: DOSDP, prefixes: PartialFunction[String, String], compiled: CompiledPattern) {

  private lazy val checker = new DOSDPEntityChecker(dosdp, prefixes)
  private lazy val expressionParser = new ManchesterOWLSyntaxClassExpressionParser(OWLManager.getOWLDataFactory, checker)

  /**
   * Placeholder-form axioms for this pattern: every variable slot left as
   * its `urn:dosdp:` IRI (class/list) or `$<name>` literal (data). Driven by
   * `Expansion.expandRow` on the synthetic placeholder row so both row-time
   * and template-time paths share one assembly.
   */
  def placeholderAxioms(kinds: AxiomKind): Set[OWLAxiom] = {
    val (logical, annotations) = Generate.axiomsOutputChoice(kinds)
    val context = Expansion.ExpansionContext(
      readableIDIndex = Map.empty,
      permutationIndex = Map.empty,
      outputLogicalAxioms = logical,
      outputAnnotationAxioms = annotations,
      restrictAxiomsColumn = None,
      generateDefinedClass = false,
      readableIdentifiers = compiled.readableIdentifierProperties,
      localLabelProperty = Generate.LocalLabelProperty)
    // The placeholder row binds every slot the compiled pattern references,
    // so `expandRow` cannot produce a binding-resolution error here — a
    // `Left` indicates a bug in slot collection or in expansion.
    Expansion.expandRow(compiled, RowBindings.placeholder(compiled), RowBindings.placeholderRow, context)
      .fold(err => throw new IllegalStateException(s"Placeholder-row expansion failed: ${err.message}"), identity)
  }

  def varExpressions: ZIO[Logging, DOSDPError, Map[String, OWLClassExpression]] =
    ZIO.foreach(dosdp.vars.getOrElse(Map.empty)) { case (k, v) =>
      ZIO.effect(expressionParser.parse(v))
        .flatMapError(e => logError(s"Failed to parse class expression: $v", e))
        .map(k -> _)
    }

  def listVarExpressions: ZIO[Logging, DOSDPError, Map[String, OWLClassExpression]] =
    ZIO.foreach(dosdp.list_vars.getOrElse(Map.empty)) { case (k, v) =>
      ZIO.effect(expressionParser.parse(v))
        .flatMapError(e => logError(s"Failed to parse class expression: $v", e))
        .map(k -> _)
    }

}

final case class ExpandedRegexSub(regexSub: RegexSub) {

  private val groupFinder = raw"\\(\d+)".r

  private val regex = regexSub.`match`.r

  /** Apply `regexSub` to a value; if the pattern does not match, return the value unchanged. */
  def substitute(value: String): String =
    regex.findFirstMatchIn(value).map { valueMatch =>
      groupFinder.replaceAllIn(regexSub.sub, (placeholder: Match) => valueMatch.group(placeholder.group(1).toInt))
    }.getOrElse(value)

  def expandBindings(bindings: Map[String, Binding]): Map[String, Binding] =
    bindings.get(regexSub.in).map {
      case SingleValue(value) => regexSub.out -> SingleValue(substitute(value))
      case MultiValue(values) => regexSub.out -> MultiValue(values.map(substitute))
    }.fold(bindings)(bindings + _)

}
