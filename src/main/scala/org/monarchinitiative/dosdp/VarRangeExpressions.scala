package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.DOSDPError
import org.monarchinitiative.dosdp.cli.DOSDPError.logError
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntaxClassExpressionParser
import org.semanticweb.owlapi.model.OWLClassExpression
import zio._
import zio.logging.Logging

/**
 * Parses the Manchester range expressions declared on `dosdp.vars` and
 * `dosdp.list_vars`. These describe the allowed types for each variable
 * (e.g. `'thing'`, `'cell type' or 'organism'`) and feed `SPARQL`'s
 * variable-restriction triples and the `Docs` Markdown table.
 */
object VarRangeExpressions {

  def varExpressions(compiled: CompiledPattern): ZIO[Logging, DOSDPError, Map[String, OWLClassExpression]] =
    parse(compiled, compiled.source.vars.getOrElse(Map.empty))

  def listVarExpressions(compiled: CompiledPattern): ZIO[Logging, DOSDPError, Map[String, OWLClassExpression]] =
    parse(compiled, compiled.source.list_vars.getOrElse(Map.empty))

  private def parse(compiled: CompiledPattern, ranges: Map[String, String]): ZIO[Logging, DOSDPError, Map[String, OWLClassExpression]] = {
    val checker = new DOSDPEntityChecker(compiled.source, compiled.prefixes)
    val parser = new ManchesterOWLSyntaxClassExpressionParser(OWLManager.getOWLDataFactory, checker)
    ZIO.foreach(ranges) { case (name, expr) =>
      ZIO.effect(parser.parse(expr))
        .flatMapError(e => logError(s"Failed to parse class expression: $expr", e))
        .map(name -> _)
    }
  }

}
