package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.Docs.DocData
import org.phenoscape.scowl._
import org.semanticweb.owlapi.io.OWLObjectRenderer
import org.semanticweb.owlapi.model.{IRI, OWLObject}

import scala.jdk.CollectionConverters._

object DocsMarkdown {

  def markdown(edosdp: ExpandedDOSDP, docData: DocData, renderer: OWLObjectRenderer, data: List[List[String]]): String = {
    def r(obj: OWLObject): String = renderer.render(obj).replace("\n", " ")

    val dosdp = edosdp.dosdp
    val PatternIRI = dosdp.pattern_iri.map(IRI.create).getOrElse(IRI.create("http://example.org/"))
    val PatternCls = Class(PatternIRI)
    val equivPrefix = if (docData.equivalentTo.flatMap(_.getClassExpressionsMinus(PatternCls).asScala).size > 1) "- " else ""
    val subClassOfPrefix = if (docData.subClassOf.size > 1) "- " else ""
    val variables = ((edosdp.varExpressions.to(List) ::: edosdp.listVarExpressions.to(List)).map { case (k, v) => k -> r(v) }) :::
      dosdp.data_vars.getOrElse(Map.empty).to(List) ::: dosdp.data_list_vars.getOrElse(Map.empty).to(List)

    s"""# ${dosdp.pattern_name.getOrElse("")}

[${dosdp.pattern_iri.getOrElse("Missing pattern IRI")}](${dosdp.pattern_iri.getOrElse("")})

## Description

${dosdp.description.getOrElse("*No description*")}

${if (dosdp.contributors.nonEmpty) "## Contributors\n" else ""}
${dosdp.contributors.getOrElse(Nil).map(c => s"- $c").mkString("\n")}

## Variables

| Variable name | Allowed type |
|:--------------|:-------------|
${variables.map { case (name, range) => s"| `{$name}` | $range |" }.mkString("\n")}

## Name

${docData.name.map { ax => r(ax.getValue) }.mkString("\n\n")}

## Annotations

${
      docData.annotations.map { ax =>
        s"- ${r(ax.getProperty)}: ${r(ax.getValue)}"
      }.mkString("\n")
    }

## Definition

${docData.definition.map { ax => r(ax.getValue) }.mkString("\n\n")}

## Equivalent to

${
      docData.equivalentTo.flatMap { ax =>
        ax.getClassExpressionsMinus(PatternCls).asScala.map(cls => r(cls)).map(text => s"$equivPrefix$text")
      }.mkString("\n")
    }

${if (docData.subClassOf.nonEmpty) "## Subclass of\n" else ""}
${docData.subClassOf.map(ax => r(ax.getSuperClass)).map(text => s"$subClassOfPrefix$text").mkString("\n")}

${if (docData.otherAxioms.nonEmpty) "## Other axioms\n" else ""}
${docData.otherAxioms.map(r).map(text => s"- $text").mkString("\n")}

## Data preview

*See full table [here](${docData.dataLocation})*

${data.head.mkString("| ", " | ", " |")}
${data.head.map(_ => "|:--").mkString}|
${data.drop(1).map(_.mkString("| ", " | ", " |")).mkString("\n")}

"""
  }

  def indexMarkdown(patterns: List[(DOSDP, String)]): String = {
    val sorted = patterns.sortBy(_._1.pattern_name)
    s"""# Design Patterns

| Pattern | Description |
|:--------|:------------|
${sorted.map { case (p, filename) => s"| [${p.pattern_name.getOrElse("*unnamed*")}]($filename) | ${p.description.getOrElse("*no description*")} |" }.mkString("\n")}
"""
  }

}
