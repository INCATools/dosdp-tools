package org.monarchinitiative.dosdp

import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntaxClassExpressionParser
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntaxInlineAxiomParser
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClassExpression

final case class DOSDP(
    pattern_name: String,
    classes: Map[String, String],
    relations: Map[String, String],
    vars: Map[String, String],
    name: PrintfText,
    `def`: PrintfText,
    equivalentTo: Option[PrintfText],
    subClassOf: Option[PrintfText],
    disjointWith: Option[PrintfText],
    GCI: Option[PrintfText]) {

  private lazy val checker = new DOSDPEntityChecker(this)
  private lazy val expressionParser = new ManchesterOWLSyntaxClassExpressionParser(OWLManager.getOWLDataFactory, checker)
  private lazy val axiomParser = new ManchesterOWLSyntaxInlineAxiomParser(OWLManager.getOWLDataFactory, checker)

  def equivalentToExpression: Option[OWLClassExpression] = expressionFor(this.equivalentTo)

  def subClassOfExpression: Option[OWLClassExpression] = expressionFor(this.subClassOf)

  def disjointWithExpression: Option[OWLClassExpression] = expressionFor(this.disjointWith)

  def gciAxiom: Option[OWLAxiom] = {
    GCI.map(gci => axiomParser.parse(gci.replaced))
  }

  def axiomTemplates: Set[OWLAxiom] = {
    val term = Class(DOSDP.variableToIRI(pattern_name))
    equivalentToExpression.map(e => (term EquivalentTo e)).toSet ++
      subClassOfExpression.map(e => (term SubClassOf e)).toSet ++
      disjointWithExpression.map(e => (term DisjointWith e)).toSet ++
      gciAxiom.toSet
  }

  def varExpressions: Map[String, OWLClassExpression] = vars.mapValues(expressionParser.parse)

  private def expressionFor(template: Option[PrintfText]): Option[OWLClassExpression] = template.map(t => expressionParser.parse(t.replaced))

}

object DOSDP {

  val variablePrefix = "urn:dosdp:"

  def processedVariable(name: String): String = name.replaceAllLiterally(" ", "_")

  def variableToIRI(name: String) = IRI.create(variablePrefix + processedVariable(name))

}

final case class PrintfText(text: String, vars: List[String]) {

  def replaced: String = this.text.format(this.vars.map(name => "'$" + name + "'"): _*)

}
