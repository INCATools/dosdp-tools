package org.monarchinitiative.dosdp

import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntaxClassExpressionParser
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntaxInlineAxiomParser
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClassExpression

final case class ExpandedDOSDP(dosdp: DOSDP, prefixes: Map[String, String]) {

  private lazy val checker = new DOSDPEntityChecker(dosdp, prefixes)
  private lazy val expressionParser = new ManchesterOWLSyntaxClassExpressionParser(OWLManager.getOWLDataFactory, checker)
  private lazy val axiomParser = new ManchesterOWLSyntaxInlineAxiomParser(OWLManager.getOWLDataFactory, checker)

  def equivalentToExpression: Option[OWLClassExpression] = expressionFor(dosdp.equivalentTo)

  def subClassOfExpression: Option[OWLClassExpression] = expressionFor(dosdp.subClassOf)

  def disjointWithExpression: Option[OWLClassExpression] = expressionFor(dosdp.disjointWith)

  def gciAxiom: Option[OWLAxiom] = {
    dosdp.GCI.map(gci => axiomParser.parse(gci.replaced))
  }

  def axiomTemplates: Set[OWLAxiom] = {
    val term = Class(DOSDP.variableToIRI(DOSDP.DefinedClassVariable))
    equivalentToExpression.map(e => (term EquivalentTo e)).toSet ++
      subClassOfExpression.map(e => (term SubClassOf e)).toSet ++
      disjointWithExpression.map(e => (term DisjointWith e)).toSet ++
      gciAxiom.toSet
  }

  def varExpressions: Map[String, OWLClassExpression] = dosdp.vars.mapValues(expressionParser.parse)

  private def expressionFor(template: Option[PrintfText]): Option[OWLClassExpression] = template.map(t => expressionParser.parse(t.replaced))

}