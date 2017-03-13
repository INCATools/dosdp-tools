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

}

object DOSDP {

  val variablePrefix = "urn:dosdp:"

  val DefinedClassVariable = "defined_class"

  def processedVariable(name: String): String = name.replaceAllLiterally(" ", "_")

  def variableToIRI(name: String) = IRI.create(variablePrefix + processedVariable(name))

}

final case class PrintfText(text: String, vars: List[String]) {

  def replaced: String = this.text.format(this.vars.map(name => "'$" + name + "'"): _*)

}
