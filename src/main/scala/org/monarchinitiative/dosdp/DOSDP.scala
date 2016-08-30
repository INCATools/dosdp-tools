package org.monarchinitiative.dosdp

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.expression.OWLEntityChecker
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntaxClassExpressionParser
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAnnotationProperty
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLDataProperty
import org.semanticweb.owlapi.model.OWLDatatype
import org.semanticweb.owlapi.model.OWLNamedIndividual
import org.semanticweb.owlapi.model.OWLObjectProperty
import org.semanticweb.owlapi.model.OWLAxiom
import org.phenoscape.scowl._

final case class DOSDP(
    pattern_name: String,
    classes: Map[String, String],
    relations: Map[String, String],
    vars: Map[String, String],
    name: PrintfText,
    `def`: PrintfText,
    equivalentTo: Option[PrintfText],
    subClassOf: Option[PrintfText],
    disjointWith: Option[PrintfText]) {

  private lazy val parser = new ManchesterOWLSyntaxClassExpressionParser(OWLManager.getOWLDataFactory, new DOSDPEntityChecker(this))

  def equivalentToExpression: Option[OWLClassExpression] = expressionFor(this.equivalentTo)

  def subClassOfExpression: Option[OWLClassExpression] = expressionFor(this.subClassOf)

  def disjointWithExpression: Option[OWLClassExpression] = expressionFor(this.disjointWith)

  def axiomTemplates: Set[OWLAxiom] = {
    val term = Class(DOSDP.variableToIRI(pattern_name))
    equivalentToExpression.map(e => (term EquivalentTo e)).toSet ++
      subClassOfExpression.map(e => (term SubClassOf e)).toSet ++
      disjointWithExpression.map(e => (term DisjointWith e)).toSet
  }

  def varExpressions: Map[String, OWLClassExpression] = vars.mapValues(parser.parse)

  private def expressionFor(template: Option[PrintfText]): Option[OWLClassExpression] = template.map(t => parser.parse(t.replaced))

}

object DOSDP {

  val variablePrefix = "urn:dosdp:"

  def processedVariable(name: String): String = name.replaceAllLiterally(" ", "_")

  def variableToIRI(name: String) = IRI.create(variablePrefix + processedVariable(name))

}

final case class PrintfText(text: String, vars: List[String]) {

  def replaced: String = this.text.format(this.vars.map(name => "'$" + name + "'"): _*)

}

class DOSDPEntityChecker(dosdp: DOSDP) extends OWLEntityChecker {

  private val factory = OWLManager.getOWLDataFactory

  def getOWLAnnotationProperty(name: String): OWLAnnotationProperty = nameOrVariableToIRI(name, dosdp.relations).map(factory.getOWLAnnotationProperty).getOrElse(null)

  def getOWLClass(name: String): OWLClass = nameOrVariableToIRI(name, dosdp.classes).map(factory.getOWLClass).getOrElse(null)

  def getOWLDataProperty(name: String): OWLDataProperty = nameOrVariableToIRI(name, dosdp.relations).map(factory.getOWLDataProperty).getOrElse(null)

  def getOWLDatatype(name: String): OWLDatatype = nameOrVariableToIRI(name, dosdp.classes).map(factory.getOWLDatatype).getOrElse(null)

  def getOWLIndividual(name: String): OWLNamedIndividual = nameOrVariableToIRI(name, dosdp.classes).map(factory.getOWLNamedIndividual).getOrElse(null)

  def getOWLObjectProperty(name: String): OWLObjectProperty = nameOrVariableToIRI(name, dosdp.relations).map(factory.getOWLObjectProperty).getOrElse(null)

  private val HTTPURI = "^http.+".r
  private val DOSDPVariable = "^'\\$(.+)'$".r
  val Quoted = "^'(.*)'$".r

  private def idToIRI(id: String): IRI = id match {
    case HTTPURI(_*) => IRI.create(id)
    case _           => IRI.create(s"http://purl.obolibrary.org/obo/$id")
  }

  private def nameOrVariableToIRI(name: String, mapper: Map[String, String]): Option[IRI] = name match {
    case DOSDPVariable(varName) => Option(DOSDP.variableToIRI(varName))
    case Quoted(unquoted)       => mapper.get(unquoted).map(idToIRI)
    case _                      => mapper.get(name).map(idToIRI)
  }

}