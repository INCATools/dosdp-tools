package org.monarchinitiative.dosdp

import org.phenoscape.scowl._

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.expression.OWLEntityChecker
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAnnotationProperty
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLDataProperty
import org.semanticweb.owlapi.model.OWLDatatype
import org.semanticweb.owlapi.model.OWLNamedIndividual
import org.semanticweb.owlapi.model.OWLObjectProperty

class DOSDPEntityChecker(dosdp: DOSDP, prefixes: PartialFunction[String, String]) extends OWLEntityChecker {

  private val factory = OWLManager.getOWLDataFactory

  def getOWLAnnotationProperty(name: String): OWLAnnotationProperty = nameToIRI(name, dosdp.relations).map(factory.getOWLAnnotationProperty).getOrElse(null)

  def getOWLClass(name: String): OWLClass = nameOrVariableToIRI(name, dosdp.classes).map(factory.getOWLClass).getOrElse(null)

  def getOWLDataProperty(name: String): OWLDataProperty = nameToIRI(name, dosdp.relations).map(factory.getOWLDataProperty).getOrElse(null)

  def getOWLDatatype(name: String): OWLDatatype = nameOrVariableToIRI(name, dosdp.classes).map(factory.getOWLDatatype).getOrElse(null)

  def getOWLIndividual(name: String): OWLNamedIndividual = nameOrVariableToIRI(name, dosdp.classes).map(factory.getOWLNamedIndividual).getOrElse(null)

  def getOWLObjectProperty(name: String): OWLObjectProperty = nameToIRI(name, dosdp.relations).map(factory.getOWLObjectProperty).getOrElse(null)

  private val HTTPURI = "^http.+".r
  private val DOSDPVariable = "^'\\$(.+)'$".r
  private val Quoted = "^'(.*)'$".r
  private val CURIE = "^([^:]*):(.*)$".r

  private def idToIRI(id: String): Option[IRI] = id match {
    case HTTPURI(_*)          => Option(IRI.create(id))
    case CURIE(prefix, local) => prefixes.lift(prefix).map(uri => IRI.create(s"$uri$local"))
    case _                    => None
    //IRI.create(s"http://purl.obolibrary.org/obo/$id")
  }

  private def nameOrVariableToIRI(name: String, mapper: Map[String, String]): Option[IRI] = name match {
    case DOSDPVariable(varName) => Option(DOSDP.variableToIRI(varName))
    case Quoted(unquoted)       => mapper.get(unquoted).flatMap(idToIRI)
    case _                      => mapper.get(name).flatMap(idToIRI)
  }

  // Added this to avoid allowing variables to possibly be properties. OWL API parser jumps to conclusions.
  private def nameToIRI(name: String, mapper: Map[String, String]): Option[IRI] = name match {
    case Quoted(unquoted) => mapper.get(unquoted).flatMap(idToIRI)
    case _                => mapper.get(name).flatMap(idToIRI)
  }

}