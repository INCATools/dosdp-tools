package org.monarchinitiative.dosdp

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

  def getOWLAnnotationProperty(name: String): OWLAnnotationProperty = {
    val properties = dosdp.annotationProperties.getOrElse(Map.empty)
    nameToIRI(name, properties).map(factory.getOWLAnnotationProperty).getOrElse(null)
  }

  def getOWLClass(name: String): OWLClass = {
    val classes = dosdp.classes.getOrElse(Map.empty)
    Prefixes.nameOrVariableToIRI(name, classes, prefixes).map(factory.getOWLClass).getOrElse(null)
  }

  def getOWLDataProperty(name: String): OWLDataProperty = {
    val properties = dosdp.dataProperties.getOrElse(Map.empty)
    nameToIRI(name, properties).map(factory.getOWLDataProperty).getOrElse(null)
  }

  def getOWLDatatype(name: String): OWLDatatype = null

  def getOWLIndividual(name: String): OWLNamedIndividual = null

  def getOWLObjectProperty(name: String): OWLObjectProperty = {
    val properties = dosdp.relations.getOrElse(Map.empty) ++ dosdp.objectProperties.getOrElse(Map.empty)
    nameToIRI(name, properties).map(factory.getOWLObjectProperty).getOrElse(null)
  }

  private val HTTPURI = "^http.+".r
  private val DOSDPVariable = "^'\\$(.+)'$".r
  private val Quoted = "^'(.*)'$".r
  private val CURIE = "^([^:]*):(.*)$".r
  private val FullIRI = "^<(.+)>$".r

  // Added this to avoid allowing variables to possibly be properties. OWL API parser jumps to conclusions.
  private def nameToIRI(name: String, mapper: Map[String, String]): Option[IRI] = name match {
    case Quoted(unquoted) => mapper.get(unquoted).flatMap(Prefixes.idToIRI(_, prefixes))
    case _                => mapper.get(name).flatMap(Prefixes.idToIRI(_, prefixes))
  }

}