package org.monarchinitiative.dosdp

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.expression.OWLEntityChecker
import org.semanticweb.owlapi.model._

class DOSDPEntityChecker(dosdp: DOSDP, prefixes: PartialFunction[String, String]) extends OWLEntityChecker {

  private val factory = OWLManager.getOWLDataFactory

  def getOWLAnnotationProperty(name: String): OWLAnnotationProperty = {
    val properties = dosdp.annotationProperties.getOrElse(Map.empty)
    nameToIRI(name, properties).map(factory.getOWLAnnotationProperty).orNull
  }

  def getOWLClass(name: String): OWLClass = {
    val classes = dosdp.classes.getOrElse(Map.empty)
    Prefixes.nameOrVariableToIRI(name, classes, prefixes).map(factory.getOWLClass).orNull
  }

  def getOWLDataProperty(name: String): OWLDataProperty = {
    val properties = dosdp.dataProperties.getOrElse(Map.empty)
    nameToIRI(name, properties).map(factory.getOWLDataProperty).orNull
  }

  def getOWLDatatype(name: String): OWLDatatype = Prefixes.nameOrVariableToIRI(name, Map.empty, prefixes).map(factory.getOWLDatatype).orNull

  def getOWLIndividual(name: String): OWLNamedIndividual = {
    // Use classes list to allow punning use cases; could add individuals definitions to spec
    val classes = dosdp.classes.getOrElse(Map.empty)
    Prefixes.nameOrVariableToIRI(name, classes, prefixes).map(factory.getOWLNamedIndividual).orNull
  }

  def getOWLObjectProperty(name: String): OWLObjectProperty = {
    val properties = dosdp.relations.getOrElse(Map.empty) ++ dosdp.objectProperties.getOrElse(Map.empty)
    nameToIRI(name, properties).map(factory.getOWLObjectProperty).orNull
  }

  private val Quoted = "^'(.*)'$".r

  // Added this to avoid allowing variables to possibly be properties. OWL API parser jumps to conclusions.
  private def nameToIRI(name: String, mapper: Map[String, String]): Option[IRI] = name match {
    case Quoted(unquoted) => mapper.get(unquoted).flatMap(Prefixes.idToIRI(_, prefixes))
    case _                => mapper.get(name).flatMap(Prefixes.idToIRI(_, prefixes))
  }

}