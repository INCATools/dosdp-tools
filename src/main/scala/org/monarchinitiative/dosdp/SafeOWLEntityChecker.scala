package org.monarchinitiative.dosdp

import org.semanticweb.owlapi.expression.OWLEntityChecker
import org.semanticweb.owlapi.model._

class SafeOWLEntityChecker(checker: OWLEntityChecker) {

  def getOWLAnnotationProperty(name: String): Option[OWLAnnotationProperty] = Option(checker.getOWLAnnotationProperty(name))

  def getOWLClass(name: String): Option[OWLClass] = Option(checker.getOWLClass(name))

  def getOWLDataProperty(name: String): Option[OWLDataProperty] = Option(checker.getOWLDataProperty(name))

  def getOWLDatatype(name: String): Option[OWLDatatype] = Option(checker.getOWLDatatype(name))

  def getOWLIndividual(name: String): Option[OWLNamedIndividual] = Option(checker.getOWLIndividual(name))

  def getOWLObjectProperty(name: String): Option[OWLObjectProperty] = Option(checker.getOWLObjectProperty(name))

}