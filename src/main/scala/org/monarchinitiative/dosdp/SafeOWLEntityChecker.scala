package org.monarchinitiative.dosdp

import org.semanticweb.owlapi.expression.OWLEntityChecker
import org.semanticweb.owlapi.model._
import zio._

class SafeOWLEntityChecker(checker: OWLEntityChecker) {

  def getOWLAnnotationProperty(name: String): IO[Option[Nothing], OWLAnnotationProperty] = ZIO.fromOption(Option(checker.getOWLAnnotationProperty(name)))

  def getOWLClass(name: String): IO[Option[Nothing], OWLClass] = ZIO.fromOption(Option(checker.getOWLClass(name)))

  def getOWLDataProperty(name: String): IO[Option[Nothing], OWLDataProperty] = ZIO.fromOption(Option(checker.getOWLDataProperty(name)))

  def getOWLDatatype(name: String): IO[Option[Nothing], OWLDatatype] = ZIO.fromOption(Option(checker.getOWLDatatype(name)))

  def getOWLIndividual(name: String): IO[Option[Nothing], OWLNamedIndividual] = ZIO.fromOption(Option(checker.getOWLIndividual(name)))

  def getOWLObjectProperty(name: String): IO[Option[Nothing], OWLObjectProperty] = ZIO.fromOption(Option(checker.getOWLObjectProperty(name)))

}
