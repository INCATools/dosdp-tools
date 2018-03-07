package org.monarchinitiative.dosdp

import org.semanticweb.owlapi.model.IRI

object OBOPrefixes extends PartialFunction[String, String] {

  private val standardPrefixes = Map(
    "rdf" -> "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdfs" -> "http://www.w3.org/2000/01/rdf-schema#",
    "owl" -> "http://www.w3.org/2002/07/owl#",
    "dc" -> "http://purl.org/dc/elements/1.1/",
    "dct" -> "http://purl.org/dc/terms/",
    "skos" -> "http://www.w3.org/2004/02/skos/core#",
    "obo" -> "http://purl.obolibrary.org/obo/",
    "oio" -> "http://www.geneontology.org/formats/oboInOwl#")

  private val OBONamespace = "http://purl.obolibrary.org/obo/"

  def apply(prefix: String): String = mapper(prefix)

  def isDefinedAt(prefix: String): Boolean = true

  private val mapper: (String => String) = standardPrefixes.orElse { case prefix => expandToOBO(prefix) }

  private def expandToOBO(prefix: String): String = s"$OBONamespace${prefix}_"

}

object Prefixes {

  private val HTTPURI = "^http.+".r
  private val DOSDPVariable = "^'\\$(.+)'$".r
  private val Quoted = "^'(.*)'$".r
  private val CURIE = "^([^:]*):(.*)$".r
  private val FullIRI = "^<(.+)>$".r

  def idToIRI(id: String, prefixes: PartialFunction[String, String]): Option[IRI] = id match {
    case HTTPURI(_*)          => Option(IRI.create(id))
    case CURIE(prefix, local) => prefixes.lift(prefix).map(uri => IRI.create(s"$uri$local"))
    case _                    => None
  }

  def nameOrVariableToIRI(name: String, mapper: Map[String, String], prefixes: PartialFunction[String, String]): Option[IRI] = name match {
    case DOSDPVariable(varName) => Option(DOSDP.variableToIRI(varName))
    case Quoted(unquoted)       => mapper.get(unquoted).flatMap(idToIRI(_, prefixes))
    case FullIRI(iri)           => Option(IRI.create(iri))
    case http @ HTTPURI(_*)     => idToIRI(http, prefixes)
    case curie @ CURIE(_, _)    => idToIRI(curie, prefixes)
    case _                      => mapper.get(name).flatMap(idToIRI(_, prefixes))
  }

}