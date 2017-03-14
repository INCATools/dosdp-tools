package org.monarchinitiative.dosdp

object OBOPrefixes extends PartialFunction[String, String] {

  private val standardPrefixes = Map(
    "rdf" -> "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdfs" -> "http://www.w3.org/2000/01/rdf-schema#",
    "owl" -> "http://www.w3.org/2002/07/owl#")

  private val OBONamespace = "http://purl.obolibrary.org/obo/"

  def apply(prefix: String): String = mapper(prefix)

  def isDefinedAt(prefix: String): Boolean = true

  private val mapper: (String => String) = standardPrefixes.orElse { case prefix => expandToOBO(prefix) }

  private def expandToOBO(prefix: String): String = s"$OBONamespace${prefix}_"

}