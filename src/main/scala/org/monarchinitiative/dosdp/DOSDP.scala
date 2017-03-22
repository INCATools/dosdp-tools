package org.monarchinitiative.dosdp

import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntaxClassExpressionParser
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntaxInlineAxiomParser
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClassExpression
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import scala.util.Either

final case class DOSDP(
  pattern_name: Option[String],
  base_IRI: Option[String],
  description: Option[String],
  readable_identifiers: Option[List[String]],
  classes: Option[Map[String, String]],
  relations: Option[Map[String, String]],
  objectProperties: Option[Map[String, String]],
  dataProperties: Option[Map[String, String]],
  annotationProperties: Option[Map[String, String]],
  vars: Option[Map[String, String]],
  list_vars: Option[Map[String, String]], //TODO
  data_vars: Option[Map[String, String]], //TODO
  data_list_vars: Option[Map[String, String]], //TODO
  annotations: Option[List[PrintfAnnotation]],
  logical_axioms: Option[List[PrintfOWL]],
  equivalentTo: Option[PrintfOWLConvenience],
  subClassOf: Option[PrintfOWLConvenience],
  disjointWith: Option[PrintfOWLConvenience],
  GCI: Option[PrintfOWLConvenience],
  name: Option[PrintfAnnotationOBO],
  comment: Option[PrintfAnnotationOBO],
  `def`: Option[PrintfAnnotationOBO],
  exact_synonym: Option[PrintfAnnotationOBO],
  narrow_synonym: Option[PrintfAnnotationOBO],
  related_synonym: Option[PrintfAnnotationOBO],
  broad_synonym: Option[PrintfAnnotationOBO],
  xref: Option[PrintfAnnotationOBO],
  instance_graph: Option[InstanceGraph])

object DOSDP {

  val variablePrefix = "urn:dosdp:"

  val DefinedClassVariable = "defined_class"

  def processedVariable(name: String): String = name.replaceAllLiterally(" ", "_")

  def variableToIRI(name: String) = IRI.create(variablePrefix + processedVariable(name))

}

trait PrintfText {

  def text: String

  def vars: List[String]

  def replaced: String = this.text.format(this.vars.map(name => "'$" + name + "'"): _*)

}

final case class PrintfOWL(
  annotations: Option[List[Annotations]],
  axiom_type: AxiomType,
  text: String,
  vars: List[String]) extends PrintfText

final case class PrintfOWLConvenience(
  annotations: Option[List[Annotations]],
  text: String,
  vars: List[String]) extends PrintfText

sealed class AxiomType(val property: String)

object AxiomType {

  implicit val decodeAxiomType: Decoder[AxiomType] = Decoder.decodeString.emap {
    case "equivalentTo" => Right(EquivalentTo)
    case "subClassOf"   => Right(SubClassOf)
    case "disjointWith" => Right(DisjointWith)
    case "GCI"          => Right(GCI)
    case other          => Left(s"Invalid axiom type: $other")
  }

  implicit val encodeAxiomType: Encoder[AxiomType] = Encoder.encodeString.contramap[AxiomType](_.property)

  case object EquivalentTo extends AxiomType("equivalentTo")
  case object SubClassOf extends AxiomType("subClassOf")
  case object DisjointWith extends AxiomType("disjointWith")
  case object GCI extends AxiomType("GCI")

}

sealed trait Annotations

final case class PrintfAnnotation(
  annotations: Option[List[Annotations]],
  annotationProperty: String,
  text: String,
  vars: List[String])
    extends Annotations

final case class ListAnnotation(
  annotationProperty: String,
  value: String)
    extends Annotations

object Annotations {

  implicit val decodeAnnotations: Decoder[Annotations] = Decoder[PrintfAnnotation].map[Annotations](identity).or(Decoder[ListAnnotation].map[Annotations](identity))
  implicit val encodeAnnotations: Encoder[Annotations] = Encoder.instance {
    case pfa @ PrintfAnnotation(_, _, _, _) => pfa.asJson
    case la @ ListAnnotation(_, _)          => la.asJson
  }

}

final case class PrintfAnnotationOBO(
  annotations: Option[List[Annotations]],
  xrefs: Option[List[String]],
  text: String,
  vars: List[String]) extends PrintfText

object PrintfAnnotationOBO {

  val Xref: String = "oboInOwl:hasDbXref" //FIXME should be IRI
  val Name: String = "rdfs:label" //FIXME should be IRI
  val Comment: String = "rdfs:comment" //FIXME should be IRI
  val Def: String = "obo:IAO_0000115" //FIXME should be IRI
  val ExactSynonym: String = "oboInOwl:hasExactSynonym" //FIXME should be IRI
  val NarrowSynonym: String = "oboInOwl:hasNarrowSynonym" //FIXME should be IRI
  val RelatedSynonym: String = "oboInOwl:hasRelatedSynonym" //FIXME should be IRI
  val BroadSynonym: String = "oboInOwl:hasBroadSynonym" //FIXME should be IRI

}

final case class ListAnnotationOBO(
  value: String,
  xrefs: Option[List[String]])

final case class InstanceGraph(
  nodes: Map[String, String],
  edges: List[OPA])

final case class OPA(
  edge: List[String],
  annotations: Option[List[Annotations]],
  not: Option[Boolean]) 
