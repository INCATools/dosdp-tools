package org.monarchinitiative.dosdp

import org.phenoscape.scowl._
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAnnotationProperty

import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._

/**
 * Basic data model for DOSDP schema, for serializing to/from JSON.
 */
final case class DOSDP(
  pattern_name:               Option[String],
  pattern_iri:                Option[String],
  base_IRI:                   Option[String],
  description:                Option[String],
  readable_identifiers:       Option[List[String]],
  classes:                    Option[Map[String, String]],
  relations:                  Option[Map[String, String]],
  objectProperties:           Option[Map[String, String]],
  dataProperties:             Option[Map[String, String]],
  annotationProperties:       Option[Map[String, String]],
  vars:                       Option[Map[String, String]],
  list_vars:                  Option[Map[String, String]],
  data_vars:                  Option[Map[String, String]],
  data_list_vars:             Option[Map[String, String]],
  substitutions:              Option[List[RegexSub]],
  annotations:                Option[List[PrintfAnnotation]],
  logical_axioms:             Option[List[PrintfOWL]],
  equivalentTo:               Option[PrintfOWLConvenience],
  subClassOf:                 Option[PrintfOWLConvenience],
  disjointWith:               Option[PrintfOWLConvenience],
  GCI:                        Option[PrintfOWLConvenience],
  name:                       Option[PrintfAnnotationOBO],
  comment:                    Option[PrintfAnnotationOBO],
  `def`:                      Option[PrintfAnnotationOBO],
  namespace:                  Option[PrintfAnnotationOBO],
  exact_synonym:              Option[ListAnnotationOBO],
  narrow_synonym:             Option[ListAnnotationOBO],
  related_synonym:            Option[ListAnnotationOBO],
  broad_synonym:              Option[ListAnnotationOBO],
  generated_synonyms:         Option[List[PrintfAnnotationOBO]],
  generated_narrow_synonyms:  Option[List[PrintfAnnotationOBO]],
  generated_broad_synonyms:   Option[List[PrintfAnnotationOBO]],
  generated_related_synonyms: Option[List[PrintfAnnotationOBO]],
  xref:                       Option[ListAnnotationOBO],
  instance_graph:             Option[InstanceGraph])

object DOSDP {

  val empty = DOSDP(None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)

  val MultiValueDelimiter: Char = '|'

  val variablePrefix: String = "urn:dosdp:"

  val DefinedClassVariable: String = "defined_class"

  def processedVariable(name: String): String = name.replaceAllLiterally(" ", "_")

  def variableToIRI(name: String) = IRI.create(variablePrefix + processedVariable(name))

}

trait PrintfText {

  def text: String

  def vars: Option[List[String]]

  def annotations: Option[List[Annotations]]

  def replaced(bindings: Option[Map[String, SingleValue]]): String = PrintfText.replaced(this.text, this.vars, bindings)

}

object PrintfText {

  def replaced(text: String, vars: Option[List[String]], bindings: Option[Map[String, SingleValue]]): String = {
    val fillers = (vars.map { realVars =>
      bindings match {
        case None        => realVars.map(name => "'$" + name + "'")
        case Some(bound) => realVars.map(bound.mapValues(_.value))
      }
    }).getOrElse(Nil)
    text.format(fillers: _*)
  }

}

final case class PrintfOWL(
  annotations: Option[List[Annotations]],
  axiom_type:  AxiomType,
  text:        String,
  vars:        Option[List[String]]) extends PrintfText

final case class PrintfOWLConvenience(
  annotations: Option[List[Annotations]],
  text:        String,
  vars:        Option[List[String]]) extends PrintfText

abstract sealed class AxiomType(val property: String)

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

sealed trait AnnotationLike

sealed trait Annotations extends AnnotationLike {

  def annotationProperty: String

}

final case class PrintfAnnotation(
  annotations:        Option[List[Annotations]],
  annotationProperty: String,
  text:               String,
  vars:               Option[List[String]])
  extends Annotations with PrintfText

final case class ListAnnotation(
  annotations:        Option[List[Annotations]],
  annotationProperty: String,
  value:              String)
  extends Annotations

object Annotations {

  implicit val decodeAnnotations: Decoder[Annotations] = Decoder[PrintfAnnotation].map[Annotations](identity).or(Decoder[ListAnnotation].map[Annotations](identity))
  implicit val encodeAnnotations: Encoder[Annotations] = Encoder.instance {
    case pfa @ PrintfAnnotation(_, _, _, _) => pfa.asJson
    case la @ ListAnnotation(_, _, _)       => la.asJson
  }

}

sealed trait OBOAnnotations

final case class PrintfAnnotationOBO(
  annotations: Option[List[Annotations]],
  xrefs:       Option[String],
  text:        String,
  vars:        Option[List[String]]) extends PrintfText with AnnotationLike with OBOAnnotations

object PrintfAnnotationOBO {

  val Xref: OWLAnnotationProperty = AnnotationProperty("http://www.geneontology.org/formats/oboInOwl#hasDbXref")
  val Name: OWLAnnotationProperty = AnnotationProperty("http://www.w3.org/2000/01/rdf-schema#label")
  val Comment: OWLAnnotationProperty = AnnotationProperty("http://www.w3.org/2000/01/rdf-schema#comment")
  val Def: OWLAnnotationProperty = AnnotationProperty("http://purl.obolibrary.org/obo/IAO_0000115")
  val Namespace: OWLAnnotationProperty = AnnotationProperty("http://www.geneontology.org/formats/oboInOwl#hasOBONamespace")
  val ExactSynonym: OWLAnnotationProperty = AnnotationProperty("http://www.geneontology.org/formats/oboInOwl#hasExactSynonym")
  val NarrowSynonym: OWLAnnotationProperty = AnnotationProperty("http://www.geneontology.org/formats/oboInOwl#hasNarrowSynonym")
  val RelatedSynonym: OWLAnnotationProperty = AnnotationProperty("http://www.geneontology.org/formats/oboInOwl#hasRelatedSynonym")
  val BroadSynonym: OWLAnnotationProperty = AnnotationProperty("http://www.geneontology.org/formats/oboInOwl#hasBroadSynonym")

}

final case class ListAnnotationOBO(
  value: String,
  xrefs: Option[String]) extends AnnotationLike with OBOAnnotations

final case class RegexSub(in: String, out: String, `match`: String, sub: String)

final case class InstanceGraph(
  nodes: Map[String, String],
  edges: List[OPA])

final case class OPA(
  edge:        List[String], //TODO require length of 3
  annotations: Option[List[Annotations]],
  not:         Option[Boolean]) 
