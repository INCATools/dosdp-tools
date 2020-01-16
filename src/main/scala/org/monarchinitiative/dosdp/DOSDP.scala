package org.monarchinitiative.dosdp

import org.phenoscape.scowl._
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAnnotationProperty
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import org.apache.commons.codec.digest.DigestUtils

/**
 * Basic data model for DOSDP schema, for serializing to/from JSON.
 */
final case class DOSDP(
                        pattern_name: Option[String] = None,
                        pattern_iri: Option[String] = None,
                        base_IRI: Option[String] = None,
                        description: Option[String] = None,
                        readable_identifiers: Option[List[String]] = None,
                        classes: Option[Map[String, String]] = None,
                        relations: Option[Map[String, String]] = None,
                        objectProperties: Option[Map[String, String]] = None,
                        dataProperties: Option[Map[String, String]] = None,
                        annotationProperties: Option[Map[String, String]] = None,
                        vars: Option[Map[String, String]] = None,
                        list_vars: Option[Map[String, String]] = None,
                        data_vars: Option[Map[String, String]] = None,
                        data_list_vars: Option[Map[String, String]] = None,
                        substitutions: Option[List[RegexSub]] = None,
                        annotations: Option[List[Annotations]] = None,
                        logical_axioms: Option[List[PrintfOWL]] = None,
                        equivalentTo: Option[PrintfOWLConvenience] = None,
                        subClassOf: Option[PrintfOWLConvenience] = None,
                        disjointWith: Option[PrintfOWLConvenience] = None,
                        GCI: Option[PrintfOWLConvenience] = None,
                        name: Option[PrintfAnnotationOBO] = None,
                        comment: Option[PrintfAnnotationOBO] = None,
                        `def`: Option[PrintfAnnotationOBO] = None,
                        namespace: Option[PrintfAnnotationOBO] = None,
                        exact_synonym: Option[ListAnnotationOBO] = None,
                        narrow_synonym: Option[ListAnnotationOBO] = None,
                        related_synonym: Option[ListAnnotationOBO] = None,
                        broad_synonym: Option[ListAnnotationOBO] = None,
                        generated_synonyms: Option[List[PrintfAnnotationOBO]] = None,
                        generated_narrow_synonyms: Option[List[PrintfAnnotationOBO]] = None,
                        generated_broad_synonyms: Option[List[PrintfAnnotationOBO]] = None,
                        generated_related_synonyms: Option[List[PrintfAnnotationOBO]] = None,
                        xref: Option[ListAnnotationOBO] = None,
                        instance_graph: Option[InstanceGraph] = None)

object DOSDP {

  val empty: DOSDP = DOSDP()

  val MultiValueDelimiter: Char = '|'

  val variablePrefix: String = "urn:dosdp:"

  val DefinedClassVariable: String = "defined_class"

  def processedVariable(name: String): String = name.replaceAllLiterally(" ", "_")

  def variableToIRI(name: String): IRI = IRI.create(variablePrefix + processedVariable(name))

  def computeDefinedIRI(pattern: IRI, bindings: Map[String, Binding]): IRI = {
    val text: String = bindings.toSeq.sortBy(_._1).map { case (_, binding) =>
      binding match {
        case SingleValue(value) => value
        case MultiValue(values) => values.toSeq.sorted.mkString("|")
      }
    }.mkString("&")
    val hash = DigestUtils.sha1Hex(text)
    IRI.create(s"$pattern#$hash")
  }

}

trait PrintfText {

  def text: String

  def vars: Option[List[String]]

  def annotations: Option[List[Annotations]]

  def replaced(bindings: Option[Map[String, SingleValue]]): Option[String] = PrintfText.replaced(this.text, this.vars, bindings)

}

object PrintfText {

  def replaced(text: String, vars: Option[List[String]], bindings: Option[Map[String, SingleValue]]): Option[String] = {
    import cats.implicits._
    val fillersOpt = vars.map { realVars =>
      bindings match {
        case None        => Some(realVars.map(name => "'$" + name + "'"))
        case Some(bound) =>
          val stringValues = bound.mapValues(_.value)
          realVars.map(v => stringValues.get(v)).sequence
      }
    }
    println(text)
    println(vars)
    println(bindings)
    println(fillersOpt)
    println
    fillersOpt.getOrElse(Some(Nil)).map(fillers => text.format(fillers: _*))
    //fillersOpt.map(_.getOrElse(Nil)).map(fillers => text.format(fillers: _*))
  }

}

final case class PrintfOWL(
                            annotations: Option[List[Annotations]],
                            axiom_type: AxiomType,
                            text: String,
                            vars: Option[List[String]]) extends PrintfText

final case class PrintfOWLConvenience(
                                       annotations: Option[List[Annotations]],
                                       text: String,
                                       vars: Option[List[String]]) extends PrintfText

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
                                   annotations: Option[List[Annotations]],
                                   annotationProperty: String,
                                   text: String,
                                   vars: Option[List[String]],
                                   `override`: Option[String])
  extends Annotations with PrintfText

final case class ListAnnotation(
                                 annotations: Option[List[Annotations]],
                                 annotationProperty: String,
                                 value: String)
  extends Annotations

final case class IRIValueAnnotation(
                                     annotations: Option[List[Annotations]],
                                     annotationProperty: String,
                                     `var`: String
                                   )
  extends Annotations

object Annotations {

  implicit val decodeAnnotations: Decoder[Annotations] = Decoder[PrintfAnnotation].map[Annotations](identity).or(Decoder[ListAnnotation].map[Annotations](identity)).or(Decoder[IRIValueAnnotation].map[Annotations](identity))
  implicit val encodeAnnotations: Encoder[Annotations] = Encoder.instance {
    case pfa @ PrintfAnnotation(_, _, _, _, _) => pfa.asJson
    case la @ ListAnnotation(_, _, _)          => la.asJson
    case iva @ IRIValueAnnotation(_, _, _)     => iva.asJson
  }

}

sealed trait OBOAnnotations

final case class PrintfAnnotationOBO(
                                      annotations: Option[List[Annotations]],
                                      xrefs: Option[String],
                                      text: String,
                                      vars: Option[List[String]]) extends PrintfText with AnnotationLike with OBOAnnotations

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

  val overrides = Map(
    Name -> "defined_class_name",
    Comment -> "defined_class_comment",
    Def -> "defined_class_definition",
    Namespace -> "defined_class_namespace",
    ExactSynonym -> "defined_class_exact_synonym",
    NarrowSynonym -> "defined_class_narrow_synonym",
    RelatedSynonym -> "defined_class_related_synonym",
    BroadSynonym -> "defined_class_broad_synonym"
  )

}

final case class ListAnnotationOBO(
                                    value: String,
                                    xrefs: Option[String]) extends AnnotationLike with OBOAnnotations

final case class RegexSub(in: String, out: String, `match`: String, sub: String)

final case class InstanceGraph(
                                nodes: Map[String, String],
                                edges: List[OPA])

final case class OPA(
                      edge: List[String], //TODO require length of 3
                      annotations: Option[List[Annotations]],
                      not: Option[Boolean])
