package org.monarchinitiative.dosdp

import java.util.UUID

import scala.collection.JavaConversions._

import org.phenoscape.owlet.OwletManchesterSyntaxDataType.SerializableClassExpression
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLDisjointClassesAxiom
import org.semanticweb.owlapi.model.OWLEntity
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf
import org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom

import com.typesafe.scalalogging.LazyLogging
import org.semanticweb.owlapi.model.OWLObjectUnionOf
import org.semanticweb.owlapi.model.OWLObjectAllValuesFrom

object SPARQL extends LazyLogging {

  def queryFor(dosdp: ExpandedDOSDP): String = {
    s"""
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>

SELECT DISTINCT ${selectFor(dosdp)}
WHERE {
${triplesFor(dosdp).mkString("\n")}
}
"""
  }

  def selectFor(dosdp: ExpandedDOSDP): String = {
    val axVariables = axiomVariables(dosdp)
    val variables = axVariables ++ axVariables.map(v => s"(STR(${v}__label) AS ${v}_label)")
    if (variables.isEmpty) "*" else variables.toSeq
      .sortBy(_.replaceFirst("\\(STR\\(", "").replaceFirst(DOSDP.DefinedClassVariable, "0"))
      .mkString(" ")
  }

  private def axiomVariables(dosdp: ExpandedDOSDP): Set[String] = dosdp.axiomTemplates.flatMap(selectVariables)

  private val DOSDPVariable = s"^${DOSDP.variablePrefix}(.+)".r

  def selectVariables(axiom: OWLAxiom): Set[String] =
    axiom.getSignature.toSet[OWLEntity].map(_.getIRI.toString).collect {
      case DOSDPVariable(variable) => s"?$variable"
    }

  private val Thing = OWLManager.getOWLDataFactory.getOWLThing

  def triplesFor(dosdp: ExpandedDOSDP): Seq[String] = {
    val axiomTriples = dosdp.axiomTemplates.toSeq.flatMap(triples)
    val variableTriples = dosdp.varExpressions.toSeq.flatMap {
      case (variable, Thing)           => Seq.empty // relationships to owl:Thing are not typically explicit in the ontology
      case (variable, named: OWLClass) => Seq(s"?${DOSDP.processedVariable(variable)} rdfs:subClassOf* <${named.getIRI}> .")
      case (variable, expression)      => Seq(s"?${DOSDP.processedVariable(variable)} rdfs:subClassOf ${expression.asOMN} .")
    }
    val labelTriples = axiomVariables(dosdp).map(v => s"OPTIONAL { $v rdfs:label ${v}__label . }")
    axiomTriples ++ variableTriples ++ labelTriples
  }

  def triples(axiom: OWLAxiom): Seq[String] = axiom match {
    case subClassOf: OWLSubClassOfAxiom => {
      val (subClass, subClassTriples) = triples(subClassOf.getSubClass)
      val (superClass, superClassTriples) = triples(subClassOf.getSuperClass)
      Seq(s"$subClass rdfs:subClassOf $superClass .") ++ subClassTriples ++ superClassTriples
    }
    case equivalentTo: OWLEquivalentClassesAxiom => {
      if (!equivalentTo.containsNamedEquivalentClass || (equivalentTo.getClassExpressions.size > 2)) logger.warn("More than two operands or missing named class in equivalent class axiom unexpected")
      (for {
        named <- equivalentTo.getNamedClasses.headOption
        expression <- equivalentTo.getClassExpressionsMinus(named).headOption
      } yield {
        val (namedClass, namedClassTriples) = triples(named)
        val (equivClass, equivClassTriples) = triples(expression)
        Seq(s"$namedClass owl:equivalentClass $equivClass .") ++ namedClassTriples ++ equivClassTriples
      }).toSeq.flatten
    }
    case disjointWith: OWLDisjointClassesAxiom => {
      if (!disjointWith.getClassExpressions.exists(!_.isAnonymous) || (disjointWith.getClassExpressions.size > 2)) logger.warn("More than two operands or missing named class in equivalent class axiom unexpected")
      (for {
        named <- disjointWith.getClassExpressions.find(!_.isAnonymous)
        expression <- disjointWith.getClassExpressionsMinus(named).headOption
      } yield {
        val (namedClass, namedClassTriples) = triples(named)
        val (equivClass, equivClassTriples) = triples(expression)
        Seq(s"$namedClass owl:disjointWith $equivClass .") ++ namedClassTriples ++ equivClassTriples
      }).toSeq.flatten
    }
  }

  def triples(expression: OWLClassExpression): (String, Seq[String]) = expression match {
    case named: OWLClass => {
      val node = named.getIRI.toString match {
        case DOSDPVariable(variable) => s"?$variable"
        case _                       => s"<${named.getIRI}>"
      }
      (node, Seq.empty)
    }
    case svf: OWLObjectSomeValuesFrom => {
      val node = genVar
      val (filler, fillerTriples) = triples(svf.getFiller)
      (node, Seq(
        //FIXME this assumes named object property
        s"$node owl:onProperty <${svf.getProperty.asOWLObjectProperty.getIRI}> .",
        s"$node owl:someValuesFrom $filler .") ++ fillerTriples)
    }
    case and: OWLObjectIntersectionOf => {
      val node = genVar
      val (intersectionTriples, operandTriplesList, operands) = (and.getOperands.map { o =>
        val (operand, operandTriples) = triples(o)
        (s"$node owl:intersectionOf/rdf:rest*/rdf:first $operand .", operandTriples, operand)
      }).unzip3
      val filters = operands.toSeq.combinations(2).map { pair =>
        s"FILTER(${pair(0)} != ${pair(1)})"
      }
      val listLengthTriple = s"$node owl:intersectionOf/${and.getOperands.toSeq.map(_ => "rdf:rest").mkString("/")} rdf:nil ."
      (node, (intersectionTriples.toSeq :+ listLengthTriple) ++ operandTriplesList.toSeq.flatten ++ filters)
    }
    case or: OWLObjectUnionOf         => ???
    case only: OWLObjectAllValuesFrom => ???
  }

  private def genVar: String = "?" + UUID.randomUUID.toString.replaceAllLiterally("-", "")

}