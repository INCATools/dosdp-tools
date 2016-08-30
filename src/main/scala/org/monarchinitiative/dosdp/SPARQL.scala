package org.monarchinitiative.dosdp

import java.util.UUID

import scala.collection.JavaConversions._

import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLEntity
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf
import org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom

import org.phenoscape.owlet.OwletManchesterSyntaxDataType.SerializableClassExpression
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom

object SPARQL {

  def queryFor(dosdp: DOSDP): String = {
    s"""
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT DISTINCT ${selectFor(dosdp)}
WHERE {
${triplesFor(dosdp).mkString("\n")}
}
"""
  }

  def selectFor(dosdp: DOSDP): String = {
    val variables = dosdp.axiomTemplates.flatMap(selectVariables)
    if (variables.isEmpty) "*" else variables.toSeq.sorted.mkString(" ")
  }

  private val DOSDPVariable = s"^${DOSDP.variablePrefix}(.+)".r

  def selectVariables(axiom: OWLAxiom): Set[String] =
    axiom.getSignature.toSet[OWLEntity].map(_.getIRI).collect {
      case DOSDPVariable(variable) => s"?$variable"
    }

  def triplesFor(dosdp: DOSDP): Seq[String] = {
    val axiomTriples = dosdp.axiomTemplates.toSeq.flatMap(triples)
    val variableTriples = dosdp.varExpressions.toSeq.map {
      case (variable, named: OWLClass) => s"?${DOSDP.processedVariable(variable)} rdfs:subClassOf* <${named.getIRI}> ."
      case (variable, expression)      => s"?${DOSDP.processedVariable(variable)} rdfs:subClassOf ${expression.asOMN} ."
    }
    axiomTriples ++ variableTriples
  }

  def triples(axiom: OWLAxiom): Seq[String] = axiom match {
    case subClassOf: OWLSubClassOfAxiom => {
      val (subClass, subClassTriples) = triples(subClassOf.getSubClass)
      val (superClass, superClassTriples) = triples(subClassOf.getSuperClass)
      Seq(s"$subClass rdfs:subClassOf $superClass") ++ subClassTriples ++ superClassTriples
    }
    //TODO improve warning if there are more than 2 operands in equiv class axiom
    case equivalentTo: OWLEquivalentClassesAxiom => {
      if (!equivalentTo.containsNamedEquivalentClass || (equivalentTo.getClassExpressions.size > 2)) println("Bad equivalent class")
      (for {
        named <- equivalentTo.getNamedClasses.headOption
        expression <- equivalentTo.getClassExpressionsMinus(named).headOption
      } yield {
        val (namedClass, namedClassTriples) = triples(named)
        val (equivClass, equivClassTriples) = triples(expression)
        Seq(s"$namedClass owl:equivalentClass $equivClass") ++ namedClassTriples ++ equivClassTriples
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
  }

  private def genVar: String = "?" + UUID.randomUUID.toString.replaceAllLiterally("-", "")

}