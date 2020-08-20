package org.monarchinitiative.dosdp

import java.util.UUID

import org.apache.jena.query.ParameterizedSparqlString
import org.phenoscape.owlet.OwletManchesterSyntaxDataType.SerializableClassExpression
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._

import scala.jdk.CollectionConverters._

object SPARQL {

  def queryFor(dosdp: ExpandedDOSDP): String = {
    s"""
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>

SELECT DISTINCT ${selectFor(dosdp)}
WHERE {
${triplesFor(dosdp).mkString("\n")}
}
ORDER BY ?defined_class_label
"""
  }

  def selectFor(dosdp: ExpandedDOSDP): String = {
    val axVariables = axiomVariables(dosdp)
    val variables = axVariables ++ axVariables.map(v => s"(STR(${v}__label) AS ${v}_label)")
    if (variables.isEmpty) "*" else variables.toSeq
      .sortBy(_.replaceFirst("\\(STR\\(", "").replaceFirst(DOSDP.DefinedClassVariable, "0"))
      .mkString(" ")
  }

  private def axiomVariables(dosdp: ExpandedDOSDP): Set[String] = dosdp.filledLogicalAxioms(None, None).flatMap(selectVariables)

  private val DOSDPVariable = s"^${DOSDP.variablePrefix}(.+)".r

  def selectVariables(axiom: OWLAxiom): Set[String] =
    axiom.getSignature.asScala.toSet[OWLEntity].map(_.getIRI.toString).collect {
      case DOSDPVariable(variable) => s"?$variable"
    }

  private val Thing = OWLManager.getOWLDataFactory.getOWLThing

  def triplesFor(dosdp: ExpandedDOSDP): Seq[String] = {
    val axiomTriples = dosdp.filledLogicalAxioms(None, None).toSeq.flatMap(triples)
    val variableTriples = dosdp.varExpressions.toSeq.flatMap {
      case (_, Thing)                  => Seq.empty // relationships to owl:Thing are not typically explicit in the ontology
      case (variable, named: OWLClass) => Seq(s"?${DOSDP.processedVariable(variable)} rdfs:subClassOf* <${named.getIRI}> .")
      case (variable, expression)      =>
        val pss = new ParameterizedSparqlString()
        pss.appendNode(expression.asOMN)
        val sanitizedExpression = pss.toString
        Seq(s"?${DOSDP.processedVariable(variable)} rdfs:subClassOf $sanitizedExpression .")
    }
    val labelTriples = axiomVariables(dosdp).map(v => s"OPTIONAL { $v rdfs:label ${v}__label . }")
    axiomTriples ++ variableTriples ++ labelTriples
  }

  def triples(axiom: OWLAxiom): Seq[String] = axiom match {
    case subClassOf: OWLSubClassOfAxiom          =>
      val (subClass, subClassTriples) = triples(subClassOf.getSubClass)
      val (superClass, superClassTriples) = triples(subClassOf.getSuperClass)
      Seq(s"$subClass rdfs:subClassOf $superClass .") ++ subClassTriples ++ superClassTriples
    case equivalentTo: OWLEquivalentClassesAxiom =>
      if (!equivalentTo.containsNamedEquivalentClass || (equivalentTo.getClassExpressions.size > 2)) scribe.warn("More than two operands or missing named class in equivalent class axiom unexpected")
      (for {
        named <- equivalentTo.getNamedClasses.asScala.headOption
        expression <- equivalentTo.getClassExpressionsMinus(named).asScala.headOption
      } yield {
        val (namedClass, namedClassTriples) = triples(named)
        val (equivClass, equivClassTriples) = triples(expression)
        Seq(s"$namedClass owl:equivalentClass $equivClass .") ++ namedClassTriples ++ equivClassTriples
      }).toSeq.flatten
    case disjointWith: OWLDisjointClassesAxiom   =>
      if (!disjointWith.getClassExpressions.asScala.forall(_.isAnonymous) || (disjointWith.getClassExpressions.size > 2)) scribe.warn("More than two operands or missing named class in equivalent class axiom unexpected")
      (for {
        named <- disjointWith.getClassExpressions.asScala.find(!_.isAnonymous)
        expression <- disjointWith.getClassExpressionsMinus(named).asScala.headOption
      } yield {
        val (namedClass, namedClassTriples) = triples(named)
        val (equivClass, equivClassTriples) = triples(expression)
        Seq(s"$namedClass owl:disjointWith $equivClass .") ++ namedClassTriples ++ equivClassTriples
      }).toSeq.flatten
  }

  def triples(expression: OWLClassExpression): (String, Seq[String]) = expression match {
    case named: OWLClass              =>
      named.getIRI.toString match {
        case DOSDPVariable(variable) => (s"?$variable", List(s"FILTER(isIRI(?$variable))"))
        case _                       => (s"<${named.getIRI}>", Nil)
      }
    case svf: OWLObjectSomeValuesFrom =>
      val node = genVar
      val (filler, fillerTriples) = triples(svf.getFiller)
      (node, Seq(
        //this assumes named object property
        s"$node owl:onProperty <${svf.getProperty.asOWLObjectProperty.getIRI}> .",
        s"$node owl:someValuesFrom $filler .") ++ fillerTriples)
    case avf: OWLObjectAllValuesFrom  =>
      val node = genVar
      val (filler, fillerTriples) = triples(avf.getFiller)
      (node, Seq(
        //this assumes named object property
        s"$node owl:onProperty <${avf.getProperty.asOWLObjectProperty.getIRI}> .",
        s"$node owl:allValuesFrom $filler .") ++ fillerTriples)
    case and: OWLObjectIntersectionOf =>
      val node = genVar
      val (intersectionTriples, operandTriplesList, operands) = and.getOperands.asScala.map { o =>
        val (operand, operandTriples) = triples(o)
        (s"$node owl:intersectionOf/rdf:rest*/rdf:first $operand .", operandTriples, operand)
      }.unzip3
      val filters = operands.toSeq.combinations(2).map { pair =>
        s"FILTER(${pair.head} != ${pair.last})"
      }
      val listLengthTriple = s"$node owl:intersectionOf/${and.getOperands.asScala.toSeq.map(_ => "rdf:rest").mkString("/")} rdf:nil ."
      (node, (intersectionTriples.toSeq :+ listLengthTriple) ++ operandTriplesList.toSeq.flatten ++ filters)
    case or: OWLObjectUnionOf         =>
      val node = genVar
      val (unionTriples, operandTriplesList, operands) = or.getOperands.asScala.map { o =>
        val (operand, operandTriples) = triples(o)
        (s"$node owl:unionOf/rdf:rest*/rdf:first $operand .", operandTriples, operand)
      }.unzip3
      val filters = operands.toSeq.combinations(2).map { pair =>
        s"FILTER(${pair.head} != ${pair.last})"
      }
      val listLengthTriple = s"$node owl:unionOf/${or.getOperands.asScala.toSeq.map(_ => "rdf:rest").mkString("/")} rdf:nil ."
      (node, (unionTriples.toSeq :+ listLengthTriple) ++ operandTriplesList.toSeq.flatten ++ filters)
  }

  private def genVar: String = "?" + UUID.randomUUID.toString.replaceAllLiterally("-", "")

}