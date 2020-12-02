package org.monarchinitiative.dosdp

import java.util.UUID
import java.util.regex.Pattern

import org.apache.jena.query.ParameterizedSparqlString
import org.monarchinitiative.dosdp.cli.Config.{AxiomKind, LogicalAxioms}
import org.monarchinitiative.dosdp.cli.Generate
import org.phenoscape.owlet.OwletManchesterSyntaxDataType.SerializableClassExpression
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._

import scala.jdk.CollectionConverters._

object SPARQL {

  private val factory = OWLManager.getOWLDataFactory()

  def queryFor(dosdp: ExpandedDOSDP, axioms: AxiomKind): String = {
    s"""
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT DISTINCT ${selectFor(dosdp, axioms)}
WHERE {
${triplesFor(dosdp, axioms).mkString("\n")}
}
ORDER BY ?defined_class_label
"""
  }

  def selectFor(dosdp: ExpandedDOSDP, axioms: AxiomKind): String = {
    val axVariables = axiomVariables(dosdp)
    val variables = axVariables ++ axVariables.map(v => s"(STR(${v}__label) AS ${v}_label)") ++
      (if (axioms != LogicalAxioms) axVariables.filterNot(_.startsWith(s"?${DOSDP.DefinedClassVariable}"))
        .map(v => s"(STR(${v}__match_property) AS ${v}_match_property)") else Set.empty[String])
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

  def triplesFor(dosdp: ExpandedDOSDP, axioms: AxiomKind): Seq[String] = {
    val props = dosdp.readableIdentifierProperties.to(Set)
    val logicalBindings = dosdp.dosdp.vars.map { vars =>
      vars.keys.map { key =>
        key -> SingleValue(DOSDP.variableToIRI(key).toString)
      }.toMap
    }
    val (queryLogical, queryAnnotations) = Generate.axiomsOutputChoice(axioms)
    val annotationTriples = if (queryAnnotations) dosdp.filledAnnotationAxioms(logicalBindings, None).toSeq.flatMap(ax => triples(ax, props))
    else Nil
    val axiomTriples = if (queryLogical) dosdp.filledLogicalAxioms(None, None).toSeq.flatMap(ax => triples(ax, props))
    else Nil
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
    annotationTriples ++ axiomTriples ++ variableTriples ++ labelTriples
  }

  def triples(axiom: OWLAxiom, readableIdentifierProperties: Set[OWLAnnotationProperty]): Seq[String] = axiom match {
    case subClassOf: OWLSubClassOfAxiom                   =>
      val (subClass, subClassTriples) = triples(subClassOf.getSubClass)
      val (superClass, superClassTriples) = triples(subClassOf.getSuperClass)
      Seq(s"$subClass rdfs:subClassOf $superClass .") ++ subClassTriples ++ superClassTriples
    case equivalentTo: OWLEquivalentClassesAxiom          =>
      if (!equivalentTo.containsNamedEquivalentClass || (equivalentTo.getClassExpressions.size > 2)) scribe.warn("More than two operands or missing named class in equivalent class axiom unexpected")
      (for {
        named <- equivalentTo.getNamedClasses.asScala.headOption
        expression <- equivalentTo.getClassExpressionsMinus(named).asScala.headOption
      } yield {
        val (namedClass, namedClassTriples) = triples(named)
        val (equivClass, equivClassTriples) = triples(expression)
        Seq(s"$namedClass owl:equivalentClass $equivClass .") ++ namedClassTriples ++ equivClassTriples
      }).toSeq.flatten
    case disjointWith: OWLDisjointClassesAxiom            =>
      if (!disjointWith.getClassExpressions.asScala.forall(_.isAnonymous) || (disjointWith.getClassExpressions.size > 2)) scribe.warn("More than two operands or missing named class in equivalent class axiom unexpected")
      (for {
        named <- disjointWith.getClassExpressions.asScala.find(!_.isAnonymous)
        expression <- disjointWith.getClassExpressionsMinus(named).asScala.headOption
      } yield {
        val (namedClass, namedClassTriples) = triples(named)
        val (equivClass, equivClassTriples) = triples(expression)
        Seq(s"$namedClass owl:disjointWith $equivClass .") ++ namedClassTriples ++ equivClassTriples
      }).toSeq.flatten
    case annotationAssertion: OWLAnnotationAssertionAxiom =>
      val (subject, subjecTriples) = triples(factory.getOWLClass(annotationAssertion.getSubject.asInstanceOf[IRI]))
      val property = s"<${annotationAssertion.getProperty.getIRI}>"
      val (value, valueTriples) = triples(annotationAssertion.getValue, readableIdentifierProperties)
      Seq(s"$subject $property $value .") ++ subjecTriples ++ valueTriples
  }

  private val DOSDPVariableIRIMatch = s"\\b${DOSDP.variablePrefix}(\\S+)\\b".r

  private def escape(text: String): String = {
    val pss = new ParameterizedSparqlString()
    pss.appendLiteral(text)
    pss.toString
  }

  def triples(annotationValue: OWLAnnotationValue, readableIdentifierProperties: Set[OWLAnnotationProperty]): (String, Seq[String]) = annotationValue match {
    case iri: IRI            =>
      iri.toString match {
        case DOSDPVariable(variable) => (s"?$variable", List(s"FILTER(isIRI(?$variable))"))
        case _                       => (s"<$iri>", Nil)
      }
    case literal: OWLLiteral =>
      val node = genVar
      val text = literal.getLiteral
      val valueRegex = DOSDPVariableIRIMatch.split(text).map(Pattern.quote).mkString("(.+)")
      val variableNames = DOSDPVariableIRIMatch.findAllMatchIn(text).toList.map(_.group(1))
      val predicates = readableIdentifierProperties.map(p => s"<${p.getIRI}>").mkString(" ")
      val varPatterns = variableNames.zipWithIndex.flatMap { case (variableName, index) =>
        val predicateVar = s"${variableName}__match_property"
        val variableMatchLabel = s"${variableName}__match_label"
        List(
          s"""BIND((REPLACE($node, ${escape(valueRegex)}, "$$${index + 1}")) AS ?$variableMatchLabel)""",
          s"VALUES ?$predicateVar { $predicates }",
          s"?$variableName ?$predicateVar ?$variableMatchLabel ."
        )
      }
      (node, s"FILTER(REGEX($node, ${escape(valueRegex)}))" :: varPatterns)
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

  private def genVar: String = "?" + UUID.randomUUID.toString.replace("-", "")

}