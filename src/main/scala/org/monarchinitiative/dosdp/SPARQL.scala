package org.monarchinitiative.dosdp

import java.util.UUID
import java.util.regex.Pattern
import org.apache.jena.query.ParameterizedSparqlString
import org.monarchinitiative.dosdp.cli.Config.{AnnotationAxioms, AxiomKind, LogicalAxioms}
import org.monarchinitiative.dosdp.cli.{DOSDPError, Generate}
import org.phenoscape.owlet.OwletManchesterSyntaxDataType.SerializableClassExpression
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import org.phenoscape.scowl._
import zio.{Config => _, _}

import scala.jdk.CollectionConverters._

object SPARQL {

  private val factory = OWLManager.getOWLDataFactory()

  def queryFor(compiled: CompiledPattern, axioms: AxiomKind): IO[DOSDPError, String] = {
    // Logical axioms always materialize even on an annotation-only query: the
    // `OPTIONAL { ?var rdfs:label ... }` clauses are derived from their
    // variable set. Compute once and share with `selectFor` / `triplesFor` so
    // the placeholder expansion runs only once per query.
    val logicalAxioms = Expansion.placeholderAxioms(compiled, LogicalAxioms)
    triplesFor(compiled, axioms, logicalAxioms).map { triples =>
      val select = selectFor(axioms, logicalAxioms)
      s"""
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT DISTINCT $select
WHERE {
${triples.mkString("\n")}
}
ORDER BY ?defined_class_label
"""
    }
  }

  private def selectFor(axioms: AxiomKind, logicalAxioms: Set[OWLAxiom]): String = {
    val axVariables = axiomVariables(logicalAxioms)
    val variables = axVariables ++ axVariables.map(v => s"(STR(${v}__label) AS ${v}_label)") ++
      (if (axioms != LogicalAxioms) axVariables.filterNot(_.startsWith(s"?${DOSDP.DefinedClassVariable}"))
        .map(v => s"(STR(${v}__match_property) AS ${v}_match_property)") else Set.empty[String])
    if (variables.isEmpty) "*" else variables.toSeq
      .sortBy(_.replaceFirst("\\(STR\\(", "").replaceFirst(DOSDP.DefinedClassVariable, "0"))
      .mkString(" ")
  }

  private def axiomVariables(axioms: Set[OWLAxiom]): Set[String] =
    axioms.flatMap(selectVariables)

  private val DOSDPVariable = s"^${DOSDP.variablePrefix}([A-Za-z0-9_]+)$$".r

  def selectVariables(axiom: OWLAxiom): Set[String] =
    axiom.getSignature.asScala.toSet[OWLEntity].map(_.getIRI.toString).collect {
      case DOSDPVariable(variable) => s"?$variable"
    }

  private val Thing = OWLManager.getOWLDataFactory.getOWLThing

  def triplesFor(compiled: CompiledPattern, axioms: AxiomKind): IO[DOSDPError, Seq[String]] =
    triplesFor(compiled, axioms, Expansion.placeholderAxioms(compiled, LogicalAxioms))

  private def triplesFor(compiled: CompiledPattern, axioms: AxiomKind, logicalAxioms: Set[OWLAxiom]): IO[DOSDPError, Seq[String]] = {
    val props = compiled.readableIdentifierProperties.to(Set)
    val (queryLogical, queryAnnotations) = Generate.axiomsOutputChoice(axioms)
    val annotationAxioms = if (queryAnnotations) Expansion.placeholderAxioms(compiled, AnnotationAxioms) else Set.empty[OWLAxiom]
    for {
      annotationTriples <- ZIO.foreach(annotationAxioms.to(Seq))(triplesForAxiom(_, props)).map(_.flatten)
      axiomTriples <- if (queryLogical) ZIO.foreach(logicalAxioms.to(Seq))(triplesForAxiom(_, props)).map(_.flatten) else ZIO.succeed(Nil)
      varExpressions <- VarRangeExpressions.varExpressions(compiled)
      variableTriples = varExpressions.toSeq.flatMap {
        case (_, Thing)                                                               =>
          Seq.empty // relationships to owl:Thing are not typically explicit in the ontology
        case (variable, named: OWLClass)                                              =>
          Seq(s"?${DOSDP.processedVariable(variable)} rdfs:subClassOf* <${named.getIRI}> .")
        case (variable, ObjectUnionOf(operands)) if operands.forall(_.isNamed)        =>
          Seq(operands.map(named => s"{ ?${DOSDP.processedVariable(variable)} rdfs:subClassOf* <${named.asOWLClass().getIRI}> . }")
            .mkString(" UNION "))
        case (variable, ObjectIntersectionOf(operands)) if operands.forall(_.isNamed) =>
          operands.map(named => s"?${DOSDP.processedVariable(variable)} rdfs:subClassOf* <${named.asOWLClass().getIRI}> . ")
        case (variable, expression)                                                   =>
          val pss = new ParameterizedSparqlString()
          pss.appendNode(expression.asOMN)
          val sanitizedExpression = pss.toString
          Seq(s"?${DOSDP.processedVariable(variable)} rdfs:subClassOf $sanitizedExpression .")
      }
      labelTriples = axiomVariables(logicalAxioms).map(v => s"OPTIONAL { $v rdfs:label ${v}__label . }")
    } yield annotationTriples ++ axiomTriples ++ variableTriples ++ labelTriples
  }

  def triplesForAxiom(axiom: OWLAxiom, readableIdentifierProperties: Set[OWLAnnotationProperty]): UIO[Seq[String]] = axiom match {
    case subClassOf: OWLSubClassOfAxiom                   =>
      val (subClass, subClassTriples) = triplesForClassExpression(subClassOf.getSubClass)
      val (superClass, superClassTriples) = triplesForClassExpression(subClassOf.getSuperClass)
      ZIO.succeed(Seq(s"$subClass rdfs:subClassOf $superClass .") ++ subClassTriples ++ superClassTriples)
    case equivalentTo: OWLEquivalentClassesAxiom          =>
      ZIO.logWarning("More than two operands or missing named class in equivalent class axiom unexpected")
        .when(!equivalentTo.containsNamedEquivalentClass || (equivalentTo.getClassExpressions.size > 2)) *>
        ZIO.succeed {
          (for {
            named <- equivalentTo.getNamedClasses.asScala.headOption
            expression <- equivalentTo.getClassExpressionsMinus(named).asScala.headOption
          } yield {
            val (namedClass, namedClassTriples) = triplesForClassExpression(named)
            val (equivClass, equivClassTriples) = triplesForClassExpression(expression)
            Seq(s"$namedClass owl:equivalentClass $equivClass .") ++ namedClassTriples ++ equivClassTriples
          }).toSeq.flatten
        }
    case disjointWith: OWLDisjointClassesAxiom            =>
      ZIO.logWarning("More than two operands or missing named class in disjointness axiom unexpected")
        .when(!disjointWith.getClassExpressions.asScala.forall(_.isAnonymous) || (disjointWith.getClassExpressions.size > 2)) *>
        ZIO.succeed {
          (for {
            named <- disjointWith.getClassExpressions.asScala.find(!_.isAnonymous)
            expression <- disjointWith.getClassExpressionsMinus(named).asScala.headOption
          } yield {
            val (namedClass, namedClassTriples) = triplesForClassExpression(named)
            val (equivClass, equivClassTriples) = triplesForClassExpression(expression)
            Seq(s"$namedClass owl:disjointWith $equivClass .") ++ namedClassTriples ++ equivClassTriples
          }).toSeq.flatten
        }
    case annotationAssertion: OWLAnnotationAssertionAxiom =>
      val (subject, subjecTriples) = triplesForClassExpression(factory.getOWLClass(annotationAssertion.getSubject.asInstanceOf[IRI]))
      val property = s"<${annotationAssertion.getProperty.getIRI}>"
      val (value, valueTriples) = triplesForAnnotationValue(annotationAssertion.getValue, readableIdentifierProperties)
      ZIO.succeed(Seq(s"$subject $property $value .") ++ subjecTriples ++ valueTriples)

  }

  private val DOSDPVariableIRIMatch = s"\\b${DOSDP.variablePrefix}([A-Za-z0-9_]+)\\b".r

  /**
   * A literal whose entire lexical form is a `$<name>` data placeholder —
   * the shape `NormalizedListAnnotation` emits for a `data_list_var` filler,
   * and that `Expansion.expandRow` substitutes at row time. SPARQL must
   * treat this as a captured variable, not as a literal text to regex-match,
   * or queries for list-style annotations (`exact_synonym`, `xref`, custom
   * value-list annotations) would require the real value to literally equal
   * `$name` and never bind.
   */
  private val DOSDPLiteralPlaceholder = """\A\$([A-Za-z0-9_]+)\z""".r

  private def escape(text: String): String = {
    val pss = new ParameterizedSparqlString()
    pss.appendLiteral(text)
    pss.toString
  }

  def triplesForAnnotationValue(annotationValue: OWLAnnotationValue, readableIdentifierProperties: Set[OWLAnnotationProperty]): (String, Seq[String]) = annotationValue match {
    case iri: IRI            =>
      iri.toString match {
        case DOSDPVariable(variable) => (s"?$variable", List(s"FILTER(isIRI(?$variable))"))
        case _                       => (s"<$iri>", Nil)
      }
    case literal: OWLLiteral =>
      literal.getLiteral match {
        case DOSDPLiteralPlaceholder(variable) =>
          (s"?$variable", List(s"FILTER(isLiteral(?$variable))"))
        case text                              =>
          val node = genVar
          val valueRegex = DOSDPVariableIRIMatch.pattern.split(text, -1).map(Pattern.quote).mkString("(.+)")
          val variableNames = DOSDPVariableIRIMatch.findAllMatchIn(text).toList.map(_.group(1))
          val predicates = readableIdentifierProperties.map(p => s"<${p.getIRI}>").mkString(" ")
          val varPatterns = variableNames.zipWithIndex.flatMap { case (variableName, index) =>
            val predicateVar = s"${variableName}__match_property"
            val variableMatchLabel = genVar
            List(
              s"""BIND((REPLACE($node, ${escape(valueRegex)}, "$$${index + 1}")) AS $variableMatchLabel)""",
              s"VALUES ?$predicateVar { $predicates }",
              s"?$variableName ?$predicateVar $variableMatchLabel ."
            )
          }
          (node, s"FILTER(REGEX($node, ${escape(valueRegex)}))" :: varPatterns)
      }
  }

  def triplesForClassExpression(expression: OWLClassExpression): (String, Seq[String]) = expression match {
    case named: OWLClass              =>
      named.getIRI.toString match {
        case DOSDPVariable(variable) => (s"?$variable", List(s"FILTER(isIRI(?$variable))"))
        case _                       => (s"<${named.getIRI}>", Nil)
      }
    case svf: OWLObjectSomeValuesFrom =>
      val node = genVar
      val (filler, fillerTriples) = triplesForClassExpression(svf.getFiller)
      (node, Seq(
        //this assumes named object property
        s"$node owl:onProperty <${svf.getProperty.asOWLObjectProperty.getIRI}> .",
        s"$node owl:someValuesFrom $filler .") ++ fillerTriples)
    case avf: OWLObjectAllValuesFrom  =>
      val node = genVar
      val (filler, fillerTriples) = triplesForClassExpression(avf.getFiller)
      (node, Seq(
        //this assumes named object property
        s"$node owl:onProperty <${avf.getProperty.asOWLObjectProperty.getIRI}> .",
        s"$node owl:allValuesFrom $filler .") ++ fillerTriples)
    case and: OWLObjectIntersectionOf =>
      val node = genVar
      val (intersectionTriples, operandTriplesList, operands) = and.getOperands.iterator().asScala.toList.map { o =>
        val (operand, operandTriples) = triplesForClassExpression(o)
        (s"$node owl:intersectionOf/rdf:rest*/rdf:first $operand .", operandTriples, operand)
      }.unzip3
      val filters = operands.combinations(2).map { pair =>
        s"FILTER(${pair.head} != ${pair.last})"
      }
      val listLengthTriple = s"$node owl:intersectionOf/${and.getOperands.asScala.toSeq.map(_ => "rdf:rest").mkString("/")} rdf:nil ."
      (node, listLengthTriple :: intersectionTriples ::: operandTriplesList.flatten ::: filters.toList)
    case or: OWLObjectUnionOf         =>
      val node = genVar
      val (unionTriples, operandTriplesList, operands) = or.getOperands.asScala.map { o =>
        val (operand, operandTriples) = triplesForClassExpression(o)
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
