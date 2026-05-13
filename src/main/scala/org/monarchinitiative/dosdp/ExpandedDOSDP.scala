package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.AxiomType
import org.monarchinitiative.dosdp.cli.DOSDPError
import org.monarchinitiative.dosdp.cli.DOSDPError.logError
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntaxClassExpressionParser
import org.semanticweb.owlapi.model._
import zio._
import zio.logging._

import scala.jdk.CollectionConverters._
import scala.util.matching.Regex.Match

/**
 * Pattern-template helper used by `Query`, `Terms`, and `Docs`: wraps a
 * `CompiledPattern` and assembles its precomputed parsed expressions into
 * placeholder-form OWL axioms anchored to the `urn:dosdp:` defined-class
 * term. Also provides `filledAnnotationAxioms` for SPARQL's annotation-query
 * branch, which substitutes placeholder bindings into annotation templates.
 * Row-time expansion lives in `Expansion`.
 */
final case class ExpandedDOSDP(dosdp: DOSDP, prefixes: PartialFunction[String, String], compiled: CompiledPattern) {

  private lazy val checker = new DOSDPEntityChecker(dosdp, prefixes)
  private lazy val expressionParser = new ManchesterOWLSyntaxClassExpressionParser(OWLManager.getOWLDataFactory, checker)

  private type Bindings = Map[String, Binding]
  private type PermutationIndex = Map[IRI, Map[IRI, Set[String]]]

  private val term = Class(DOSDP.variableToIRI(DOSDP.DefinedClassVariable))

  /**
   * Pattern-template axioms with all variable slots left as `urn:dosdp:`
   * placeholder IRIs. Used by Query/Terms to drive SPARQL generation and
   * referenced-term extraction.
   */
  def filledLogicalAxioms: Set[OWLAxiom] = {
    val classExprAxioms: Set[OWLAxiom] = Set(
      compiled.equivalentTo.map(ce => EquivalentClasses(annotationsFor(ce.annotations).toSeq: _*)(term, ce.parsed): OWLAxiom),
      compiled.subClassOf.map(ce => SubClassOf(annotationsFor(ce.annotations), term, ce.parsed): OWLAxiom),
      compiled.disjointWith.map(ce => DisjointClasses(annotationsFor(ce.annotations).toSeq: _*)(term, ce.parsed): OWLAxiom),
      compiled.gci.map(ax => ax.piece.parsed.getAnnotatedAxiom(annotationsFor(ax.annotations).asJava): OWLAxiom)
    ).flatten
    classExprAxioms ++ compiled.logicalAxioms.flatMap(logicalAxiom).toSet
  }

  private def logicalAxiom(compiled: CompiledLogicalAxiom): Option[OWLAxiom] = compiled match {
    case CompiledLogicalClassAxiom(_, expr, anns) =>
      val annotations = annotationsFor(anns)
      compiled.axiomType match {
        case AxiomType.EquivalentTo => Some(EquivalentClasses(annotations.toSeq: _*)(term, expr.parsed))
        case AxiomType.SubClassOf   => Some(SubClassOf(annotations, term, expr.parsed))
        case AxiomType.DisjointWith => Some(DisjointClasses(annotations.toSeq: _*)(term, expr.parsed))
        case AxiomType.GCI          => None
      }
    case CompiledLogicalGCIAxiom(_, ax, anns) =>
      Some(ax.piece.parsed.getAnnotatedAxiom(annotationsFor(anns).asJava))
  }

  def varExpressions: ZIO[Logging, DOSDPError, Map[String, OWLClassExpression]] =
    ZIO.foreach(dosdp.vars.getOrElse(Map.empty)) { case (k, v) =>
      ZIO.effect(expressionParser.parse(v))
        .flatMapError(e => logError(s"Failed to parse class expression: $v", e))
        .map(k -> _)
    }

  def listVarExpressions: ZIO[Logging, DOSDPError, Map[String, OWLClassExpression]] =
    ZIO.foreach(dosdp.list_vars.getOrElse(Map.empty)) { case (k, v) =>
      ZIO.effect(expressionParser.parse(v))
        .flatMapError(e => logError(s"Failed to parse class expression: $v", e))
        .map(k -> _)
    }

  private def annotationsFor(normalized: Set[NormalizedAnnotation]): Set[OWLAnnotation] =
    normalized.flatMap(AnnotationTranslation.translate(_, None, None, Map.empty, prefixes))

  def filledAnnotationAxioms(annotationBindings: Option[Bindings], logicalBindings: Option[Bindings], permutationIndex: PermutationIndex = Map.empty): Set[OWLAnnotationAssertionAxiom] = {
    val definedTerm = (for {
      actualBindings <- annotationBindings
      SingleValue(value) <- actualBindings.get(DOSDP.DefinedClassVariable)
      iri <- Prefixes.idToIRI(value, prefixes)
    } yield Class(iri)).getOrElse(term)
    for {
      normalized <- compiled.oboAnnotations ++ compiled.annotations
      annotation <- AnnotationTranslation.translate(normalized, annotationBindings, logicalBindings, permutationIndex, prefixes)
    } yield AnnotationAssertion(annotation.getAnnotations.asScala.toSet, annotation.getProperty, definedTerm, annotation.getValue)
  }

}

final case class ExpandedRegexSub(regexSub: RegexSub) {

  private val groupFinder = raw"\\(\d+)".r

  private val regex = regexSub.`match`.r

  /** Apply `regexSub` to a value; if the pattern does not match, return the value unchanged. */
  def substitute(value: String): String =
    regex.findFirstMatchIn(value).map { valueMatch =>
      groupFinder.replaceAllIn(regexSub.sub, (placeholder: Match) => valueMatch.group(placeholder.group(1).toInt))
    }.getOrElse(value)

  def expandBindings(bindings: Map[String, Binding]): Map[String, Binding] =
    bindings.get(regexSub.in).map {
      case SingleValue(value) => regexSub.out -> SingleValue(substitute(value))
      case MultiValue(values) => regexSub.out -> MultiValue(values.map(substitute))
    }.fold(bindings)(bindings + _)

}
