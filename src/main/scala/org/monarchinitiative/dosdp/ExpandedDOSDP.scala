package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.AxiomType
import org.monarchinitiative.dosdp.cli.DOSDPError
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.manchestersyntax.parser.{ManchesterOWLSyntaxClassExpressionParser, ManchesterOWLSyntaxInlineAxiomParser}
import org.semanticweb.owlapi.model._

import scala.jdk.CollectionConverters._
import scala.util.matching.Regex.Match
import cats.implicits._

/**
 * Wraps a DOSDP data structure with functionality dependent on expanding IDs into IRIs
 */
final case class ExpandedDOSDP(dosdp: DOSDP, prefixes: PartialFunction[String, String]) {

  lazy val checker = new DOSDPEntityChecker(dosdp, prefixes)
  lazy val safeChecker = new SafeOWLEntityChecker(checker)
  private lazy val expressionParser = new ManchesterOWLSyntaxClassExpressionParser(OWLManager.getOWLDataFactory, checker)
  private lazy val axiomParser = new ManchesterOWLSyntaxInlineAxiomParser(OWLManager.getOWLDataFactory, checker)

  private type Bindings = Map[String, Binding]

  val substitutions: Seq[ExpandedRegexSub] = dosdp.substitutions.toSeq.flatten.map(ExpandedRegexSub)

  def allObjectProperties: Map[String, String] = dosdp.relations.getOrElse(Map.empty) ++ dosdp.objectProperties.getOrElse(Map.empty)

  def equivalentToExpression(logicalBindings: Option[Map[String, Binding]], annotationBindings: Option[Map[String, Binding]]): Either[DOSDPError, Option[(OWLClassExpression, Set[OWLAnnotation])]] = {
    val result = for {
      eq <- dosdp.equivalentTo
      ce <- expressionFor(eq, logicalBindings)
    } yield annotationsFor(eq, annotationBindings, logicalBindings).map { anns =>
      ce -> anns
    }
    result.sequence
  }

  def subClassOfExpression(logicalBindings: Option[Map[String, Binding]], annotationBindings: Option[Map[String, Binding]]): Either[DOSDPError, Option[(OWLClassExpression, Set[OWLAnnotation])]] = {
    val result = for {
      sco <- dosdp.subClassOf
      ce <- expressionFor(sco, logicalBindings)
    } yield annotationsFor(sco, annotationBindings, logicalBindings).map { anns =>
      ce -> anns
    }
    result.sequence
  }

  def disjointWithExpression(logicalBindings: Option[Map[String, Binding]], annotationBindings: Option[Map[String, Binding]]): Either[DOSDPError, Option[(OWLClassExpression, Set[OWLAnnotation])]] = {
    val result = for {
      dw <- dosdp.disjointWith
      ce <- expressionFor(dw, logicalBindings)
    } yield annotationsFor(dw, annotationBindings, logicalBindings).map { anns =>
      ce -> anns
    }
    result.sequence
  }

  def gciAxiom(logicalBindings: Option[Map[String, Binding]], annotationBindings: Option[Map[String, Binding]]): Either[DOSDPError, Option[(OWLAxiom, Set[OWLAnnotation])]] =
    (for {
      gci <- dosdp.GCI
      ax <- axiomFor(gci, logicalBindings)
    } yield ax -> annotationsFor(gci, annotationBindings, logicalBindings)) match {
      case Some((axiom, Right(annotations))) => Right(Some(axiom -> annotations))
      case Some((_, Left(error)))            => Left(error)
      case None                              => Right(None)
    }

  def logicalAxioms(logicalBindings: Option[Map[String, Binding]], annotationBindings: Option[Map[String, Binding]]): Either[DOSDPError, Set[OWLAxiom]] = {
    val axioms = for {
      axiomDefs <- dosdp.logical_axioms.toList
      axiomDef <- axiomDefs
      defTerm = definedTerm(logicalBindings)
      maybeAnnotations = annotationsFor(axiomDef, annotationBindings, logicalBindings)
    } yield maybeAnnotations.map { annotations =>
      axiomDef.axiom_type match {
        case AxiomType.EquivalentTo => expressionFor(axiomDef, logicalBindings).map(ce => EquivalentClasses(annotations.to(Seq): _*)(defTerm, ce))
        case AxiomType.SubClassOf   => expressionFor(axiomDef, logicalBindings).map(ce => SubClassOf(annotations, defTerm, ce))
        case AxiomType.DisjointWith => expressionFor(axiomDef, logicalBindings).map(ce => DisjointClasses(annotations.to(Seq): _*)(defTerm, ce))
        case AxiomType.GCI          => axiomFor(axiomDef, logicalBindings).map(ax => ax.getAnnotatedAxiom(annotations.asJava))
      }
    }
    axioms.sequence.map(_.to(Set).flatten)
  }

  private val term = Class(DOSDP.variableToIRI(DOSDP.DefinedClassVariable))

  private def definedTerm(bindings: Option[Map[String, Binding]]): OWLClass = (for {
    actualBindings <- bindings
    defClass <- actualBindings.collect { case (key, SingleValue(value)) => (key, SingleValue(value)) }.get(DOSDP.DefinedClassVariable)
    iri <- Prefixes.idToIRI(defClass.value, prefixes)
  } yield Class(iri)).getOrElse(term)

  def filledLogicalAxioms(logicalBindings: Option[Map[String, Binding]], annotationBindings: Option[Map[String, Binding]]): Either[DOSDPError, Set[OWLAxiom]] = {
    val theDefinedTerm = definedTerm(logicalBindings)
    for {
      gciAxiomOpt <- gciAxiom(logicalBindings, annotationBindings)
      logicalAxs <- logicalAxioms(logicalBindings, annotationBindings)
      equivalentToOpt <- equivalentToExpression(logicalBindings, annotationBindings)
      subClassOfOpt <- subClassOfExpression(logicalBindings, annotationBindings)
      disjointWithOpt <- disjointWithExpression(logicalBindings, annotationBindings)
    } yield {
      equivalentToOpt.map { case (e, anns) => EquivalentClasses(anns.toSeq: _*)(theDefinedTerm, e) }.toSet ++
        subClassOfOpt.map { case (e, anns) => SubClassOf(anns, theDefinedTerm, e) }.toSet ++
        disjointWithOpt.map { case (e, anns) => DisjointClasses(anns.toSeq: _*)(theDefinedTerm, e) }.toSet ++
        gciAxiomOpt.map { case (axiom, anns) => axiom.getAnnotatedAxiom(anns.asJava) }.to(Set) ++ logicalAxs
    }
  }

  def varExpressions: Map[String, OWLClassExpression] = {
    val vars = dosdp.vars.getOrElse(Map.empty)
    vars.view.mapValues(expressionParser.parse).toMap
  }

  def listVarExpressions: Map[String, OWLClassExpression] = {
    val vars = dosdp.list_vars.getOrElse(Map.empty)
    vars.view.mapValues(expressionParser.parse).toMap
  }

  private def expressionFor(template: PrintfText, bindings: Option[Map[String, Binding]]): Option[OWLClassExpression] =
    template.replaced(bindings).map(expressionParser.parse)

  private def axiomFor(template: PrintfText, bindings: Option[Map[String, Binding]]): Option[OWLAxiom] =
    template.replaced(bindings).map(axiomParser.parse)

  private def annotationsFor(element: PrintfText, annotationBindings: Option[Map[String, Binding]], logicalBindings: Option[Map[String, Binding]]): Either[DOSDPError, Set[OWLAnnotation]] = {
    val owlAnnotations = for {
      annotations <- element.annotations.to(List)
      annotation <- annotations
      maybeNormalizedAnnotation = normalizeAnnotation(annotation)
    } yield maybeNormalizedAnnotation.map(normalizedAnnotation => translateAnnotations(normalizedAnnotation, annotationBindings, logicalBindings))
    owlAnnotations.sequence.map(_.to(Set).flatten)
  }

  def filledAnnotationAxioms(annotationBindings: Option[Bindings], logicalBindings: Option[Bindings]): Either[DOSDPError, Set[OWLAnnotationAssertionAxiom]] = {
    val definedTerm = (for {
      actualBindings <- annotationBindings
      SingleValue(value) <- actualBindings.get(DOSDP.DefinedClassVariable)
      iri <- Prefixes.idToIRI(value, prefixes)
    } yield Class(iri)).getOrElse(term)
    for {
      oboAnns <- normalizedOBOAnnotations
      otherAnns <- dosdp.annotations.to(List).flatten.map(normalizeAnnotation).sequence
      allNormalizedAnns = oboAnns ++ otherAnns
    } yield {
      for {
        normalizedAnnotationField <- allNormalizedAnns
        annotation <- translateAnnotations(normalizedAnnotationField, annotationBindings, logicalBindings)
      } yield AnnotationAssertion(annotation.getAnnotations.asScala.toSet, annotation.getProperty, definedTerm, annotation.getValue)
    }
  }

  private def normalizedOBOAnnotations: Either[DOSDPError, Set[NormalizedAnnotation]] = {
    import PrintfAnnotationOBO._
    Map(
      dosdp.name.toSet -> (Name, Some(overrides(Name))),
      dosdp.comment.toSet -> (Comment, Some(overrides(Comment))),
      dosdp.`def`.toSet -> (Def, Some(overrides(Def))),
      dosdp.namespace.toSet -> (Namespace, Some(overrides(Namespace))),
      dosdp.exact_synonym.toSet -> (ExactSynonym, None),
      dosdp.narrow_synonym.toSet -> (NarrowSynonym, None),
      dosdp.related_synonym.toSet -> (RelatedSynonym, None),
      dosdp.broad_synonym.toSet -> (BroadSynonym, None),
      dosdp.xref.toSet -> (Xref, None),
      dosdp.generated_synonyms.toSet.flatten -> (ExactSynonym, Some(overrides(ExactSynonym))),
      dosdp.generated_narrow_synonyms.toSet.flatten -> (NarrowSynonym, Some(overrides(NarrowSynonym))),
      dosdp.generated_broad_synonyms.toSet.flatten -> (BroadSynonym, Some(overrides(BroadSynonym))),
      dosdp.generated_related_synonyms.toSet.flatten -> (RelatedSynonym, Some(overrides(RelatedSynonym)))).flatMap {
      case (value, (property, overrideColumnOpt)) => value.map(ann => normalizeOBOAnnotation(ann, property, overrideColumnOpt))
    }.to(List).sequence.map(_.to(Set))
  }

  private def translateAnnotations(annotationField: NormalizedAnnotation, annotationBindings: Option[Bindings], logicalBindings: Option[Bindings]): Set[OWLAnnotation] = annotationField match {
    case NormalizedPrintfAnnotation(prop, text, vars, multiClause, overrideColumnOpt, subAnnotations) =>
      val valueOpts = (for {
        column <- overrideColumnOpt
        bindings <- annotationBindings
        SingleValue(binding) <- bindings.get(column)
        trimmed = binding.trim
        if trimmed.nonEmpty
      } yield Seq(trimmed)).orElse(Some(printAnnotation(text, vars, multiClause, annotationBindings)))
      valueOpts.getOrElse(Seq.empty).toSet[String].map(value => Annotation(subAnnotations.flatMap(translateAnnotations(_, annotationBindings, logicalBindings)), prop, value))
    case NormalizedListAnnotation(prop, value, subAnnotations)                                        =>
      // If no variable bindings are passed in, dummy value is filled in using variable name
      val multiValBindingsOpt = annotationBindings.map(multiValueBindings)
      val bindingsMap = multiValBindingsOpt.getOrElse(Map(value -> MultiValue(Set("'$" + value + "'"))))
      val listValueOpt = bindingsMap.get(value)
      listValueOpt.toSet[MultiValue].flatMap(listValue => listValue.value.map(v => Annotation(subAnnotations.flatMap(translateAnnotations(_, annotationBindings, logicalBindings)), prop, v)))
    case NormalizedIRIValueAnnotation(prop, varr, subAnnotations)                                     =>
      val maybeIRIValue = logicalBindings.map { actualBindings =>
        for {
          SingleValue(value) <- actualBindings.get(varr)
          iri <- Prefixes.idToIRI(value, prefixes)
        } yield iri
      }.getOrElse(Some(DOSDP.variableToIRI(varr)))
      maybeIRIValue.toSet[IRI].map(iriValue => Annotation(
        subAnnotations.flatMap(translateAnnotations(_, annotationBindings, logicalBindings)),
        prop,
        iriValue))
  }

  /**
   * PrintfText tend to build concatenated text from multiValue bindings. But printing an annotation requires printing
   * a distinct text per multiValue item. This method enables calling PrintfText.replaced for each multiValue clause.
   *
   * @param text               annotation text
   * @param vars               annotation variables
   * @param multiClause        annotation multiClauses
   * @param annotationBindings variable bindings
   * @return a sequence of printed and replaced annotation texts
   */
  def printAnnotation(text: Option[String], vars: Option[List[String]], multiClause: Option[MultiClausePrintf], annotationBindings: Option[Bindings]): Seq[String] = {
    val clauseVars = for {
      mc <- multiClause.toList
      clauses <- mc.clauses.toList
      clause <- clauses
      vars <- clause.vars
    } yield vars
    val variables = vars.getOrElse(List.empty) ++ clauseVars.flatten
    val annotationRelatedMultiValueBindings = annotationBindings.getOrElse(Map.empty[String, Binding])
      .view.filterKeys(variables.contains(_)).collectFirst { case (key, MultiValue(value)) => (key, value) }
    val singleValBindings = annotationBindings.getOrElse(Map.empty[String, Binding]).collect { case (key, SingleValue(value)) => (key, SingleValue(value)) }
    annotationRelatedMultiValueBindings match {
      case None                 =>
        PrintfText.replaced(text, vars, multiClause, annotationBindings.map(singleValueBindings), quote = false).toSeq
      case Some(multiValuePair) =>
        val multiValueText = for {
          value <- multiValuePair._2
          multiText <- PrintfText.replaced(None, None, multiClause, Some(singleValBindings + (multiValuePair._1 -> SingleValue(value))), quote = false)
        } yield multiText
        multiValueText.toSeq
    }
  }

  private def normalizeAnnotation(annotation: Annotations): Either[DOSDPError, NormalizedAnnotation] = annotation match {
    case PrintfAnnotation(anns, ap, text, vars, overrideColumn, multiClause) =>
      for {
        prop <- safeChecker.getOWLAnnotationProperty(ap).toRight(DOSDPError(s"No annotation property binding: $ap"))
        annotations <- anns.to(List).flatten.map(normalizeAnnotation).sequence
      } yield NormalizedPrintfAnnotation(prop, text, vars, multiClause, overrideColumn, annotations.to(Set))
    case ListAnnotation(anns, ap, value)                                     =>
      for {
        prop <- safeChecker.getOWLAnnotationProperty(ap).toRight(DOSDPError(s"No annotation property binding: $ap"))
        annotations <- anns.to(List).flatten.map(normalizeAnnotation).sequence
      } yield NormalizedListAnnotation(prop, value, annotations.to(Set))
    case IRIValueAnnotation(anns, ap, varr)                                  =>
      for {
        prop <- safeChecker.getOWLAnnotationProperty(ap).toRight(DOSDPError(s"No annotation property binding: $ap"))
        annotations <- anns.to(List).flatten.map(normalizeAnnotation).sequence
      } yield NormalizedIRIValueAnnotation(prop, varr, annotations.to(Set))
  }

  private def normalizeOBOAnnotation(annotation: OBOAnnotations, property: OWLAnnotationProperty, overrideColumn: Option[String]): Either[DOSDPError, NormalizedAnnotation] = annotation match {
    case PrintfAnnotationOBO(anns, xrefs, text, vars, multiClause) =>
      anns.to(List).flatten.map(normalizeAnnotation).sequence.map { annotations =>
        NormalizedPrintfAnnotation(property, text, vars, multiClause, overrideColumn,
          annotations.to(Set) ++ xrefs.map(NormalizedListAnnotation(PrintfAnnotationOBO.Xref, _, Set.empty)))
      }
    case ListAnnotationOBO(value, xrefs)                           => Right(
      NormalizedListAnnotation(
        property,
        value,
        xrefs.map(NormalizedListAnnotation(PrintfAnnotationOBO.Xref, _, Set.empty)).to(Set))
    )
  }

  private def singleValueBindings(bindings: Bindings): Map[String, SingleValue] = bindings.collect { case (key, value: SingleValue) => key -> value }

  private def multiValueBindings(bindings: Bindings): Map[String, MultiValue] = bindings.collect { case (key, value: MultiValue) => key -> value }

  lazy val readableIdentifierProperties: List[OWLAnnotationProperty] = dosdp.readable_identifiers.map { identifiers =>
    identifiers.flatMap { name =>
      val prop = safeChecker.getOWLAnnotationProperty(name)
      if (prop.isEmpty) scribe.error(s"No annotation property mapping for '$name'")
      prop
    }
  }.getOrElse(RDFSLabel :: Nil)

  private sealed trait NormalizedAnnotation {
    def property: OWLAnnotationProperty

    def subAnnotations: Set[NormalizedAnnotation]
  }

  private case class NormalizedPrintfAnnotation(property: OWLAnnotationProperty, text: Option[String], vars: Option[List[String]], multiClause: Option[MultiClausePrintf], overrideColumn: Option[String], subAnnotations: Set[NormalizedAnnotation]) extends NormalizedAnnotation

  private case class NormalizedListAnnotation(property: OWLAnnotationProperty, value: String, subAnnotations: Set[NormalizedAnnotation]) extends NormalizedAnnotation

  private case class NormalizedIRIValueAnnotation(property: OWLAnnotationProperty, `var`: String, subAnnotations: Set[NormalizedAnnotation]) extends NormalizedAnnotation

}

final case class ExpandedRegexSub(regexSub: RegexSub) {

  private val groupFinder = raw"\\(\d+)".r

  private val regex = regexSub.`match`.r

  def substitute(value: String): String = {
    val valueMatchOpt = regex.findFirstMatchIn(value)
    val substitutedOpt = valueMatchOpt.map { valueMatch =>
      groupFinder.replaceAllIn(regexSub.sub, (placeholder: Match) => {
        val group = placeholder.group(1).toInt
        valueMatch.group(group)
      })
    }
    substitutedOpt match {
      case Some(substitution) => substitution
      case None               =>
        scribe.warn(s"Regex sub '$regexSub' did not match on '$value'")
        value
    }
  }

  def expandBindings(bindings: Map[String, Binding]): Map[String, Binding] = {
    val substituted: Option[(String, Binding)] = bindings.get(regexSub.in).map {
      case SingleValue(value) => regexSub.out -> SingleValue(substitute(value))
      case MultiValue(values) => regexSub.out -> MultiValue(values.map(substitute))
    }
    bindings ++ substituted
  }

}
