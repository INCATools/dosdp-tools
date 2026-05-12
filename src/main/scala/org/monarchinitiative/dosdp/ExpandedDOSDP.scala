package org.monarchinitiative.dosdp

import cats.implicits._
import org.monarchinitiative.dosdp.AxiomType
import org.monarchinitiative.dosdp.cli.DOSDPError
import org.monarchinitiative.dosdp.cli.DOSDPError.{logError, logErrorFail}
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.manchestersyntax.parser.{ManchesterOWLSyntaxClassExpressionParser, ManchesterOWLSyntaxInlineAxiomParser}
import org.semanticweb.owlapi.model._
import zio._
import zio.logging._

import scala.jdk.CollectionConverters._
import scala.util.matching.Regex.Match

/**
 * Wraps a DOSDP data structure with functionality dependent on expanding IDs into IRIs
 */
final case class ExpandedDOSDP(dosdp: DOSDP, prefixes: PartialFunction[String, String]) {

  lazy val checker = new DOSDPEntityChecker(dosdp, prefixes)
  lazy val safeChecker = new SafeOWLEntityChecker(checker)
  private lazy val expressionParser = new ManchesterOWLSyntaxClassExpressionParser(OWLManager.getOWLDataFactory, checker)
  private lazy val axiomParser = new ManchesterOWLSyntaxInlineAxiomParser(OWLManager.getOWLDataFactory, checker)

  private type Bindings = Map[String, Binding]

  /**
   * Index mapping filler term IRIs to their annotation property values.
   * Structure: Map[FillerTermIRI, Map[AnnotationPropertyIRI, Set[AnnotationValues]]]
   */
  type PermutationIndex = Map[IRI, Map[IRI, Set[String]]]

  val substitutions: Seq[ExpandedRegexSub] = dosdp.substitutions.toSeq.flatten.map(ExpandedRegexSub)

  def allObjectProperties: Map[String, String] = dosdp.relations.getOrElse(Map.empty) ++ dosdp.objectProperties.getOrElse(Map.empty)

  // Data var fillers are numeric/string literals that get substituted into datatype
  // facets (e.g. `xsd:short[>= %s]`). They must NOT be wrapped in the Manchester-syntax
  // name-quoting that PrintfText applies to class/property name fillers — otherwise the
  // resulting literal has stray apostrophes in its lexical form and is ill-typed.
  private val dataVarNames: Set[String] =
    dosdp.data_vars.toSet.flatMap((m: Map[String, String]) => m.keySet) ++
      dosdp.data_list_vars.toSet.flatMap((m: Map[String, String]) => m.keySet)

  private def parseExpression(exp: Option[PrintfOWLConvenience], logicalBindings: Option[Map[String, Binding]], annotationBindings: Option[Map[String, Binding]]): ZIO[Logging, DOSDPError, Option[(OWLClassExpression, Set[OWLAnnotation])]] =
    ZIO.foreach(exp) { eq =>
      expressionFor(eq, logicalBindings).flatMap { ceOpt =>
        ZIO.foreach(ceOpt) { ce =>
          annotationsFor(eq, annotationBindings, logicalBindings).map { anns =>
            ce -> anns
          }
        }
      }
    }.map(_.flatten)

  def equivalentToExpression(logicalBindings: Option[Map[String, Binding]], annotationBindings: Option[Map[String, Binding]]): ZIO[Logging, DOSDPError, Option[(OWLClassExpression, Set[OWLAnnotation])]] =
    parseExpression(dosdp.equivalentTo, logicalBindings, annotationBindings)

  def subClassOfExpression(logicalBindings: Option[Map[String, Binding]], annotationBindings: Option[Map[String, Binding]]): ZIO[Logging, DOSDPError, Option[(OWLClassExpression, Set[OWLAnnotation])]] =
    parseExpression(dosdp.subClassOf, logicalBindings, annotationBindings)

  def disjointWithExpression(logicalBindings: Option[Map[String, Binding]], annotationBindings: Option[Map[String, Binding]]): ZIO[Logging, DOSDPError, Option[(OWLClassExpression, Set[OWLAnnotation])]] =
    parseExpression(dosdp.disjointWith, logicalBindings, annotationBindings)

  def gciAxiom(logicalBindings: Option[Map[String, Binding]], annotationBindings: Option[Map[String, Binding]]): ZIO[Logging, DOSDPError, Option[(OWLAxiom, Set[OWLAnnotation])]] =
    ZIO.foreach(dosdp.GCI) { gci =>
      axiomFor(gci, logicalBindings).flatMap { axOpt =>
        ZIO.foreach(axOpt) { ax =>
          annotationsFor(gci, annotationBindings, logicalBindings).map { anns =>
            ax -> anns
          }
        }
      }
    }.map(_.flatten)

  def logicalAxioms(logicalBindings: Option[Map[String, Binding]], annotationBindings: Option[Map[String, Binding]]): ZIO[Logging, DOSDPError, Set[OWLAxiom]] = {
    ZIO.foreach(dosdp.logical_axioms.toList.flatten) { axiomDef =>
      val defTerm = definedTerm(logicalBindings)
      annotationsFor(axiomDef, annotationBindings, logicalBindings).flatMap { annotations =>
        axiomDef.axiom_type match {
          case AxiomType.EquivalentTo => expressionFor(axiomDef, logicalBindings).map(ceOpt => ceOpt.map(ce => EquivalentClasses(annotations.to(Seq): _*)(defTerm, ce)))
          case AxiomType.SubClassOf   => expressionFor(axiomDef, logicalBindings).map(ceOpt => ceOpt.map(ce => SubClassOf(annotations, defTerm, ce)))
          case AxiomType.DisjointWith => expressionFor(axiomDef, logicalBindings).map(ceOpt => ceOpt.map(ce => DisjointClasses(annotations.to(Seq): _*)(defTerm, ce)))
          case AxiomType.GCI          => axiomFor(axiomDef, logicalBindings).map(axOpt => axOpt.map(ax => ax.getAnnotatedAxiom(annotations.asJava)))
        }
      }
    }.map(_.flatten.to(Set))
  }

  private val term = Class(DOSDP.variableToIRI(DOSDP.DefinedClassVariable))

  private def definedTerm(bindings: Option[Map[String, Binding]]): OWLClass = (for {
    actualBindings <- bindings
    defClass <- actualBindings.collect { case (key, SingleValue(value)) => (key, SingleValue(value)) }.get(DOSDP.DefinedClassVariable)
    iri <- Prefixes.idToIRI(defClass.value, prefixes)
  } yield Class(iri)).getOrElse(term)

  def filledLogicalAxioms(logicalBindings: Option[Map[String, Binding]], annotationBindings: Option[Map[String, Binding]]): ZIO[Logging, DOSDPError, Set[OWLAxiom]] = {
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

  private def expressionFor(template: PrintfText, bindings: Option[Map[String, Binding]]): ZIO[Logging, DOSDPError, Option[OWLClassExpression]] =
    ZIO.foreach(PrintfText.replaced(template.text, template.vars, template.multi_clause, bindings, template.shouldQuote, dataVarNames)) { text =>
      ZIO.effect(expressionParser.parse(text)).flatMapError(e => logError(s"Failed to parse class expression: $text", e))
    }

  private def axiomFor(template: PrintfText, bindings: Option[Map[String, Binding]]): ZIO[Logging, DOSDPError, Option[OWLAxiom]] =
    ZIO.foreach(PrintfText.replaced(template.text, template.vars, template.multi_clause, bindings, template.shouldQuote, dataVarNames)) { text =>
      ZIO.effect(axiomParser.parse(text)).flatMapError(e => logError(s"Failed to parse axiom: $text", e))
    }

  private def annotationsFor(element: PrintfText, annotationBindings: Option[Map[String, Binding]], logicalBindings: Option[Map[String, Binding]]): ZIO[Logging, DOSDPError, Set[OWLAnnotation]] = {
    val owlAnnotations = for {
      annotations <- element.annotations.to(List)
      annotation <- annotations
      maybeNormalizedAnnotation = normalizeAnnotation(annotation)
    } yield maybeNormalizedAnnotation.map(normalizedAnnotation => translateAnnotations(normalizedAnnotation, annotationBindings, logicalBindings))
    ZIO.collectAll(owlAnnotations).map(_.to(Set).flatten)
  }

  def filledAnnotationAxioms(annotationBindings: Option[Bindings], logicalBindings: Option[Bindings], permutationIndex: PermutationIndex = Map.empty): ZIO[Logging, DOSDPError, Set[OWLAnnotationAssertionAxiom]] = {
    val definedTerm = (for {
      actualBindings <- annotationBindings
      SingleValue(value) <- actualBindings.get(DOSDP.DefinedClassVariable)
      iri <- Prefixes.idToIRI(value, prefixes)
    } yield Class(iri)).getOrElse(term)
    for {
      oboAnns <- normalizedOBOAnnotations
      otherAnns <- ZIO.foreach(dosdp.annotations.to(List).flatten)(normalizeAnnotation)
      allNormalizedAnns = oboAnns ++ otherAnns
    } yield {
      for {
        normalizedAnnotationField <- allNormalizedAnns
        annotation <- translateAnnotations(normalizedAnnotationField, annotationBindings, logicalBindings, permutationIndex)
      } yield AnnotationAssertion(annotation.getAnnotations.asScala.toSet, annotation.getProperty, definedTerm, annotation.getValue)
    }
  }

  private def normalizedOBOAnnotations: ZIO[Logging, DOSDPError, Set[NormalizedAnnotation]] = {
    import PrintfAnnotationOBO._
    val base: Map[Set[OBOAnnotations], (OWLAnnotationProperty, Option[String])] = Map(
      dosdp.name.toSet[OBOAnnotations] -> (Name, Some(overrides(Name))),
      dosdp.comment.toSet[OBOAnnotations] -> (Comment, Some(overrides(Comment))),
      dosdp.`def`.toSet[OBOAnnotations] -> (Def, Some(overrides(Def))),
      dosdp.namespace.toSet[OBOAnnotations] -> (Namespace, Some(overrides(Namespace))),
      dosdp.exact_synonym.toSet[OBOAnnotations] -> (ExactSynonym, None),
      dosdp.narrow_synonym.toSet[OBOAnnotations] -> (NarrowSynonym, None),
      dosdp.related_synonym.toSet[OBOAnnotations] -> (RelatedSynonym, None),
      dosdp.broad_synonym.toSet[OBOAnnotations] -> (BroadSynonym, None),
      dosdp.xref.toSet[OBOAnnotations] -> (Xref, None),
      dosdp.generated_synonyms.toSet.flatten[OBOAnnotations] -> (ExactSynonym, Some(overrides(ExactSynonym))),
      dosdp.generated_narrow_synonyms.toSet.flatten[OBOAnnotations] -> (NarrowSynonym, Some(overrides(NarrowSynonym))),
      dosdp.generated_broad_synonyms.toSet.flatten[OBOAnnotations] -> (BroadSynonym, Some(overrides(BroadSynonym))),
      dosdp.generated_related_synonyms.toSet.flatten[OBOAnnotations] -> (RelatedSynonym, Some(overrides(RelatedSynonym))))
    ZIO.foreach(base.to(Iterable)) { case (value, (property, overrideColumnOpt)) =>
      ZIO.foreach(value)(ann => normalizeOBOAnnotation(ann, property, overrideColumnOpt))
    }.map(_.flatten.to(Set))
  }

  private def translateAnnotations(annotationField: NormalizedAnnotation, annotationBindings: Option[Bindings], logicalBindings: Option[Bindings], permutationIndex: PermutationIndex = Map.empty): Set[OWLAnnotation] = annotationField match {
    case NormalizedPrintfAnnotation(prop, text, vars, multiClause, overrideColumnOpt, subAnnotations, permutations) =>
      val valueOpts = (for {
        column <- overrideColumnOpt
        bindings <- annotationBindings
        SingleValue(binding) <- bindings.get(column)
        trimmed = binding.trim
        if trimmed.nonEmpty
      } yield Seq(trimmed)).orElse(Some(printAnnotationWithPermutations(text, vars, multiClause, annotationBindings, logicalBindings, permutations, permutationIndex)))
      valueOpts.getOrElse(Seq.empty).toSet[String].map(value => Annotation(subAnnotations.flatMap(translateAnnotations(_, annotationBindings, logicalBindings, permutationIndex)), prop, value))
    case NormalizedListAnnotation(prop, value, subAnnotations)                                        =>
      // If no variable bindings are passed in, dummy value is filled in using variable name
      val multiValBindingsOpt = annotationBindings.map(multiValueBindings)
      val bindingsMap = multiValBindingsOpt.getOrElse(Map(value -> MultiValue(Set("'$" + value + "'"))))
      val listValueOpt = bindingsMap.get(value)
      listValueOpt.toSet[MultiValue].flatMap(listValue => listValue.value.map(v => Annotation(subAnnotations.flatMap(translateAnnotations(_, annotationBindings, logicalBindings, permutationIndex)), prop, v)))
    case NormalizedIRIValueAnnotation(prop, varr, subAnnotations)                                     =>
      val maybeIRIValue = logicalBindings.map { actualBindings =>
        for {
          SingleValue(value) <- actualBindings.get(varr)
          iri <- Prefixes.idToIRI(value, prefixes)
        } yield iri
      }.getOrElse(Some(DOSDP.variableToIRI(varr)))
      maybeIRIValue.toSet[IRI].map(iriValue => Annotation(
        subAnnotations.flatMap(translateAnnotations(_, annotationBindings, logicalBindings, permutationIndex)),
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

  /**
   * Generates annotation texts with permutations. This extends the standard annotation generation
   * by also substituting synonym values from filler terms to generate additional annotations.
   *
   * The algorithm:
   * 1. For each variable in vars, collect all possible values:
   *    - Always include the label (from annotationBindings)
   *    - If the variable has permutation specs, also include values from the specified annotation properties
   * 2. Generate the cartesian product of all value combinations
   * 3. Format each combination using the text template
   */
  private def printAnnotationWithPermutations(
    text: Option[String],
    vars: Option[List[String]],
    multiClause: Option[MultiClausePrintf],
    annotationBindings: Option[Bindings],
    logicalBindings: Option[Bindings],
    permutations: List[NormalizedPermutation],
    permutationIndex: PermutationIndex
  ): Seq[String] = {
    val variableList = vars.getOrElse(List.empty)

    if (permutations.isEmpty || variableList.isEmpty)
      printAnnotation(text, vars, multiClause, annotationBindings)
    else {
      val permutationsByVar: Map[String, NormalizedPermutation] = permutations.map(p => p.`var` -> p).toMap

      // For each variable, collect label value (always first) plus any permutation values.
      val valueLists: List[List[String]] = variableList.map { varName =>
        val labelValue: Option[String] = for {
          bindings <- annotationBindings
          SingleValue(value) <- bindings.get(varName)
        } yield value

        val fillerIRI: Option[IRI] = for {
          bindings <- logicalBindings
          SingleValue(value) <- bindings.get(varName)
          iri <- Prefixes.idToIRI(value, prefixes)
        } yield iri

        val permutationValues: Set[String] = (for {
          perm <- permutationsByVar.get(varName)
          iri <- fillerIRI
          termAnnos <- permutationIndex.get(iri)
        } yield perm.annotationProperties.flatMap(prop => termAnnos.getOrElse(prop.getIRI, Set.empty)).toSet)
          .getOrElse(Set.empty)

        labelValue.toList ++ permutationValues.toList
      }

      cartesianProduct(valueLists).flatMap { values =>
        val bindingsForCombination = variableList.zip(values).map { case (varName, value) => varName -> SingleValue(value) }.toMap
        PrintfText.replaced(text, vars, multiClause, Some(bindingsForCombination), quote = false)
      }.distinct
    }
  }

  /**
   * Computes the cartesian product of a list of lists.
   * E.g., [[a, b], [1, 2]] => [[a, 1], [a, 2], [b, 1], [b, 2]]
   */
  private def cartesianProduct[T](lists: List[List[T]]): List[List[T]] = lists match {
    case Nil => List(Nil)
    case head :: tail =>
      val tailProduct = cartesianProduct(tail)
      for {
        h <- head
        t <- tailProduct
      } yield h :: t
  }

  private def normalizeAnnotation(annotation: Annotations): ZIO[Logging, DOSDPError, NormalizedAnnotation] = annotation match {
    case PrintfAnnotation(anns, ap, text, vars, overrideColumn, multiClause, perms) =>
      for {
        prop <- safeChecker.getOWLAnnotationProperty(ap).orElse(logErrorFail(s"No annotation property binding: $ap"))
        annotations <- ZIO.foreach(anns.to(List).flatten)(normalizeAnnotation)
        _ <- validatePermutationVars(perms.getOrElse(List.empty), vars.getOrElse(List.empty))
        normalizedPerms <- ZIO.foreach(perms.getOrElse(List.empty))(normalizePermutation)
      } yield NormalizedPrintfAnnotation(prop, text, vars, multiClause, overrideColumn, annotations.to(Set), normalizedPerms)
    case ListAnnotation(anns, ap, value)                                     =>
      for {
        prop <- safeChecker.getOWLAnnotationProperty(ap).orElse(logErrorFail(s"No annotation property binding: $ap"))
        annotations <- ZIO.foreach(anns.to(List).flatten)(normalizeAnnotation)
      } yield NormalizedListAnnotation(prop, value, annotations.to(Set))
    case IRIValueAnnotation(anns, ap, varr)                                  =>
      for {
        prop <- safeChecker.getOWLAnnotationProperty(ap).orElse(logErrorFail(s"No annotation property binding: $ap"))
        annotations <- ZIO.foreach(anns.to(List).flatten)(normalizeAnnotation)
      } yield NormalizedIRIValueAnnotation(prop, varr, annotations.to(Set))
  }

  private def normalizeOBOAnnotation(annotation: OBOAnnotations, property: OWLAnnotationProperty, overrideColumn: Option[String]): ZIO[Logging, DOSDPError, NormalizedAnnotation] =
    annotation match {
      case PrintfAnnotationOBO(anns, xrefs, text, vars, multiClause, perms) =>
        for {
          annotations <- ZIO.foreach(anns.to(List).flatten)(normalizeAnnotation)
          _ <- validatePermutationVars(perms.getOrElse(List.empty), vars.getOrElse(List.empty))
          normalizedPerms <- ZIO.foreach(perms.getOrElse(List.empty))(normalizePermutation)
        } yield NormalizedPrintfAnnotation(property, text, vars, multiClause, overrideColumn,
          annotations.to(Set) ++ xrefs.map(NormalizedListAnnotation(PrintfAnnotationOBO.Xref, _, Set.empty)),
          normalizedPerms)
      case ListAnnotationOBO(value, xrefs)                           => ZIO.succeed(
        NormalizedListAnnotation(
          property,
          value,
          xrefs.map(NormalizedListAnnotation(PrintfAnnotationOBO.Xref, _, Set.empty)).to(Set))
      )
    }

  private def normalizePermutation(permutation: Permutation): ZIO[Logging, DOSDPError, NormalizedPermutation] = {
    for {
      props <- ZIO.foreach(permutation.annotationProperties) { apName =>
        safeChecker.getOWLAnnotationProperty(apName).orElse(logErrorFail(s"No annotation property binding for permutation: $apName"))
      }
    } yield NormalizedPermutation(permutation.`var`, props)
  }

  /**
   * Validates that all permutation vars reference variables in the annotation's vars list.
   */
  private def validatePermutationVars(permutations: List[Permutation], vars: List[String]): ZIO[Logging, DOSDPError, Unit] = {
    val varSet = vars.toSet
    val invalidVars = permutations.map(_.`var`).filterNot(varSet.contains)
    if (invalidVars.isEmpty) ZIO.unit
    else logErrorFail(s"Permutation vars not found in annotation vars list: ${invalidVars.mkString(", ")}. Available vars: ${vars.mkString(", ")}")
  }

  private def singleValueBindings(bindings: Bindings): Map[String, SingleValue] = bindings.collect { case (key, value: SingleValue) => key -> value }

  private def multiValueBindings(bindings: Bindings): Map[String, MultiValue] = bindings.collect { case (key, value: MultiValue) => key -> value }

  val readableIdentifierProperties: ZIO[Logging, DOSDPError, List[OWLAnnotationProperty]] =
    ZIO.foreach(dosdp.readable_identifiers) { identifiers =>
      ZIO.foreach(identifiers) { name =>
        safeChecker.getOWLAnnotationProperty(name)
          .orElse(logErrorFail(s"No annotation property mapping for '$name'"))
      }
    }.map(_.getOrElse(RDFSLabel :: Nil))

  /**
   * The OWL annotation properties referenced by any `permutation` entry anywhere in this pattern.
   * Names that fail to resolve are silently dropped here; `normalizePermutation` is the canonical
   * place where an unresolved reference becomes a validation error.
   */
  val permutationAnnotationProperties: URIO[Logging, Set[OWLAnnotationProperty]] = {
    val names = collectPermutationPropertyNames(dosdp)
    ZIO.foreach(names.toList)(name => safeChecker.getOWLAnnotationProperty(name).option)
      .map(_.flatten.toSet)
  }

  private def collectPermutationPropertyNames(dosdp: DOSDP): Set[String] = {
    def fromAnnotations(annos: List[Annotations]): List[Permutation] =
      annos.flatMap {
        case pfa: PrintfAnnotation =>
          pfa.permutations.toList.flatten ++ fromAnnotations(pfa.annotations.toList.flatten)
        case _                     => Nil
      }
    def fromOBO(obo: PrintfAnnotationOBO): List[Permutation] =
      obo.permutations.toList.flatten ++ fromAnnotations(obo.annotations.toList.flatten)

    val perms =
      fromAnnotations(dosdp.annotations.toList.flatten) ++
        dosdp.name.toList.flatMap(fromOBO) ++
        dosdp.comment.toList.flatMap(fromOBO) ++
        dosdp.`def`.toList.flatMap(fromOBO) ++
        dosdp.namespace.toList.flatMap(fromOBO) ++
        dosdp.generated_synonyms.toList.flatten.flatMap(fromOBO) ++
        dosdp.generated_narrow_synonyms.toList.flatten.flatMap(fromOBO) ++
        dosdp.generated_broad_synonyms.toList.flatten.flatMap(fromOBO) ++
        dosdp.generated_related_synonyms.toList.flatten.flatMap(fromOBO)

    perms.flatMap(_.annotationProperties).toSet
  }

  private sealed trait NormalizedAnnotation {
    def property: OWLAnnotationProperty

    def subAnnotations: Set[NormalizedAnnotation]
  }

  private case class NormalizedPrintfAnnotation(property: OWLAnnotationProperty, text: Option[String], vars: Option[List[String]], multiClause: Option[MultiClausePrintf], overrideColumn: Option[String], subAnnotations: Set[NormalizedAnnotation], permutations: List[NormalizedPermutation]) extends NormalizedAnnotation

  private case class NormalizedPermutation(`var`: String, annotationProperties: List[OWLAnnotationProperty])

  private case class NormalizedListAnnotation(property: OWLAnnotationProperty, value: String, subAnnotations: Set[NormalizedAnnotation]) extends NormalizedAnnotation

  private case class NormalizedIRIValueAnnotation(property: OWLAnnotationProperty, `var`: String, subAnnotations: Set[NormalizedAnnotation]) extends NormalizedAnnotation

}

final case class ExpandedRegexSub(regexSub: RegexSub) {

  private val groupFinder = raw"\\(\d+)".r

  private val regex = regexSub.`match`.r

  def substitute(value: String): URIO[Logging, String] = {
    val valueMatchOpt = regex.findFirstMatchIn(value)
    val substitutedOpt = valueMatchOpt.map { valueMatch =>
      groupFinder.replaceAllIn(regexSub.sub, (placeholder: Match) => {
        val group = placeholder.group(1).toInt
        valueMatch.group(group)
      })
    }
    substitutedOpt match {
      case Some(substitution) =>
        ZIO.succeed(substitution)
      case None               =>
        log.info(s"Regex sub '$regexSub' did not match on '$value'").as(value)
    }
  }

  def expandBindings(bindings: Map[String, Binding]): URIO[Logging, Map[String, Binding]] =
    ZIO.foreach(bindings.get(regexSub.in)) {
      case SingleValue(value) => substitute(value).map(v => regexSub.out -> SingleValue(v))
      case MultiValue(values) => ZIO.foreach(values)(substitute).map(set => regexSub.out -> MultiValue(set))
    }.map(bindings ++ _)

}
