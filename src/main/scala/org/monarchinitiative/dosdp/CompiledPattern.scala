package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.DOSDPError
import org.semanticweb.owlapi.model.{IRI, OWLAnnotationProperty}
import zio._
import zio.logging.Logging

/**
 * A pattern after pattern-time resolution: every annotation property name
 * has been looked up against the pattern's dictionaries, sub-annotations
 * have been recursively normalized, and permutation specs have been
 * validated.
 *
 * Manchester text in the logical templates is NOT parsed here — those
 * expressions carry variable placeholders and only become well-formed once
 * a row's bindings are applied. The compiled template is held verbatim
 * (`PrintfOWL` / `PrintfOWLConvenience`) for the row-time parser to consume.
 */
private[dosdp] final case class CompiledPattern(
  source: DOSDP,
  prefixes: PartialFunction[String, String],
  patternIRI: Option[IRI],
  equivalentTo: Option[CompiledExpression],
  subClassOf: Option[CompiledExpression],
  disjointWith: Option[CompiledExpression],
  gci: Option[CompiledExpression],
  logicalAxioms: List[CompiledLogicalAxiom],
  oboAnnotations: Set[NormalizedAnnotation],
  annotations: Set[NormalizedAnnotation],
  readableIdentifierProperties: List[OWLAnnotationProperty],
  permutationProperties: Set[OWLAnnotationProperty],
  substitutions: Seq[ExpandedRegexSub],
  dataVarNames: Set[String]
)

private[dosdp] final case class CompiledExpression(
  template: PrintfOWLConvenience,
  annotations: Set[NormalizedAnnotation]
)

private[dosdp] final case class CompiledLogicalAxiom(
  template: PrintfOWL,
  annotations: Set[NormalizedAnnotation]
)

private[dosdp] object PatternCompiler {

  import PrintfAnnotationOBO._

  def compile(dosdp: DOSDP, prefixes: PartialFunction[String, String]): ZIO[Logging, DOSDPError, CompiledPattern] = {
    val safeChecker = new SafeOWLEntityChecker(new DOSDPEntityChecker(dosdp, prefixes))
    for {
      equivalentTo          <- compileExpression(dosdp.equivalentTo, safeChecker)
      subClassOf            <- compileExpression(dosdp.subClassOf, safeChecker)
      disjointWith          <- compileExpression(dosdp.disjointWith, safeChecker)
      gci                   <- compileExpression(dosdp.GCI, safeChecker)
      logicalAxioms         <- ZIO.foreach(dosdp.logical_axioms.toList.flatten)(compileLogicalAxiom(_, safeChecker))
      oboAnnotations        <- compileOBOAnnotations(dosdp, safeChecker)
      annotations           <- ZIO.foreach(dosdp.annotations.toList.flatten)(AnnotationCompiler.normalizeAnnotation(_, safeChecker)).map(_.toSet)
      readableIDProperties  <- compileReadableIdentifierProperties(dosdp, safeChecker)
      permutationProperties <- AnnotationCompiler.permutationAnnotationProperties(dosdp, safeChecker)
    } yield CompiledPattern(
      source = dosdp,
      prefixes = prefixes,
      patternIRI = dosdp.pattern_iri.flatMap(Prefixes.idToIRI(_, prefixes)),
      equivalentTo = equivalentTo,
      subClassOf = subClassOf,
      disjointWith = disjointWith,
      gci = gci,
      logicalAxioms = logicalAxioms,
      oboAnnotations = oboAnnotations,
      annotations = annotations,
      readableIdentifierProperties = readableIDProperties,
      permutationProperties = permutationProperties,
      substitutions = dosdp.substitutions.toSeq.flatten.map(ExpandedRegexSub),
      dataVarNames = dataVarNamesOf(dosdp))
  }

  private def compileExpression(templateOpt: Option[PrintfOWLConvenience], checker: SafeOWLEntityChecker): ZIO[Logging, DOSDPError, Option[CompiledExpression]] =
    ZIO.foreach(templateOpt)(template =>
      ZIO.foreach(template.annotations.toList.flatten)(AnnotationCompiler.normalizeAnnotation(_, checker))
        .map(anns => CompiledExpression(template, anns.toSet)))

  private def compileLogicalAxiom(template: PrintfOWL, checker: SafeOWLEntityChecker): ZIO[Logging, DOSDPError, CompiledLogicalAxiom] =
    ZIO.foreach(template.annotations.toList.flatten)(AnnotationCompiler.normalizeAnnotation(_, checker))
      .map(anns => CompiledLogicalAxiom(template, anns.toSet))

  private def compileOBOAnnotations(dosdp: DOSDP, checker: SafeOWLEntityChecker): ZIO[Logging, DOSDPError, Set[NormalizedAnnotation]] = {
    val fields: List[(Iterable[OBOAnnotations], OWLAnnotationProperty, Option[String])] = List(
      (dosdp.name,                                       Name,           Some(overrides(Name))),
      (dosdp.comment,                                    Comment,        Some(overrides(Comment))),
      (dosdp.`def`,                                      Def,            Some(overrides(Def))),
      (dosdp.namespace,                                  Namespace,      Some(overrides(Namespace))),
      (dosdp.exact_synonym,                              ExactSynonym,   None),
      (dosdp.narrow_synonym,                             NarrowSynonym,  None),
      (dosdp.related_synonym,                            RelatedSynonym, None),
      (dosdp.broad_synonym,                              BroadSynonym,   None),
      (dosdp.xref,                                       Xref,           None),
      (dosdp.generated_synonyms.toList.flatten,          ExactSynonym,   Some(overrides(ExactSynonym))),
      (dosdp.generated_narrow_synonyms.toList.flatten,   NarrowSynonym,  Some(overrides(NarrowSynonym))),
      (dosdp.generated_broad_synonyms.toList.flatten,    BroadSynonym,   Some(overrides(BroadSynonym))),
      (dosdp.generated_related_synonyms.toList.flatten,  RelatedSynonym, Some(overrides(RelatedSynonym))))
    ZIO.foreach(fields) { case (anns, property, overrideColumnOpt) =>
      ZIO.foreach(anns)(ann => AnnotationCompiler.normalizeOBOAnnotation(ann, property, overrideColumnOpt, checker))
    }.map(_.flatten.toSet)
  }

  private def compileReadableIdentifierProperties(dosdp: DOSDP, checker: SafeOWLEntityChecker): ZIO[Logging, DOSDPError, List[OWLAnnotationProperty]] =
    ZIO.foreach(dosdp.readable_identifiers) { identifiers =>
      ZIO.foreach(identifiers)(name =>
        checker.getOWLAnnotationProperty(name)
          .orElse(DOSDPError.logErrorFail(s"No annotation property mapping for '$name'")))
    }.map(_.getOrElse(org.phenoscape.scowl.RDFSLabel :: Nil))

  private def dataVarNamesOf(dosdp: DOSDP): Set[String] =
    dosdp.data_vars.toSet.flatMap((m: Map[String, String]) => m.keySet) ++
      dosdp.data_list_vars.toSet.flatMap((m: Map[String, String]) => m.keySet)

}