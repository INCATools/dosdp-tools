package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.DOSDPError
import org.monarchinitiative.dosdp.cli.DOSDPError.logErrorFail
import org.semanticweb.owlapi.model.OWLAnnotationProperty
import zio.{Config => _, _}

/**
 * Pattern-time normalization of annotations and permutations: resolves
 * annotation property names to `OWLAnnotationProperty` values, recursively
 * normalizes nested annotations, and validates permutation specs against
 * their enclosing annotation's `vars` list.
 *
 * Pure with respect to row bindings — every call here depends only on the
 * pattern YAML and the entity checker built from it. Run once per pattern;
 * the results feed into row-time expansion without further property lookups.
 */
private[dosdp] object AnnotationCompiler {

  def normalizeAnnotation(annotation: Annotations, checker: SafeOWLEntityChecker): ZIO[Any, DOSDPError, NormalizedAnnotation] = annotation match {
    case PrintfAnnotation(anns, ap, text, vars, overrideColumn, multiClause, perms) =>
      for {
        prop <- checker.getOWLAnnotationProperty(ap).orElse(logErrorFail(s"No annotation property binding: $ap"))
        annotations <- ZIO.foreach(anns.to(List).flatten)(normalizeAnnotation(_, checker))
        _ <- validatePermutationVars(perms.getOrElse(List.empty), vars.getOrElse(List.empty))
        normalizedPerms <- ZIO.foreach(perms.getOrElse(List.empty))(normalizePermutation(_, checker))
      } yield NormalizedPrintfAnnotation(prop, text, vars, multiClause, overrideColumn, annotations.to(Set), normalizedPerms)
    case ListAnnotation(anns, ap, value)                                            =>
      for {
        prop <- checker.getOWLAnnotationProperty(ap).orElse(logErrorFail(s"No annotation property binding: $ap"))
        annotations <- ZIO.foreach(anns.to(List).flatten)(normalizeAnnotation(_, checker))
      } yield NormalizedListAnnotation(prop, value, annotations.to(Set))
    case IRIValueAnnotation(anns, ap, varr)                                         =>
      for {
        prop <- checker.getOWLAnnotationProperty(ap).orElse(logErrorFail(s"No annotation property binding: $ap"))
        annotations <- ZIO.foreach(anns.to(List).flatten)(normalizeAnnotation(_, checker))
      } yield NormalizedIRIValueAnnotation(prop, varr, annotations.to(Set))
  }

  def normalizeOBOAnnotation(annotation: OBOAnnotations, property: OWLAnnotationProperty, overrideColumn: Option[String], checker: SafeOWLEntityChecker): ZIO[Any, DOSDPError, NormalizedAnnotation] =
    annotation match {
      case PrintfAnnotationOBO(anns, xrefs, text, vars, multiClause, perms) =>
        for {
          annotations <- ZIO.foreach(anns.to(List).flatten)(normalizeAnnotation(_, checker))
          _ <- validatePermutationVars(perms.getOrElse(List.empty), vars.getOrElse(List.empty))
          normalizedPerms <- ZIO.foreach(perms.getOrElse(List.empty))(normalizePermutation(_, checker))
        } yield NormalizedPrintfAnnotation(property, text, vars, multiClause, overrideColumn,
          annotations.to(Set) ++ xrefs.map(NormalizedListAnnotation(PrintfAnnotationOBO.Xref, _, Set.empty)),
          normalizedPerms)
      case ListAnnotationOBO(value, xrefs)                                  =>
        ZIO.succeed(NormalizedListAnnotation(property, value,
          xrefs.map(NormalizedListAnnotation(PrintfAnnotationOBO.Xref, _, Set.empty)).to(Set)))
    }

  def normalizePermutation(permutation: Permutation, checker: SafeOWLEntityChecker): ZIO[Any, DOSDPError, NormalizedPermutation] =
    for {
      props <- ZIO.foreach(permutation.annotationProperties)(apName =>
        checker.getOWLAnnotationProperty(apName).orElse(logErrorFail(s"No annotation property binding for permutation: $apName")))
    } yield NormalizedPermutation(permutation.`var`, props)

  /** Every variable named in a permutation spec must appear in the enclosing annotation's `vars` list. */
  def validatePermutationVars(permutations: List[Permutation], vars: List[String]): ZIO[Any, DOSDPError, Unit] = {
    val varSet = vars.toSet
    val invalidVars = permutations.map(_.`var`).filterNot(varSet.contains)
    if (invalidVars.isEmpty) ZIO.unit
    else logErrorFail(s"Permutation vars not found in annotation vars list: ${invalidVars.mkString(", ")}. Available vars: ${vars.mkString(", ")}")
  }

  /**
   * The OWL annotation properties referenced by any `permutation` entry anywhere in this pattern.
   * Names that fail to resolve are silently dropped here; `normalizePermutation` is the canonical
   * place where an unresolved reference becomes a validation error.
   */
  def permutationAnnotationProperties(dosdp: DOSDP, checker: SafeOWLEntityChecker): URIO[Any, Set[OWLAnnotationProperty]] = {
    val names = collectPermutationPropertyNames(dosdp)
    ZIO.foreach(names.toList)(name => checker.getOWLAnnotationProperty(name).option)
      .map(_.flatten.toSet)
  }

  private def collectPermutationPropertyNames(dosdp: DOSDP): Set[String] = {
    def fromAnnotations(annos: List[Annotations]): List[Permutation] =
      annos.flatMap {
        case pfa: PrintfAnnotation => pfa.permutations.toList.flatten ++ fromAnnotations(pfa.annotations.toList.flatten)
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

}
