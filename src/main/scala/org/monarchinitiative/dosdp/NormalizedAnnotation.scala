package org.monarchinitiative.dosdp

import org.semanticweb.owlapi.model.OWLAnnotationProperty

/**
 * Compile-time representation of a pattern annotation: the annotation property
 * has been resolved to an `OWLAnnotationProperty`, sub-annotations have been
 * recursively normalized, and permutation specs (where applicable) have had
 * their property names resolved as well. The remaining `text` / `vars` /
 * `value` fields are still templates; binding substitution and rendering happen
 * per row.
 */
private[dosdp] sealed trait NormalizedAnnotation {

  def property: OWLAnnotationProperty

  def subAnnotations: Set[NormalizedAnnotation]

}

private[dosdp] final case class NormalizedPrintfAnnotation(
  property: OWLAnnotationProperty,
  text: Option[String],
  vars: Option[List[String]],
  multiClause: Option[MultiClausePrintf],
  overrideColumn: Option[String],
  subAnnotations: Set[NormalizedAnnotation],
  permutations: List[NormalizedPermutation]
) extends NormalizedAnnotation

private[dosdp] final case class NormalizedListAnnotation(
  property: OWLAnnotationProperty,
  value: String,
  subAnnotations: Set[NormalizedAnnotation]
) extends NormalizedAnnotation

private[dosdp] final case class NormalizedIRIValueAnnotation(
  property: OWLAnnotationProperty,
  `var`: String,
  subAnnotations: Set[NormalizedAnnotation]
) extends NormalizedAnnotation

private[dosdp] final case class NormalizedPermutation(
  `var`: String,
  annotationProperties: List[OWLAnnotationProperty]
)