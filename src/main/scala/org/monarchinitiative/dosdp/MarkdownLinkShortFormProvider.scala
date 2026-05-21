package org.monarchinitiative.dosdp

import org.semanticweb.owlapi.model.OWLEntity
import org.semanticweb.owlapi.util.ShortFormProvider

/**
 * Short-form provider that renders OWL entities as Markdown links.
 *
 * Adapted from owl-diff's `MarkdownLinkShortFormProvider`. Keeping
 * this local avoids depending on all of `owl-diff` for
 * one small docs-rendering helper.
 */
class MarkdownLinkShortFormProvider(labelProvider: ShortFormProvider) extends ShortFormProvider {

  override def getShortForm(entity: OWLEntity): String = {
    val label = labelProvider.getShortForm(entity)
    val iri = entity.getIRI.toString
    s"[$label]($iri)"
  }

  override def dispose(): Unit = ()

}
