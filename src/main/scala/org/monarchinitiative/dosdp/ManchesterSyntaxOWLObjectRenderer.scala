package org.monarchinitiative.dosdp

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.io.OWLObjectRenderer
import org.semanticweb.owlapi.manchestersyntax.renderer.ManchesterOWLSyntaxObjectRenderer
import org.semanticweb.owlapi.model.{IRI, OWLDataVisitor, OWLLiteral, OWLObject}
import org.semanticweb.owlapi.util.{ShortFormProvider, SimpleShortFormProvider}
import org.semanticweb.owlapi.vocab.XSDVocabulary

import java.io.{StringWriter, Writer}

/**
 * Manchester syntax renderer that applies short forms to bare IRIs.
 *
 * Adapted from owl-diff's `ManchesterSyntaxOWLObjectRenderer`. The
 * literal escaping is implemented locally so this
 * project does not need owl-diff's transitive `commons-text` dependency.
 */
class ManchesterSyntaxOWLObjectRenderer extends OWLObjectRenderer {

  import ManchesterSyntaxOWLObjectRenderer._

  private object WriterDelegate extends Writer {

    private var delegate = new StringWriter()

    def reset(): Unit = delegate = new StringWriter()

    override def toString: String = delegate.getBuffer.toString

    override def close(): Unit = delegate.close()

    override def flush(): Unit = delegate.flush()

    override def write(cbuf: Array[Char], off: Int, len: Int): Unit = delegate.write(cbuf, off, len)

  }

  private var renderer: ManchesterOWLSyntaxObjectRenderer = new BetterIRIRenderer(WriterDelegate, new SimpleShortFormProvider())

  override def render(obj: OWLObject): String = synchronized {
    WriterDelegate.reset()
    obj.accept(renderer)
    WriterDelegate.toString
  }

  override def setShortFormProvider(shortFormProvider: ShortFormProvider): Unit = synchronized {
    renderer = new BetterIRIRenderer(WriterDelegate, shortFormProvider)
  }

}

object ManchesterSyntaxOWLObjectRenderer {

  private val factory = OWLManager.getOWLDataFactory

  private def escapeHtmlText(text: String): String =
    text.flatMap {
      case '&'  => "&amp;"
      case '<'  => "&lt;"
      case '>'  => "&gt;"
      case '"'  => "&quot;"
      case char => char.toString
    }

  class BetterIRIRenderer(writer: Writer, entityShortFormProvider: ShortFormProvider) extends ManchesterOWLSyntaxObjectRenderer(writer, entityShortFormProvider) {

    override def visit(iri: IRI): Unit = visit(factory.getOWLClass(iri))

    override def visit(node: OWLLiteral): Unit = {

      // xsd:decimal is the default datatype for literal forms like "33.3"
      // with no specified datatype
      if (XSDVocabulary.DECIMAL.getIRI.equals(node.getDatatype.getIRI)) {
        write(node.getLiteral)
      } else if (node.getDatatype.isFloat) {
        write(node.getLiteral)
        write("f")
      } else if (node.getDatatype.isInteger) {
        write(node.getLiteral)
      } else if (node.getDatatype.isBoolean) {
        write(node.getLiteral)
      } else {
        pushTab(getIndent)
        write("\"")
        write(escapeHtmlText(node.getLiteral))
        write("\"")
        if (node.hasLang()) {
          write("@")
          write(node.getLang)
        } else if (!node.isRDFPlainLiteral) {
          write("^^")
          node.getDatatype.accept(this: OWLDataVisitor)
        }
        popTab()
      }
    }

  }

}
