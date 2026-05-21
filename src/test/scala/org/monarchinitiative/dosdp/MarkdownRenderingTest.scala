package org.monarchinitiative.dosdp

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{IRI, OWLEntity}
import org.semanticweb.owlapi.util.ShortFormProvider
import zio.test._

object MarkdownRenderingTest extends ZIOSpecDefault {

  private val factory = OWLManager.getOWLDataFactory

  private def shortFormProvider(shortForm: String): ShortFormProvider =
    new ShortFormProvider {
      override def getShortForm(entity: OWLEntity): String = shortForm

      override def dispose(): Unit = ()
    }

  def spec = suite("Markdown rendering")(
    test("Markdown short forms wrap labels in links") {
      val entity = factory.getOWLClass(IRI.create("http://example.org/Thing"))
      val provider = new MarkdownLinkShortFormProvider(shortFormProvider("Example thing"))

      assert(provider.getShortForm(entity))(Assertion.equalTo("[Example thing](http://example.org/Thing)"))
    },
    test("Manchester renderer applies short forms to bare IRIs") {
      val renderer = new ManchesterSyntaxOWLObjectRenderer()
      renderer.setShortFormProvider(shortFormProvider("Example thing"))

      assert(renderer.render(IRI.create("http://example.org/Thing")))(Assertion.equalTo("Example thing"))
    },
    test("Manchester renderer HTML-escapes string literals") {
      val renderer = new ManchesterSyntaxOWLObjectRenderer()
      val literal = factory.getOWLLiteral("alpha < beta & \"gamma\"")

      assert(renderer.render(literal))(Assertion.equalTo("\"alpha &lt; beta &amp; &quot;gamma&quot;\"^^string"))
    }
  )

}
