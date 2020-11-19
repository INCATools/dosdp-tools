package org.monarchinitiative.dosdp

import zio.test.Assertion._
import zio.test._

object PrefixesTest extends DefaultRunnableSpec {

  def spec = suite("Test prefixes") {
    test("OBO prefixes should be fallback") {
      val specifiedPrefixes = Map("ex" -> "http://example.org/")
      val prefixes = specifiedPrefixes.orElse(OBOPrefixes)
      assert(prefixes("owl"))(equalTo("http://www.w3.org/2002/07/owl#")) &&
        assert(prefixes("UBERON"))(equalTo("http://purl.obolibrary.org/obo/UBERON_")) &&
        assert(prefixes("OBO_REL"))(equalTo("http://purl.obolibrary.org/obo/OBO_REL_")) &&
        assert(prefixes("ex"))(equalTo("http://example.org/"))
    }
  }

}