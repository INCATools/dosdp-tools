package org.monarchinitiative.dosdp

class PrefixesTest extends UnitSpec {

  "OBO prefixes" should "be fallback" in {
    val specifiedPrefixes = Map("ex" -> "http://example.org/")
    val prefixes = specifiedPrefixes.orElse(OBOPrefixes)
    prefixes("owl") shouldEqual "http://www.w3.org/2002/07/owl#"
    prefixes("UBERON") shouldEqual "http://purl.obolibrary.org/obo/UBERON_"
    prefixes("OBO_REL") shouldEqual "http://purl.obolibrary.org/obo/OBO_REL_"
    prefixes("ex") shouldEqual "http://example.org/"
  }

}