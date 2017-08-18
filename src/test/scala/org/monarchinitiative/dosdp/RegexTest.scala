package org.monarchinitiative.dosdp

class RegexTest extends UnitSpec {

  "RegexSub" should "replace values correctly" in {
    val definition = RegexSub("regulated_activity", "regulated_activity_munged", "(.+) activity", raw"\1")
    val eDefinition = ExpandedRegexSub(definition)
    eDefinition.substitute("kinase activity") shouldEqual "kinase"
    eDefinition.substitute("foo") shouldEqual "foo"
    eDefinition.substitute("activity kinase") shouldEqual "activity kinase"
  }

  "RegexSub" should "replace multiple values correctly" in {
    val definition = RegexSub("regulated_activity", "regulated_activity_munged", "(.+) activity (.+)", raw"\2 and then \1")
    val eDefinition = ExpandedRegexSub(definition)
    eDefinition.substitute("kinase activity promoter") shouldEqual "promoter and then kinase"
  }

}