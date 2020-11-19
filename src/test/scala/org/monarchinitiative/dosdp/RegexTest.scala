package org.monarchinitiative.dosdp

import zio.test.Assertion._
import zio.test._

object RegexTest extends DefaultRunnableSpec {

  def spec = suite("Missing columns and cell values")(
    test("RegexSub should replace values correctly") {
      val definition = RegexSub("regulated_activity", "regulated_activity_munged", "(.+) activity", raw"\1")
      val eDefinition = ExpandedRegexSub(definition)
      assert(eDefinition.substitute("kinase activity"))(equalTo("kinase")) &&
        assert(eDefinition.substitute("foo"))(equalTo("foo")) &&
        assert(eDefinition.substitute("activity kinase"))(equalTo("activity kinase"))
    },
    test("RegexSub should replace multiple values correctly") {
      val definition = RegexSub("regulated_activity", "regulated_activity_munged", "(.+) activity (.+)", raw"\2 and then \1")
      val eDefinition = ExpandedRegexSub(definition)
      assert(eDefinition.substitute("kinase activity promoter"))(equalTo("promoter and then kinase"))
    }
  )

}
