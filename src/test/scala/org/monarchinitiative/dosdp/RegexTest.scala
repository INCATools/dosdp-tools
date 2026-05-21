package org.monarchinitiative.dosdp

import zio.test.Assertion._
import zio.test._

object RegexTest extends ZIOSpecDefault {

  def spec = suite("Missing columns and cell values")(
    test("RegexSub should replace values correctly") {
      val eDefinition = ExpandedRegexSub(RegexSub("regulated_activity", "regulated_activity_munged", "(.+) activity", raw"\1"))
      assertTrue(eDefinition.substitute("kinase activity") == "kinase") &&
        assertTrue(eDefinition.substitute("foo") == "foo") &&
        assertTrue(eDefinition.substitute("activity kinase") == "activity kinase")
    },
    test("RegexSub should replace multiple values correctly") {
      val eDefinition = ExpandedRegexSub(RegexSub("regulated_activity", "regulated_activity_munged", "(.+) activity (.+)", raw"\2 and then \1"))
      assert(eDefinition.substitute("kinase activity promoter"))(equalTo("promoter and then kinase"))
    }
  )

}
