package org.monarchinitiative.dosdp

import zio.logging._
import zio.test.Assertion._
import zio.test._

object RegexTest extends DefaultRunnableSpec {

  def spec = suite("Missing columns and cell values")(
    testM("RegexSub should replace values correctly") {
      val definition = RegexSub("regulated_activity", "regulated_activity_munged", "(.+) activity", raw"\1")
      val eDefinition = ExpandedRegexSub(definition)
      for {
        a <- eDefinition.substitute("kinase activity")
        b <- eDefinition.substitute("foo")
        c <- eDefinition.substitute("activity kinase")
      } yield assertTrue(a == "kinase") && assertTrue(b == "foo") && assertTrue(c == "activity kinase")
    },
    testM("RegexSub should replace multiple values correctly") {
      val definition = RegexSub("regulated_activity", "regulated_activity_munged", "(.+) activity (.+)", raw"\2 and then \1")
      val eDefinition = ExpandedRegexSub(definition)
      assertM(eDefinition.substitute("kinase activity promoter"))(equalTo("promoter and then kinase"))
    }
  ).provideCustomLayer(Logging.consoleErr())

}
