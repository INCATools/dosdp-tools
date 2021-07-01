package org.monarchinitiative.dosdp

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntaxClassExpressionParser
import zio._
import zio.test.Assertion._
import zio.test._

object ExpressionParsingTest extends DefaultRunnableSpec {

  val factory = OWLManager.getOWLDataFactory

  def spec = suite("Test expression parsing")(
    testM("Datatype restrictions should be parseable") {
      val specifiedPrefixes = Map("ex" -> "http://example.org/")
      val prefixes = specifiedPrefixes.orElse(OBOPrefixes)
      val dosdp = DOSDP.empty.copy(
        classes = Some(Map("population of Drosophila" -> "ex:1")),
        relations = Some(Map("inheres_in" -> "ex:2")),
        dataProperties = Some(Map("has_increased_mortality_rate" -> "ex:3")))
      val edosdp = ExpandedDOSDP(dosdp, prefixes)
      val expressionParser = new ManchesterOWLSyntaxClassExpressionParser(OWLManager.getOWLDataFactory, edosdp.checker)
      val result = ZIO.effect(expressionParser.parse("'inheres_in' some ('population of Drosophila') and ('has_increased_mortality_rate' some xsd:short[>= 98])")).either
      assertM(result)(isRight)
    },
    testM("Full IRI of owl:Thing should be parseable") {
      // the issue here was that the # in the Thing IRI was commenting out the rest of the expression
      val dosdp = DOSDP.empty
      val checker = new DOSDPEntityChecker(dosdp, OBOPrefixes)
      val expressionParser = new ManchesterOWLSyntaxClassExpressionParser(OWLManager.getOWLDataFactory, checker)
      val expressionOpt = PrintfText.replaced(Some("%s and %s"),
        Some(List("thing", "nothing")),
        None,
        Some(Map(
          "thing" -> SingleValue("http://www.w3.org/2002/07/owl#Thing"),
          "nothing" -> SingleValue("http://www.w3.org/2002/07/owl#Nothing"))), true)
      for {
        expression <- ZIO.fromOption(expressionOpt)
        ce <- ZIO.effect(expressionParser.parse(expression))
      } yield assert(ce)(equalTo(factory.getOWLObjectIntersectionOf(factory.getOWLThing, factory.getOWLNothing)))
    }
  )

}
