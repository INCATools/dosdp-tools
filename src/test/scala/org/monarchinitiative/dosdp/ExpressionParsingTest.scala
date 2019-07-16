package org.monarchinitiative.dosdp

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntaxClassExpressionParser
import org.phenoscape.scowl._

class ExpressionParsingTest extends UnitSpec {

  "Datatype restrictions" should "be parseable" in {
    val factory = OWLManager.getOWLDataFactory
    val specifiedPrefixes = Map("ex" -> "http://example.org/")
    val prefixes = specifiedPrefixes.orElse(OBOPrefixes)
    val dosdp = DOSDP.empty.copy(classes = Some(Map("population of Drosophila" -> "ex:1")), relations = Some(Map("inheres_in" -> "ex:2")), dataProperties = Some(Map("has_increased_mortality_rate" -> "ex:3")))
    val edosdp = ExpandedDOSDP(dosdp, prefixes)
    val expressionParser = new ManchesterOWLSyntaxClassExpressionParser(OWLManager.getOWLDataFactory, edosdp.checker)
    //Should not throw exception
    expressionParser.parse("'inheres_in' some ('population of Drosophila') and ('has_increased_mortality_rate' some xsd:short[>= 98])")
  }

}
