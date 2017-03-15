package org.monarchinitiative.dosdp

import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.circe.yaml.parser
import java.io.InputStreamReader
import scala.collection.JavaConverters._
import org.apache.jena.query.QueryFactory

class QueryGeneratorTest extends UnitSpec {

  "Defined class" should "be first column" in {
    val dosdp = (for {
      json <- parser.parse(new InputStreamReader(getClass.getResourceAsStream("QueryGeneratorTest.yaml"))).right.toOption
      pattern <- json.as[DOSDP].right.toOption
    } yield pattern).getOrElse(???)
    val prefixes = (for {
      prefixesJson <- parser.parse(new InputStreamReader(getClass.getResourceAsStream("StandardPrefixes.yaml"))).right.toOption
      prefixMap <- prefixesJson.as[Map[String, String]].right.toOption
    } yield prefixMap).getOrElse(Map.empty)
    val variables = QueryFactory.create(SPARQL.queryFor(ExpandedDOSDP(dosdp, prefixes))).getProjectVars.asScala
    variables(0).getVarName shouldEqual "defined_class"
    variables(1).getVarName shouldEqual "defined_class_label"
  }

}
