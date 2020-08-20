package org.monarchinitiative.dosdp

import java.io.InputStreamReader

import io.circe.generic.auto._
import io.circe.yaml.parser
import org.apache.jena.query.QueryFactory

import scala.jdk.CollectionConverters._

class QueryGeneratorTest extends UnitSpec {

  "Defined class" should "be first column" in {
    val dosdp = (for {
      json <- parser.parse(new InputStreamReader(getClass.getResourceAsStream("QueryGeneratorTest.yaml"))).toOption
      pattern <- json.as[DOSDP].toOption
    } yield pattern).getOrElse(???)
    val prefixes = (for {
      prefixesJson <- parser.parse(new InputStreamReader(getClass.getResourceAsStream("StandardPrefixes.yaml"))).toOption
      prefixMap <- prefixesJson.as[Map[String, String]].toOption
    } yield prefixMap).getOrElse(Map.empty)
    val variables = QueryFactory.create(SPARQL.queryFor(ExpandedDOSDP(dosdp, prefixes))).getProjectVars.asScala
    variables(0).getVarName shouldEqual "defined_class"
    variables(1).getVarName shouldEqual "defined_class_label"
  }

}
