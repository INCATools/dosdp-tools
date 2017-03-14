package org.monarchinitiative.dosdp

import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.circe.yaml.parser.Parser
import java.io.InputStreamReader
import com.hp.hpl.jena.query.QueryFactory
import scala.collection.JavaConverters._

class QueryGeneratorTest extends UnitSpec {

  "Defined class" should "be first column" in {
    var processed = false
    for {
      json <- Parser.parse(new InputStreamReader(getClass.getResourceAsStream("QueryGeneratorTest.yaml")))
      dosdp <- json.as[DOSDP]
    } {
      val prefixes = (for {
        prefixesJson <- Parser.parse(new InputStreamReader(getClass.getResourceAsStream("StandardPrefixes.yaml"))).toOption
        prefixMap <- prefixesJson.as[Map[String, String]].toOption
      } yield prefixMap).getOrElse(Map.empty)
      val variables = QueryFactory.create(SPARQL.queryFor(ExpandedDOSDP(dosdp, prefixes))).getProjectVars.asScala
      variables(0).getVarName shouldEqual "defined_class"
      variables(1).getVarName shouldEqual "defined_class_label"
      processed = true
    }
    processed shouldEqual true
  }

}
