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
      dosdp <- decode[DOSDP](json.spaces4)
    } {
      val variables = QueryFactory.create(SPARQL.queryFor(dosdp)).getProjectVars.asScala
      variables(0).getVarName shouldEqual "defined_class"
      variables(1).getVarName shouldEqual "defined_class_label"
      processed = true
    }
    processed shouldEqual true
  }

}
