package org.monarchinitiative.dosdp.cli

import org.monarchinitiative.dosdp.{DOSDP, OBOPrefixes}
import zio.test.Assertion._
import zio.test._

object FormatDataRowTest extends ZIOSpecDefault {

  private val columns = List("input1", "input1_label", "defined_class", "defined_class_label")
  private val entityColumns = Set("input1", DOSDP.DefinedClassVariable)

  def spec = suite("Docs.formatDataRow")(
    test("entity columns are linkified") {
      val row = Map("input1" -> "CHEBI:15378", DOSDP.DefinedClassVariable -> "GO:0015649")
      val out = Docs.formatDataRow(row, columns, entityColumns, OBOPrefixes)
      assert(out)(equalTo(List(
        "[CHEBI:15378](http://purl.obolibrary.org/obo/CHEBI_15378)",
        "",
        "[GO:0015649](http://purl.obolibrary.org/obo/GO_0015649)",
        ""
      )))
    },
    test("label columns containing a colon are not misparsed as CURIEs") {
      val row = Map(
        "input1" -> "CHEBI:15378",
        "input1_label" -> "hydron",
        DOSDP.DefinedClassVariable -> "GO:0015649",
        "defined_class_label" -> "2-keto-3-deoxygluconate:proton symporter activity"
      )
      val out = Docs.formatDataRow(row, columns, entityColumns, OBOPrefixes)
      assert(out)(equalTo(List(
        "[CHEBI:15378](http://purl.obolibrary.org/obo/CHEBI_15378)",
        "hydron",
        "[GO:0015649](http://purl.obolibrary.org/obo/GO_0015649)",
        "2-keto-3-deoxygluconate:proton symporter activity"
      )))
    },
    test("missing cells render as empty strings") {
      val out = Docs.formatDataRow(Map.empty, columns, entityColumns, OBOPrefixes)
      assert(out)(equalTo(List("", "", "", "")))
    }
  )

}