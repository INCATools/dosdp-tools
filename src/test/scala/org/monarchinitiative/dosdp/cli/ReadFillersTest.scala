package org.monarchinitiative.dosdp.cli

import com.github.tototoshi.csv.TSVFormat
import zio.test.Assertion._
import zio.test._
import zio.{Console => _, _}

import java.io.{File, PrintWriter}
import java.nio.file.Files

object ReadFillersTest extends ZIOSpecDefault {

  private val header = List("input2", "input1", "input1_label", "input2_label", "defined_class", "defined_class_label")

  private def writeTsv(rows: List[List[String]]): Task[File] =
    ZIO.attempt {
      val f = Files.createTempFile("fillers-", ".tsv").toFile
      f.deleteOnExit()
      val w = new PrintWriter(f, "utf-8")
      try rows.foreach(r => w.println(r.mkString("\t")))
      finally w.close()
      f
    }

  def spec = suite("Generate.readFillers")(
    test("columns are returned in file order") {
      val dataRow = List("CHEBI:1", "CHEBI:2", "alpha", "beta", "GO:1", "alpha:beta activity")
      for {
        file <- writeTsv(List(header, dataRow))
        result <- Generate.readFillers(file, new TSVFormat {})
        (columns, rows) = result
      } yield assert(columns.toList)(equalTo(header)) &&
        assert(rows)(equalTo(List(header.zip(dataRow).toMap)))
    }
  )

}
