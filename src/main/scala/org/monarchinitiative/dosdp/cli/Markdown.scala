package org.monarchinitiative.dosdp.cli

import org.monarchinitiative.dosdp.PrintfText
import zio._

import java.nio.file.{Files, Paths}
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

object Markdown {

  def run(config: MarkdownConfig): ZIO[ZEnv, DOSDPError, Unit] =
    for {
      dosdp <- config.common.inputDOSDP
      out = new ListBuffer[String]()
      headerInfoMap = Map[String, Option[String]](
        "#" -> dosdp.pattern_name,
        "### Pattern IRI:" -> dosdp.pattern_iri,
        "### Base IRI:" -> dosdp.base_IRI,
        "### Description: " -> dosdp.description,
      )
      _ = headerInfoMap.foreach(a => out += processString(a._1, a._2))
      _ = out += ""
      _ = out += "---"

      contentMap = Map[String, Option[PrintfText]](
          "name" -> dosdp.name,
          "comment" -> dosdp.comment,
          "namespace" -> dosdp.namespace,
          "def" -> dosdp.`def`,
          "equivalentTo" -> dosdp.equivalentTo,
          "subClassOf" -> dosdp.subClassOf,
          "disjointWith" -> dosdp.disjointWith,
          "GCI" -> dosdp.GCI
      )
      _ = contentMap.foreach(a => out += processPrintfText(a._1, a._2))

      _ = out += "---"

      _ = out += processListString("Contributors", dosdp.contributors)

      _ = Files.write(Paths.get(config.common.outfile), out.toList.asJava)
    } yield ()

  def processPrintfText(label: String, text: Option[PrintfText]): String =
    text match {
      case Some(name) =>
        name.vars match {
          case Some(vars) => s"__$label:__ ${String.format(name.text, vars.map(a => s"{ $a }"): _*)}"
          case None       => throw new Exception("annotation does not have vars: " + name)
        }
      case None => ""
    }

  def processString(mu: String, text: Option[String]): String =
    text match {
      case Some(name) => s"$mu $name"
      case None => ""
    }

  def processListString(label: String, text: Option[List[String]]): String =
    text match {
      case Some(name) => s"$label: \n${name.map(a => s"* $a \n").mkString}"
      case None => ""
    }

}
