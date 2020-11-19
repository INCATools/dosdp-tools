package org.monarchinitiative.dosdp.cli

import java.io.File

import caseapp.{HelpMessage, Recurse, ValueDescription}
import com.github.tototoshi.csv.{CSVFormat, DefaultCSVFormat, TSVFormat}
import io.circe.generic.auto._
import io.circe.yaml.parser
import org.monarchinitiative.dosdp.cli.Config.inputDOSDPFrom
import org.monarchinitiative.dosdp.{DOSDP, OBOPrefixes, Utilities}
import org.semanticweb.owlapi.model.OWLOntology
import zio._
import zio.blocking.Blocking

import scala.io.Source

sealed trait Config {

  def run: ZIO[ZEnv, DOSDPError, Unit]

}

//var ontOpt: Option[String] = opt[Option[String]](name = "ontology", description = "OWL ontology (provide labels, query axioms)")
//var catalogFileOpt: Option[File] = opt[Option[File]](name = "catalog", description = "catalog file to use for resolving ontology locations")
//var templateFile: String = opt[String](name = "template", default = "dosdp.yaml", description = "DOSDP file (YAML). If a local file is not found at the given path, the path will be attempted as a URL.")
//var prefixesFileOpt: Option[File] = opt[Option[File]](name = "prefixes", default = None, description = "CURIE prefixes (YAML)")
//var oboPrefixes: Boolean = opt[Boolean](name = "obo-prefixes", default = false, description = "Assume prefixes are OBO ontologies; predefine rdf, rdfs, owl, dc, dct, skos, obo, and oio.")
//var outfile: File = opt[File](name = "outfile", default = new File("dosdp.out"), description = "Output file (OWL or TSV)")
//var tableFormat: String = opt[String](name = "table-format", default = "tsv", description = "Tabular format: TSV (default) or CSV")
//var batchPatterns: Seq[String] = opt[Seq[String]](name = "batch-patterns", description = "List of patterns (without file extension) to process in batch (space separated, enclose list in quotes)", default = Nil)

final case class CommonOptions(
                                ontOpt: Option[String],
                                @HelpMessage("a catalog XML file mapping ontology IRIs to URLs")
                                @ValueDescription("file")
                                catalogPathOpt: Option[String],
                                @ValueDescription("file")
                                templateFile: String,
                                @ValueDescription("file")
                                prefixesPathOpt: Option[String],
                                oboPrefixes: Boolean = true,
                                @ValueDescription("file")
                                outfilePath: String = "dosdp.out",
                                tableFormat: String = "tsv",
                                batchPatterns: List[String] = Nil
                              ) {

  def inputDOSDP: IO[DOSDPError, DOSDP] = inputDOSDPFrom(templateFile)

  def prefixes: ZIO[Any, DOSDPError, PartialFunction[String, String]] = {
    val possiblePrefixMap = prefixesPathOpt.map { prefixesPath =>
      val prefixesFile = new File(prefixesPath)
      for {
        prefixesText <- ZIO.effect(Source.fromFile(prefixesFile, "UTF-8")).bracketAuto(s => ZIO.effect(s.mkString))
          .mapError(e => DOSDPError(s"Could not read prefixes file at $prefixesPath", e))
        prefixesJson <- ZIO.fromEither(parser.parse(prefixesText))
          .mapError(e => DOSDPError(s"Invalid JSON format for prefixes file at $prefixesPath", e))
        prefixMap <- ZIO.fromEither(prefixesJson.as[Map[String, String]])
          .mapError(e => DOSDPError(s"JSON for prefixes file at $prefixesPath should be a simple map of strings", e))
      } yield prefixMap
    }
    for {
      prefixMapOpt <- ZIO.foreach(possiblePrefixMap)(identity)
      specifiedPrefixes = prefixMapOpt.getOrElse(Map.empty)
    } yield if (oboPrefixes) specifiedPrefixes.orElse(OBOPrefixes) else specifiedPrefixes
  }

  def ontologyOpt: ZIO[Blocking, DOSDPError, Option[OWLOntology]] =
    ZIO.foreach(ontOpt)(ontPath => Utilities.loadOntology(ontPath, catalogPathOpt))

}

final case class TermsConfig(@Recurse
                             common: CommonOptions,
                             infile: String = "fillers.tsv") extends Config {

  override def run: ZIO[zio.ZEnv, DOSDPError, Unit] = Terms.run(this)

}

//var infile: File = opt[File](name = "infile", default = new File("fillers.tsv"), description = "Input file (TSV or CSV)")
//var restrictAxioms: String = opt[String](name = "restrict-axioms-to", default = "all", description = "Restrict generated axioms to 'logical', 'annotation', or 'all' (default)")
//var restrictAxiomsColumn: Option[String] = opt[Option[String]](name = "restrict-axioms-column", description = "Data column containing local axiom output restrictions")
//var generateDefinedClass: Boolean = opt[Boolean](name = "generate-defined-class", description = "Computed defined class IRI from pattern IRI and variable fillers", default = false)
//var addAxiomSourceAnnotation: Boolean = opt[Boolean](name = "add-axiom-source-annotation", description = "Add axiom annotation to generated axioms linking to pattern IRI", default = false)
//var axiomSourceAnnotationProperty: String = opt[String](name = "axiom-source-annotation-property", description = "IRI for annotation property to use to link generated axioms to pattern IRI", default = "http://www.geneontology.org/formats/oboInOwl#source")

final case class GenerateConfig(@Recurse
                                common: CommonOptions,
                                infile: String = "fillers.tsv",
                                restrictAxioms: String = "all",
                                restrictAxiomsColumn: Option[String],
                                generateDefinedClass: Boolean = false,
                                addAxiomSourceAnnotation: Boolean = false,
                                axiomSourceAnnotationProperty: String = "http://www.geneontology.org/formats/oboInOwl#source"
                               ) extends Config {

  override def run: ZIO[zio.ZEnv, DOSDPError, Unit] = Generate.run(this)

}

final case class PrototypeConfig(@Recurse
                                 common: CommonOptions) extends Config {

  override def run: ZIO[zio.ZEnv, DOSDPError, Unit] = Prototype.run(this)

}

final case class QueryConfig(@Recurse
                             common: CommonOptions,
                             reasonerNameOpt: Option[String],
                             printQuery: Boolean = false) extends Config {

  override def run: ZIO[zio.ZEnv, DOSDPError, Unit] = Query.run(this)

}

object Config {

  def tabularFormat(arg: String): Either[DOSDPError, CSVFormat] = arg.toLowerCase match {
    case "csv" => Right(new DefaultCSVFormat {})
    case "tsv" => Right(new TSVFormat {})
    case other => Left(DOSDPError(s"Invalid tabular format requested: $other"))
  }

  def inputDOSDPFrom(location: String): IO[DOSDPError, DOSDP] =
    for {
      file <- ZIO.effectTotal(new File(location))
      fileExists <- ZIO.effect(file.exists).mapError(e => DOSDPError(s"Could not read pattern file at $location", e))
      sourceZ = if (fileExists) ZIO.effect(Source.fromFile(file, "UTF-8")) else
        ZIO.effect(Source.fromURL(location, "UTF-8"))
      dosdpText <- sourceZ.bracketAuto(s => ZIO.effect(s.mkString)).mapError(e => DOSDPError(s"Could not read pattern file at $location", e))
      json <- ZIO.fromEither(parser.parse(dosdpText)).mapError(e => DOSDPError(s"Invalid JSON format for pattern file at $location", e))
      dosdp <- ZIO.fromEither(json.as[DOSDP]).mapError(e => DOSDPError(s"JSON does not conform to DOS-DP schema for pattern file at $location", e))
    } yield dosdp


}

final case class DOSDPError(msg: String, cause: Throwable) extends Exception(msg, cause)

object DOSDPError {

  def apply(msg: String): DOSDPError = DOSDPError(msg, new Exception(msg))

}