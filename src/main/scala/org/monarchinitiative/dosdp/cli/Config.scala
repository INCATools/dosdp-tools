package org.monarchinitiative.dosdp.cli

import java.io.File

import caseapp._
import caseapp.core.Error.MalformedValue
import caseapp.core.argparser.{ArgParser, SimpleArgParser}
import com.github.tototoshi.csv.{CSVFormat, DefaultCSVFormat, TSVFormat}
import io.circe.generic.auto._
import io.circe.yaml.parser
import org.monarchinitiative.dosdp.cli.Config.{AllAxioms, AxiomKind, BoolValue, FalseValue, LogicalAxioms, MultiArgList, inputDOSDPFrom}
import org.monarchinitiative.dosdp.{DOSDP, OBOPrefixes, Utilities}
import org.semanticweb.owlapi.model.OWLOntology
import zio._
import zio.blocking.Blocking

import scala.io.Source

@AppName("dosdp-tools")
@ProgName("dosdp-tools")
sealed trait Config {

  def run: ZIO[ZEnv, DOSDPError, Unit]

}

final case class CommonOptions(
                                @HelpMessage("OWL ontology (provide labels, query axioms)")
                                @ValueDescription("file or URI")
                                ontology: Option[String],
                                @HelpMessage("A catalog XML file to use for resolving mapping ontology IRIs to URLs")
                                @ValueDescription("file")
                                @Name("catalog")
                                catalog: Option[String],
                                @HelpMessage("DOSDP file (YAML). If a local file is not found at the given path, the path will be attempted as a URL.")
                                @ValueDescription("file")
                                template: String,
                                @HelpMessage("CURIE prefixes (YAML)")
                                @ValueDescription("file")
                                prefixes: Option[String],
                                @HelpMessage("Assume prefixes are OBO ontologies; predefine rdf, rdfs, owl, dc, dct, skos, obo, and oio")
                                @ValueDescription("true|false")
                                oboPrefixes: BoolValue = FalseValue,
                                @HelpMessage("Output file (OWL or TSV)")
                                @ValueDescription("file")
                                outfile: String = "dosdp.out",
                                @HelpMessage("Tabular format: TSV (default) or CSV")
                                @ValueDescription("tsv|csv")
                                tableFormat: String = "tsv",
                                @HelpMessage("List of patterns (without file extension) to process in batch (space separated, enclose list in quotes)")
                                @ValueDescription("names")
                                batchPatterns: MultiArgList = MultiArgList(Nil)
                              ) {

  def inputDOSDP: IO[DOSDPError, DOSDP] = inputDOSDPFrom(template)

  def prefixesMap: ZIO[Any, DOSDPError, PartialFunction[String, String]] = {
    val possiblePrefixMap = prefixes.map { prefixesPath =>
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
    } yield if (oboPrefixes.bool) specifiedPrefixes.orElse(OBOPrefixes) else specifiedPrefixes
  }

  def ontologyOpt: ZIO[Blocking, DOSDPError, Option[OWLOntology]] =
    ZIO.foreach(ontology)(ontPath => Utilities.loadOntology(ontPath, catalog))

}

@CommandName("terms")
@HelpMessage("dump terms referenced in TSV input and a Dead Simple OWL Design Pattern")
final case class TermsConfig(@Recurse
                             common: CommonOptions,
                             @HelpMessage("Input file (TSV or CSV)")
                             @ValueDescription("file")
                             infile: String = "fillers.tsv") extends Config {

  override def run: ZIO[zio.ZEnv, DOSDPError, Unit] = Terms.run(this)

}

@CommandName("generate")
@HelpMessage("generate ontology axioms for TSV input to a Dead Simple OWL Design Pattern")
final case class GenerateConfig(@Recurse
                                common: CommonOptions,
                                @HelpMessage("Input file (TSV or CSV)")
                                @ValueDescription("file")
                                infile: String = "fillers.tsv",
                                @HelpMessage("Restrict generated axioms to 'logical', 'annotation', or 'all' (default)")
                                @ValueDescription("all|logical|annotation")
                                restrictAxiomsTo: AxiomKind = AllAxioms,
                                @HelpMessage("Data column containing local axiom output restrictions")
                                @ValueDescription("name")
                                restrictAxiomsColumn: Option[String],
                                @HelpMessage("Compute defined class IRI from pattern IRI and variable fillers")
                                @ValueDescription("true|false")
                                generateDefinedClass: BoolValue = FalseValue,
                                @HelpMessage("Add axiom annotation to generated axioms linking to pattern IRI")
                                @ValueDescription("true|false")
                                addAxiomSourceAnnotation: BoolValue = FalseValue,
                                @HelpMessage("IRI for annotation property to use to link generated axioms to pattern IRI")
                                @ValueDescription("IRI")
                                axiomSourceAnnotationProperty: String = "http://www.geneontology.org/formats/oboInOwl#source"
                               ) extends Config {

  override def run: ZIO[zio.ZEnv, DOSDPError, Unit] = Generate.run(this)

}

@CommandName("prototype")
@HelpMessage("output \"prototype\" axioms using default fillers for a pattern or folder of patterns")
final case class PrototypeConfig(@Recurse
                                 common: CommonOptions) extends Config {

  override def run: ZIO[zio.ZEnv, DOSDPError, Unit] = Prototype.run(this)

}

@CommandName("docs")
@HelpMessage("output Markdown documentation for patterns")
final case class DocsConfig(@Recurse
                            common: CommonOptions,
                            @HelpMessage("Input file (TSV or CSV)")
                            @ValueDescription("file")
                            infile: String = "fillers.tsv",
                            @HelpMessage("URL prefix for linking to data files")
                            @ValueDescription("URL")
                            dataLocationPrefix: String = "http://example.org/") extends Config {

  override def run: ZIO[zio.ZEnv, DOSDPError, Unit] = Docs.run(this)

}

@CommandName("query")
@HelpMessage("query an ontology for terms matching a Dead Simple OWL Design Pattern")
final case class QueryConfig(@Recurse
                             common: CommonOptions,
                             @HelpMessage("Reasoner to use for expanding variable constraints. Valid options are ELK, HermiT, or JFact.")
                             @ValueDescription("elk|hermit|jfact")
                             reasoner: Option[String],
                             @HelpMessage("Print generated query without running against ontology")
                             @ValueDescription("true|false")
                             printQuery: BoolValue = FalseValue,
                             @HelpMessage("Restrict queried axioms to 'logical', 'annotation', or 'all' (default)")
                             @ValueDescription("all|logical|annotation")
                             restrictAxiomsTo: AxiomKind = LogicalAxioms
                            ) extends Config {

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

  /**
   * This works around some confusing behavior in case-app boolean parsing
   */
  sealed trait BoolValue {

    def bool: Boolean

  }

  case object TrueValue extends BoolValue {

    def bool = true

  }

  case object FalseValue extends BoolValue {

    def bool = false

  }

  implicit val boolArgParser: ArgParser[BoolValue] = SimpleArgParser.from[BoolValue]("boolean value") { arg =>
    arg.toLowerCase match {
      case "true"  => Right(TrueValue)
      case "false" => Right(FalseValue)
      case "1"     => Right(TrueValue)
      case "0"     => Right(FalseValue)
      case _       => Left(MalformedValue("boolean value", arg))
    }
  }

  final case class MultiArgList(items: List[String])

  implicit val listArgParser: ArgParser[MultiArgList] = SimpleArgParser.from[MultiArgList]("multiple values") { arg =>
    val trimmed = arg.trim
    if (trimmed.isEmpty) Left(MalformedValue("empty list input", arg))
    else Right(MultiArgList(arg.split(" ", -1).toList))
  }

  sealed trait AxiomKind

  case object LogicalAxioms extends AxiomKind

  case object AnnotationAxioms extends AxiomKind

  case object AllAxioms extends AxiomKind

  implicit val axiomKindArgParser: ArgParser[AxiomKind] = SimpleArgParser.from[AxiomKind]("axiom kind")(parseAxiomKind)

  def parseAxiomKind(arg: String): Either[MalformedValue, AxiomKind] = {
    arg.toLowerCase match {
      case "all"        => Right(AllAxioms)
      case "logical"    => Right(LogicalAxioms)
      case "annotation" => Right(AnnotationAxioms)
      case _            => Left(MalformedValue("Not a valid axiom type", arg))
    }
  }

}

final case class DOSDPError(msg: String, cause: Throwable) extends Exception(msg, cause)

object DOSDPError {

  def apply(msg: String): DOSDPError = DOSDPError(msg, new Exception(msg))

}