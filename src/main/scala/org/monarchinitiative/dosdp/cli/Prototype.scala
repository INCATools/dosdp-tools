package org.monarchinitiative.dosdp.cli

import org.eclipse.rdf4j.model.vocabulary.DCTERMS
import org.monarchinitiative.dosdp.cli.DOSDPError.{logError, logErrorFail}
import org.monarchinitiative.dosdp.{DOSDP, Utilities}
import org.phenoscape.scowl._
import org.semanticweb.owlapi.model.{OWLAnnotationProperty, OWLAxiom}
import zio.{Config => _, _}

import java.nio.file.{Files, Path, Paths}
import java.util.Locale
import scala.jdk.CollectionConverters._

object Prototype {

  private val DCTTitle: OWLAnnotationProperty = AnnotationProperty(DCTERMS.TITLE.stringValue)
  val OboInOwlSource: OWLAnnotationProperty = AnnotationProperty("http://www.geneontology.org/formats/oboInOwl#source")

  def run(config: PrototypeConfig): IO[DOSDPError, Unit] = {
    Main.withLogContext(Map("command" -> "prototype")) {
      val possibleFile = Paths.get(config.common.template)
      for {
        isDir <- ZIO.attempt(Files.isDirectory(possibleFile)).flatMapError(e => logError(s"Unable to read input at $possibleFile", e))
        filenames <- if (isDir) {
          ZIO.attempt(Files.list(possibleFile)).acquireReleaseWithAuto { paths =>
            ZIO.attempt(paths.iterator.asScala.filter(isYamlFile).map(_.toString).toSet)
          }.flatMapError(e => logError(s"Couldn't list files in $possibleFile", e))
        } else ZIO.succeed(Set(config.common.template))
        dosdps <- ZIO.foreach(filenames)(f => Config.inputDOSDPFrom(f))
        axioms <- ZIO.foreach(dosdps)(dosdp => axiomsFor(dosdp, config)).map(_.flatten)
        _ <- Utilities.saveAxiomsToOntology(axioms, config.common.outfile)
      } yield ()
    }
  }

  private def axiomsFor(dosdp: DOSDP, config: PrototypeConfig): IO[DOSDPError, Set[OWLAxiom]] =
    Main.withLogContext(dosdp.pattern_name.map(n => Map("pattern" -> n)).getOrElse(Map.empty)) {
      for {
        prefixes <- config.common.prefixesMap
        ontologyOpt <- config.common.ontologyOpt
        iri <- ZIO.fromOption(dosdp.pattern_iri).orElse(logErrorFail("Pattern must have pattern IRI for prototype command"))
        fillers = dosdp.vars.getOrElse(Map.empty) ++
          dosdp.list_vars.getOrElse(Map.empty) ++
          dosdp.data_vars.getOrElse(Map.empty) ++
          dosdp.data_list_vars.getOrElse(Map.empty) +
          (DOSDP.DefinedClassVariable -> iri)
        axioms <- Generate.renderPattern(dosdp, prefixes, fillers, ontologyOpt, true, true, None, false, OboInOwlSource, false, Map.empty)
        maybeTitleAxiom = dosdp.pattern_name.map(name => Class(iri) Annotation(DCTTitle, name))
      } yield axioms ++ maybeTitleAxiom
    }

  private def isYamlFile(path: Path): Boolean = {
    val name = path.getFileName.toString.toLowerCase(Locale.ROOT)
    name.endsWith(".yaml") || name.endsWith(".yml")
  }

}
