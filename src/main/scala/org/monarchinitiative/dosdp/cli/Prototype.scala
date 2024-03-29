package org.monarchinitiative.dosdp.cli

import better.files._
import org.eclipse.rdf4j.model.vocabulary.DCTERMS
import org.monarchinitiative.dosdp.cli.DOSDPError.{logError, logErrorFail}
import org.monarchinitiative.dosdp.cli.Main.loggingContext
import org.monarchinitiative.dosdp.{DOSDP, Utilities}
import org.phenoscape.scowl._
import org.semanticweb.owlapi.model.{OWLAnnotationProperty, OWLAxiom}
import zio._
import zio.logging._
import zio.blocking.Blocking

object Prototype {

  private val DCTTitle: OWLAnnotationProperty = AnnotationProperty(DCTERMS.TITLE.stringValue)
  val OboInOwlSource: OWLAnnotationProperty = AnnotationProperty("http://www.geneontology.org/formats/oboInOwl#source")

  def run(config: PrototypeConfig): ZIO[ZEnv with Logging, DOSDPError, Unit] = {
    log.locally(_.annotate(loggingContext, Map("command" -> "prototype"))) {
      val possibleFile = File(config.common.template)
      for {
        isDir <- ZIO.effect(possibleFile.isDirectory).flatMapError(e => logError(s"Unable to read input at $possibleFile", e))
        filenames <- if (isDir) {
          ZIO.effect {
            possibleFile.list.filter { f =>
              f.extension(false, false, true).exists(e => (e == "yaml") || (e == "yml"))
            }.map(_.toString).toSet
          }.flatMapError(e => logError(s"Couldn't list files in $possibleFile", e))
        } else ZIO.succeed(Set(config.common.template))
        dosdps <- ZIO.foreach(filenames)(f => Config.inputDOSDPFrom(f))
        axioms <- ZIO.foreach(dosdps)(dosdp => axiomsFor(dosdp, config)).map(_.flatten)
        _ <- Utilities.saveAxiomsToOntology(axioms, config.common.outfile)
      } yield ()
    }
  }

  private def axiomsFor(dosdp: DOSDP, config: PrototypeConfig): ZIO[Blocking with Logging, DOSDPError, Set[OWLAxiom]] =
    log.locally(_.annotate(loggingContext, dosdp.pattern_name.map(n => Map("pattern" -> n)).getOrElse(Map.empty))) {
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

}
