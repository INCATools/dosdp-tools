package org.monarchinitiative.dosdp.cli

import better.files._
import org.eclipse.rdf4j.model.vocabulary.DCTERMS
import org.monarchinitiative.dosdp.{DOSDP, Utilities}
import org.phenoscape.scowl._
import org.semanticweb.owlapi.model.{OWLAnnotationProperty, OWLAxiom}
import zio._
import zio.blocking.Blocking
import zio.console.putStrLn

object Prototype {

  private val DCTTitle: OWLAnnotationProperty = AnnotationProperty(DCTERMS.TITLE.stringValue)
  private val OboInOwlSource: OWLAnnotationProperty = AnnotationProperty("http://www.geneontology.org/formats/oboInOwl#source")

  def run(config: PrototypeConfig): ZIO[ZEnv, DOSDPError, Unit] = {
    val possibleFile = File(config.common.template)
    val program = for {
      isDir <- ZIO.effect(possibleFile.isDirectory).mapError(e => DOSDPError(s"Unable to read input at $possibleFile", e))
      filenames <- if (isDir) {
        ZIO.effect {
          possibleFile.list.filter { f =>
            f.extension(false, false, true).exists(e => (e == "yaml") || (e == "yml"))
          }.map(_.toString).toSet
        }.mapError(e => DOSDPError(s"Couldn't list files in $possibleFile", e))
      } else ZIO.succeed(Set(config.common.template))
      dosdps <- ZIO.foreach(filenames)(f => Config.inputDOSDPFrom(f))
      axioms <- ZIO.foreach(dosdps)(dosdp => axiomsFor(dosdp, config)).map(_.flatten)
      _ <- Utilities.saveAxiomsToOntology(axioms, config.common.outfile)
    } yield ()
    program.catchSome { case msg: DOSDPError => ZIO.effectTotal(scribe.error(msg.msg)) }.catchAllCause(cause => putStrLn(cause.untraced.prettyPrint))
  }

  private def axiomsFor(dosdp: DOSDP, config: PrototypeConfig): ZIO[Blocking, DOSDPError, Set[OWLAxiom]] =
    for {
      prefixes <- config.common.prefixesMap
      ontologyOpt <- config.common.ontologyOpt
      iri <- ZIO.fromOption(dosdp.pattern_iri).orElseFail(DOSDPError("Pattern must have pattern IRI for prototype command"))
      fillers = dosdp.vars.getOrElse(Map.empty) ++
        dosdp.list_vars.getOrElse(Map.empty) ++
        dosdp.data_vars.getOrElse(Map.empty) ++
        dosdp.data_list_vars.getOrElse(Map.empty) +
        (DOSDP.DefinedClassVariable -> iri)
      axioms <- Generate.renderPattern(dosdp, prefixes, fillers, ontologyOpt, true, true, None, false, OboInOwlSource, false)
      maybeTitleAxiom = dosdp.pattern_name.map(name => Class(iri) Annotation(DCTTitle, name))
    } yield axioms ++ maybeTitleAxiom

}
