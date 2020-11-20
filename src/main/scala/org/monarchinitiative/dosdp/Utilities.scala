package org.monarchinitiative.dosdp

import java.io.{File, IOException}
import java.nio.file.{Files, Paths}

import org.monarchinitiative.dosdp.cli.DOSDPError
import org.obolibrary.robot.CatalogXmlIRIMapper
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.FunctionalSyntaxDocumentFormat
import org.semanticweb.owlapi.model.{IRI, OWLAxiom, OWLOntology}
import zio._
import zio.blocking.{Blocking, effectBlocking, effectBlockingIO}

import scala.jdk.CollectionConverters._

object Utilities {

  def saveAxiomsToOntology(axioms: Set[OWLAxiom], filepath: String): ZIO[Blocking, DOSDPError, Unit] =
    for {
      manager <- ZIO.effectTotal(OWLManager.createOWLOntologyManager())
      ont <- ZIO.effect(manager.createOntology(axioms.asJava)).orDie
      _ <- effectBlocking(manager.saveOntology(ont, new FunctionalSyntaxDocumentFormat(), IRI.create(new File(filepath)))).mapError(e =>
        DOSDPError(s"Unable to write ontology to file $filepath", e))
    } yield ()

  def loadOntology(location: String, catalogPathOpt: Option[String]): ZIO[Blocking, DOSDPError, OWLOntology] = {
    val ontIRI = if (location.startsWith("http")) IRI.create(location) else IRI.create(new File(location))
    for {
      manager <- ZIO.effectTotal(OWLManager.createOWLOntologyManager())
      _ <- ZIO.foreach_(catalogPathOpt) { catalog =>
        for {
          iriMapper <- effectBlockingIO(new CatalogXmlIRIMapper(catalog))
            .mapError(e => DOSDPError(s"Failed reading ontology catalog file at path $catalog", e))
          _ <- ZIO.effectTotal(manager.addIRIMapper(iriMapper))
        } yield ()
      }
      ontology <- ZIO.effect(manager.loadOntology(ontIRI))
        .mapError(e => DOSDPError(s"Failed loading ontology from location $location", e))
    } yield ontology
  }

  def isDirectory(path: String): RIO[Blocking, Boolean] = effectBlocking(Files.isDirectory(Paths.get(path)))

}
