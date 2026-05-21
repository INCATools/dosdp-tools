package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.DOSDPError
import org.monarchinitiative.dosdp.cli.DOSDPError.logError
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.FunctionalSyntaxDocumentFormat
import org.semanticweb.owlapi.model.{IRI, OWLAxiom, OWLOntology}
import zio.{Config => _, _}

import java.io.File
import scala.jdk.CollectionConverters._

object Utilities {

  def saveAxiomsToOntology(axioms: Set[OWLAxiom], filepath: String): IO[DOSDPError, Unit] =
    for {
      manager <- ZIO.succeed(OWLManager.createOWLOntologyManager())
      ont <- ZIO.attempt(manager.createOntology(axioms.asJava)).orDie
      _ <- ZIO.attemptBlocking(manager.saveOntology(ont, new FunctionalSyntaxDocumentFormat(), IRI.create(new File(filepath)))).flatMapError(e =>
        logError(s"Unable to write ontology to file $filepath", e))
    } yield ()

  def loadOntology(location: String, catalogPathOpt: Option[String]): IO[DOSDPError, OWLOntology] = {
    val ontIRI = if (location.startsWith("http")) IRI.create(location) else IRI.create(new File(location))
    for {
      manager <- ZIO.succeed(OWLManager.createOWLOntologyManager())
      _ <- ZIO.foreachDiscard(catalogPathOpt) { catalog =>
        for {
          iriMapper <- ZIO.attemptBlockingIO(new CatalogXmlIRIMapper(catalog))
            .flatMapError(e => logError(s"Failed reading ontology catalog file at path $catalog", e))
          _ <- ZIO.succeed(manager.getIRIMappers.add(iriMapper))
        } yield ()
      }
      ontology <- ZIO.attemptBlocking(manager.loadOntology(ontIRI))
        .flatMapError(e => logError(s"Failed loading ontology from location $location", e))
    } yield ontology
  }

}
