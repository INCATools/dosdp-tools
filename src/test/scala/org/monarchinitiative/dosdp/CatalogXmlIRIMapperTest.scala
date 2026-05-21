package org.monarchinitiative.dosdp

import org.semanticweb.owlapi.model.IRI
import zio.ZIO
import zio.test._

import java.nio.charset.StandardCharsets
import java.nio.file.Files

object CatalogXmlIRIMapperTest extends ZIOSpecDefault {

  override def spec: Spec[TestEnvironment, Any] =
    suite("CatalogXmlIRIMapper")(
      test("resolves relative uri entries relative to the catalog file") {
        for {
          dir <- ZIO.attempt(Files.createTempDirectory("dosdp-catalog-"))
          ontologyPath <- ZIO.attempt(Files.createFile(dir.resolve("imports.owl")))
          catalogPath = dir.resolve("catalog.xml")
          catalog =
            """<?xml version="1.0" encoding="UTF-8"?>
              |<catalog prefer="public" xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog">
              |  <uri name="http://example.org/imports.owl" uri="imports.owl"/>
              |</catalog>
              |""".stripMargin
          _ <- ZIO.attempt(Files.write(catalogPath, catalog.getBytes(StandardCharsets.UTF_8)))
          mapper = new CatalogXmlIRIMapper(catalogPath.toFile)
          mapped = mapper.getDocumentIRI(IRI.create("http://example.org/imports.owl"))
          expected = IRI.create(ontologyPath.toFile.getCanonicalFile)
        } yield assertTrue(mapped == expected)
      },
      test("keeps absolute IRI mappings as IRIs") {
        for {
          catalogPath <- ZIO.attempt(Files.createTempFile("dosdp-catalog-", ".xml"))
          catalog =
            """<?xml version="1.0" encoding="UTF-8"?>
              |<catalog prefer="public" xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog">
              |  <uri name="http://example.org/ontology.owl" uri="https://example.org/mirror/ontology.owl"/>
              |</catalog>
              |""".stripMargin
          _ <- ZIO.attempt(Files.write(catalogPath, catalog.getBytes(StandardCharsets.UTF_8)))
          mapper = new CatalogXmlIRIMapper(catalogPath.toFile)
          mapped = mapper.getDocumentIRI(IRI.create("http://example.org/ontology.owl"))
        } yield assertTrue(mapped == IRI.create("https://example.org/mirror/ontology.owl"))
      },
      test("accepts catalog files with an external DTD declaration") {
        for {
          dir <- ZIO.attempt(Files.createTempDirectory("dosdp-catalog-"))
          ontologyPath <- ZIO.attempt(Files.createFile(dir.resolve("imports.owl")))
          catalogPath = dir.resolve("catalog.xml")
          catalog =
            """<?xml version="1.0" encoding="UTF-8"?>
              |<!DOCTYPE catalog PUBLIC "-//OASIS//DTD Entity Resolution XML Catalog V1.0//EN" "http://example.org/catalog.dtd">
              |<catalog prefer="public" xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog">
              |  <uri name="http://example.org/imports.owl" uri="imports.owl"/>
              |</catalog>
              |""".stripMargin
          _ <- ZIO.attempt(Files.write(catalogPath, catalog.getBytes(StandardCharsets.UTF_8)))
          mapper = new CatalogXmlIRIMapper(catalogPath.toFile)
          mapped = mapper.getDocumentIRI(IRI.create("http://example.org/imports.owl"))
          expected = IRI.create(ontologyPath.toFile.getCanonicalFile)
        } yield assertTrue(mapped == expected)
      }
    )

}
