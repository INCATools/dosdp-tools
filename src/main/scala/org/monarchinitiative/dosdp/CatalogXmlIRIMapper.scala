package org.monarchinitiative.dosdp

import org.semanticweb.owlapi.model.{IRI, OWLOntologyIRIMapper}
import org.xml.sax.Attributes
import org.xml.sax.helpers.DefaultHandler

import java.io.{File, FileInputStream, InputStream}
import java.net.URL
import java.util.HashMap
import javax.xml.parsers.SAXParserFactory
import scala.util.control.NonFatal

/**
 * OWL ontology IRI mapper backed by OASIS-style catalog XML `uri` entries.
 *
 * Adapted from ROBOT's `org.obolibrary.robot.CatalogXmlIRIMapper` and
 * `CatalogElementHandler` implementations (BSD-3-Clause). Keeping this local
 * avoids depending on all of `robot-core` for one small mapper.
 */
final class CatalogXmlIRIMapper private (mappings: java.util.Map[IRI, IRI]) extends OWLOntologyIRIMapper {

  def this(catalogFile: String) =
    this(CatalogXmlIRIMapper.parseCatalogFile(new File(catalogFile).getAbsoluteFile))

  def this(catalogFile: File) =
    this(CatalogXmlIRIMapper.parseCatalogFile(catalogFile.getAbsoluteFile))

  def this(catalogFile: File, parentFolder: File) =
    this(CatalogXmlIRIMapper.parseCatalogXml(new FileInputStream(catalogFile), Option(parentFolder)))

  def this(catalogIRI: IRI) =
    this(CatalogXmlIRIMapper.parseCatalogXml(catalogIRI.toURI.toURL))

  def this(catalogURL: URL) =
    this(CatalogXmlIRIMapper.parseCatalogXml(catalogURL))

  def this(catalogURL: URL, parentFolder: File) =
    this(CatalogXmlIRIMapper.parseCatalogXml(catalogURL.openStream(), Option(parentFolder)))

  override def getDocumentIRI(ontologyIRI: IRI): IRI =
    mappings.get(ontologyIRI)

}

private object CatalogXmlIRIMapper {

  private def parseCatalogFile(catalogFile: File): java.util.Map[IRI, IRI] =
    parseCatalogXml(new FileInputStream(catalogFile), Option(catalogFile.getParentFile))

  private def parseCatalogXml(catalogURL: URL): java.util.Map[IRI, IRI] =
    if (catalogURL.getProtocol == "file") {
      val catalogFile = new File(catalogURL.toURI)
      parseCatalogXml(new FileInputStream(catalogFile), Option(catalogFile.getParentFile))
    } else parseCatalogXml(catalogURL.openStream(), None)

  private def parseCatalogXml(inputStream: InputStream, parentFolder: Option[File]): java.util.Map[IRI, IRI] = {
    val mappings = new HashMap[IRI, IRI]()
    try {
      val factory = SAXParserFactory.newInstance()
      factory.setValidating(false)
      factory.setNamespaceAware(false)
      disableExternalXml(factory)
      val saxParser = factory.newSAXParser()
      saxParser.parse(inputStream, new CatalogElementHandler(parentFolder, mappings))
      mappings
    } finally inputStream.close()
  }

  private def disableExternalXml(factory: SAXParserFactory): Unit = {
    setFeatureIfSupported(factory, "http://xml.org/sax/features/external-general-entities", false)
    setFeatureIfSupported(factory, "http://xml.org/sax/features/external-parameter-entities", false)
    setFeatureIfSupported(factory, "http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
    try factory.setXIncludeAware(false)
    catch {
      case NonFatal(_) => ()
    }
  }

  private def setFeatureIfSupported(factory: SAXParserFactory, feature: String, value: Boolean): Unit =
    try factory.setFeature(feature, value)
    catch {
      case NonFatal(_) => ()
    }

  private final class CatalogElementHandler(parentFolder: Option[File], mappings: java.util.Map[IRI, IRI]) extends DefaultHandler {

    override def startElement(uri: String, localName: String, qName: String, attributes: Attributes): Unit = {
      val elementName = if (qName != null && qName.nonEmpty) qName else localName
      if (elementName != "uri") return

      val fromString = attributes.getValue("name")
      val toString = attributes.getValue("uri")
      if ((fromString == null) || (toString == null)) return

      mappedIRI(toString).foreach { toIRI =>
        mappings.put(IRI.create(fromString), toIRI)
      }
    }

    private def mappedIRI(value: String): Option[IRI] =
      parentFolder.filter(_ => !value.contains(":")) match {
        case Some(parent) =>
          try {
            val file = new File(value)
            val resolved = if (file.isAbsolute) file else new File(parent, value)
            Some(IRI.create(resolved.getCanonicalFile))
          } catch {
            case NonFatal(_) => None
          }
        case None => Some(IRI.create(value))
      }

  }

}
