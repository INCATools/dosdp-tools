package org.monarchinitiative.dosdp

import org.apache.jena.datatypes.TypeMapper
import org.apache.jena.rdf.model.impl.ResourceImpl
import org.apache.jena.rdf.model.{AnonId, ResourceFactory, Statement}
import org.eclipse.rdf4j.model.{BNode, IRI, Literal, URI, Statement => SesameStatement}
import org.eclipse.rdf4j.rio.helpers.StatementCollector
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.rio.RioRenderer

import scala.jdk.CollectionConverters._

object SesameJena {

  def ontologyAsTriples(ontology: OWLOntology): Set[Statement] = {
    val collector = new StatementCollector()
    new RioRenderer(ontology, collector, null).render()
    collector.getStatements.asScala.map(sesameTripleToJena).toSet
  }

  def sesameTripleToJena(triple: SesameStatement): Statement = {
    val subject = triple.getSubject match {
      case bnode: BNode => new ResourceImpl(new AnonId(bnode.getID))
      case uri: IRI     => ResourceFactory.createResource(uri.stringValue)
      case uri: URI     => ResourceFactory.createResource(uri.stringValue)
    }
    val predicate = ResourceFactory.createProperty(triple.getPredicate.stringValue)
    val obj = triple.getObject match {
      case bnode: BNode                                    => new ResourceImpl(new AnonId(bnode.getID))
      case uri: IRI                                        => ResourceFactory.createResource(uri.stringValue)
      case uri: URI                                        => ResourceFactory.createResource(uri.stringValue)
      case literal: Literal if literal.getLanguage.isPresent => ResourceFactory.createLangLiteral(literal.getLabel, literal.getLanguage.get)
      case literal: Literal if literal.getDatatype != null => ResourceFactory.createTypedLiteral(literal.getLabel, TypeMapper.getInstance.getSafeTypeByName(literal.getDatatype.stringValue))
      case literal: Literal                                => ResourceFactory.createStringLiteral(literal.getLabel)
    }
    ResourceFactory.createStatement(subject, predicate, obj)
  }

}