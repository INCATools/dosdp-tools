package org.monarchinitiative.dosdp

import org.apache.jena.datatypes.TypeMapper
import org.apache.jena.rdf.model.impl.ResourceImpl
import org.apache.jena.rdf.model.{AnonId, ResourceFactory, Statement}
import org.openrdf.model.{BNode, Literal, URI, Statement => SesameStatement}
import org.openrdf.rio.helpers.StatementCollector
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
      case uri: URI     => ResourceFactory.createResource(uri.stringValue)
    }
    val predicate = ResourceFactory.createProperty(triple.getPredicate.stringValue)
    val obj = triple.getObject match {
      case bnode: BNode                                    => new ResourceImpl(new AnonId(bnode.getID))
      case uri: URI                                        => ResourceFactory.createResource(uri.stringValue)
      case literal: Literal if literal.getLanguage != null => ResourceFactory.createLangLiteral(literal.getLabel, literal.getLanguage)
      case literal: Literal if literal.getDatatype != null => ResourceFactory.createTypedLiteral(literal.getLabel, TypeMapper.getInstance.getSafeTypeByName(literal.getDatatype.stringValue))
      case literal: Literal                                => ResourceFactory.createStringLiteral(literal.getLabel)
    }
    ResourceFactory.createStatement(subject, predicate, obj)
  }

}