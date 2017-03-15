package org.monarchinitiative.dosdp

import scala.collection.JavaConverters._

import org.apache.jena.datatypes.TypeMapper
import org.apache.jena.rdf.model.AnonId
import org.apache.jena.rdf.model.ResourceFactory
import org.apache.jena.rdf.model.Statement
import org.apache.jena.rdf.model.impl.ResourceImpl
import org.openrdf.model.BNode
import org.openrdf.model.Literal
import org.openrdf.model.{ Statement => SesameStatement }
import org.openrdf.model.URI
import org.openrdf.rio.helpers.StatementCollector
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.rio.RioRenderer

object SesameJena {

  def ontologyAsTriples(ontology: OWLOntology): Set[Statement] = {
    val collector = new StatementCollector();
    new RioRenderer(ontology, collector, null).render();
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
      case literal: Literal if literal.getDatatype != null => ResourceFactory.createTypedLiteral(literal.getLabel,
        TypeMapper.getInstance.getSafeTypeByName(literal.getDatatype.stringValue))
      case literal: Literal => ResourceFactory.createStringLiteral(literal.getLabel)
    }
    ResourceFactory.createStatement(subject, predicate, obj)
  }

}