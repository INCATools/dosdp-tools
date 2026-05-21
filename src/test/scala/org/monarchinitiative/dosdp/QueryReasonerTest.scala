package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.{Config, Query}
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.reasoner.InferenceType
import zio.{Config => _, _}
import zio.test.Assertion._
import zio.test._

import scala.jdk.CollectionConverters._

object QueryReasonerTest extends ZIOSpecDefault {

  private val prefixes: Map[String, String] = Map(
    "BFO" -> "http://purl.obolibrary.org/obo/BFO_",
    "EX" -> "http://example.org/"
  )

  private val pattern = DOSDP(
    classes = Some(Map("target" -> "EX:Target")),
    relations = Some(Map("part_of" -> "BFO:0000050")),
    vars = Some(Map("entity" -> "'part_of' some 'target'")),
    subClassOf = Some(PrintfOWLConvenience(
      annotations = None,
      text = Some("%s"),
      vars = Some(List("entity"))
    ))
  )

  def spec = suite("Query reasoner expansion")(
    test("anonymous variable ranges without a reasoner produce empty VALUES") {
      for {
        query <- Query.makeProcessedQuery(pattern, prefixes, Config.LogicalAxioms, None)
      } yield assert(query)(containsString("VALUES ?entity { }")) &&
        assert(query)(not(containsString("'part_of' some 'target'")))
    },
    test("ELK expands complex variable ranges into direct VALUES blocks") {
      ZIO.scoped {
        for {
          ontology <- ZIO.attempt {
            val manager = OWLManager.createOWLOntologyManager()
            val factory = manager.getOWLDataFactory
            val target = factory.getOWLClass(IRI.create("http://example.org/Target"))
            val partOfTarget = factory.getOWLClass(IRI.create("http://example.org/PartOfTarget"))
            val candidate = factory.getOWLClass(IRI.create("http://example.org/Candidate"))
            val directCandidate = factory.getOWLClass(IRI.create("http://example.org/DirectCandidate"))
            val nonMatching = factory.getOWLClass(IRI.create("http://example.org/NonMatching"))
            val partOf = factory.getOWLObjectProperty(IRI.create("http://purl.obolibrary.org/obo/BFO_0000050"))
            val ontology = manager.createOntology()
            manager.addAxiom(ontology, factory.getOWLDeclarationAxiom(target))
            manager.addAxiom(ontology, factory.getOWLDeclarationAxiom(partOfTarget))
            manager.addAxiom(ontology, factory.getOWLDeclarationAxiom(candidate))
            manager.addAxiom(ontology, factory.getOWLDeclarationAxiom(directCandidate))
            manager.addAxiom(ontology, factory.getOWLDeclarationAxiom(partOf))
            manager.addAxiom(ontology, factory.getOWLEquivalentClassesAxiom(partOfTarget, factory.getOWLObjectSomeValuesFrom(partOf, target)))
            manager.addAxiom(ontology, factory.getOWLSubClassOfAxiom(candidate, partOfTarget))
            manager.addAxiom(ontology, factory.getOWLSubClassOfAxiom(directCandidate, factory.getOWLObjectSomeValuesFrom(partOf, target)))
            manager.addAxiom(ontology, factory.getOWLDeclarationAxiom(nonMatching))
            ontology
          }
          reasoner <- ZIO.acquireRelease(
            ZIO.attempt(new ElkReasonerFactory().createReasoner(ontology))
          )(reasoner => ZIO.succeed(reasoner.dispose()))
          _ <- ZIO.attempt(reasoner.precomputeInferences(InferenceType.CLASS_HIERARCHY))
          query <- Query.makeProcessedQuery(pattern, prefixes, Config.LogicalAxioms, Some(reasoner))
          queryClasses = ontology.getClassesInSignature().asScala.exists(_.getIRI.toString.startsWith("urn:dosdp:query:"))
        } yield assert(query)(containsString("VALUES")) &&
          assert(query)(containsString("?entity")) &&
          assert(query)(containsString("http://example.org/PartOfTarget")) &&
          assert(query)(containsString("http://example.org/DirectCandidate")) &&
          assert(query)(not(containsString("http://example.org/NonMatching"))) &&
          assert(query)(not(containsString("urn:dosdp:query:"))) &&
          assert(queryClasses)(isFalse)
      }
    }
  )

}
