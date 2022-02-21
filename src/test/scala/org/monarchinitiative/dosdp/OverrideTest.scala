package org.monarchinitiative.dosdp

import com.github.tototoshi.csv.TSVFormat
import org.monarchinitiative.dosdp.cli.{Config, Generate}
import org.phenoscape.scowl.{not => _, _}
import org.semanticweb.owlapi.model.OWLAnnotationProperty
import org.semanticweb.owlapi.vocab.DublinCoreVocabulary
import zio.logging._
import zio.test.Assertion._
import zio.test._

import java.io.File

object OverrideTest extends DefaultRunnableSpec {

  val DCSource: OWLAnnotationProperty = AnnotationProperty(DublinCoreVocabulary.SOURCE.getIRI)

  def spec = suite("Overrides") {
    testM("Overrides should be checked") {
      for {
        ontology <- Utilities.loadOntology("src/test/resources/org/monarchinitiative/dosdp/OverrideTest.ofn", None)
        dosdp <- Config.inputDOSDPFrom("src/test/resources/org/monarchinitiative/dosdp/OverrideTest.yaml")
        columnsAndFillers <- Generate.readFillers(new File("src/test/resources/org/monarchinitiative/dosdp/OverrideTest.tsv"), new TSVFormat {})
        (_, fillers) = columnsAndFillers
        axioms <- Generate.renderPattern(dosdp, OBOPrefixes, fillers, Some(ontology), true, true, None, false, AxiomRestrictionsTest.OboInOwlSource, false, Map.empty)
      } yield assert(axioms)(contains(Class("http://ex.org/1") Annotation(RDFSLabel, "Entity 1 thing"))) &&
        assert(axioms)(contains(Class("http://ex.org/2") Annotation(RDFSLabel, "Entity 2 TSV"))) &&
        assert(axioms)(contains(Class("http://ex.org/3") Annotation(RDFSLabel, "http://example.org/Entity3 thing"))) &&
        assert(axioms)(contains(Class("http://ex.org/4") Annotation(RDFSLabel, "Entity4LabelInTSV thing"))) &&
        assert(axioms)(contains(Class("http://ex.org/5") Annotation(RDFSLabel, "Entity5LabelInOnt thing"))) &&
        assert(axioms)(contains(Class("http://ex.org/1") Annotation(DCSource, "overridden source 1"))) &&
        assert(axioms)(contains(Class("http://ex.org/2") Annotation(DCSource, "The source is source2"))) &&
        assert(axioms)(contains(Class("http://ex.org/3") Annotation(DCSource, "The source is source3"))) &&
        assert(axioms)(not(contains(Class("http://ex.org/2") Annotation(RDFSLabel, "Entity 2 thing")))) &&
        assert(axioms)(not(contains(Class("http://ex.org/1") Annotation(DCSource, "The source is source1")))) &&
        assert(axioms)(not(contains(Class("http://ex.org/5") Annotation(RDFSLabel, "Entity5LabelInTSV thing"))))
    }
  }.provideCustomLayer(Logging.consoleErr())

}
