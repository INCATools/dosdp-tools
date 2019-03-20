package org.monarchinitiative.dosdp

import java.io.File

import com.github.tototoshi.csv.{CSVReader, TSVFormat}
import org.monarchinitiative.dosdp.cli.Generate
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.vocab.DublinCoreVocabulary

class OverrideTest extends UnitSpec {
  
  val DCSource = AnnotationProperty(DublinCoreVocabulary.SOURCE.getIRI)
  val dosdp = Generate.inputDOSDPFrom("src/test/resources/org/monarchinitiative/dosdp/OverrideTest.yaml")
  val fillers = CSVReader.open(new File("src/test/resources/org/monarchinitiative/dosdp/OverrideTest.tsv"), "utf-8")(new TSVFormat {}).iteratorWithHeaders
  val ontology = OWLManager.createOWLOntologyManager().loadOntology(IRI.create(new File("src/test/resources/org/monarchinitiative/dosdp/OverrideTest.ofn")))
  val axioms = Generate.renderPattern(dosdp: DOSDP, OBOPrefixes, fillers: Iterator[Map[String, String]], Some(ontology), true, true, None, false)
  axioms.foreach(println)

  "Overrides" should "be checked" in {
    axioms should contain(Class("http://ex.org/1") Annotation(RDFSLabel, "Entity 1 thing"))
    axioms should contain(Class("http://ex.org/2") Annotation(RDFSLabel, "Entity 2 TSV"))
    axioms should contain(Class("http://ex.org/3") Annotation(RDFSLabel, "http://example.org/Entity3 thing"))
    axioms should contain(Class("http://ex.org/4") Annotation(RDFSLabel, "Entity4LabelInTSV thing"))
    axioms should contain(Class("http://ex.org/5") Annotation(RDFSLabel, "Entity5LabelInOnt thing"))
    axioms should contain(Class("http://ex.org/1") Annotation(DCSource, "overridden source 1"))
    axioms should contain(Class("http://ex.org/2") Annotation(DCSource, "The source is source2"))
    axioms should contain(Class("http://ex.org/3") Annotation(DCSource, "The source is source3"))

    axioms shouldNot contain(Class("http://ex.org/2") Annotation(RDFSLabel, "Entity 2 thing"))
    axioms shouldNot contain(Class("http://ex.org/1") Annotation(DCSource, "The source is source1"))
    axioms shouldNot contain(Class("http://ex.org/5") Annotation(RDFSLabel, "Entity5LabelInTSV thing"))
  }

}
