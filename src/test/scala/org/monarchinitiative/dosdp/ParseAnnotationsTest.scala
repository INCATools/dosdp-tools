package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.Config
import zio.logging.Logging
import zio.test.Assertion._
import zio.test._
import org.monarchinitiative.dosdp.Annotations


object ParseAnnotationsTest extends DefaultRunnableSpec {

  def spec = suite("Annotations parsing") {
    testM("Annotation types should be distinguishable") {
      for {
        dosdp <- Config.inputDOSDPFrom("src/test/resources/org/monarchinitiative/dosdp/test_annotation_parsing.yaml")
        annotations = dosdp.annotations.toList.flatten
        ann1: Annotations = IRIValueAnnotation(Some(List(PrintfAnnotation(None, "image_comment", Some("%s"), Some(List("image_comment")), None))), "depicted_by", "depicted_by")
        ann2: Annotations = ListAnnotation(None, "created_by", "dbxref")
        ann3: Annotations = PrintfAnnotation(None, "comment", Some("%s"), Some(List("comment")), None)
      } yield assertTrue(annotations.contains(ann1)) && assertTrue(annotations.contains(ann2)) && assertTrue(annotations.contains(ann3))
    }
  }.provideCustomLayer(Logging.consoleErr())

}
