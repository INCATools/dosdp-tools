package org.monarchinitiative.dosdp

import org.monarchinitiative.dosdp.cli.Config
import zio.logging.Logging
import zio.test._


object ParseAnnotationsTest extends DefaultRunnableSpec {

  def spec = suite("Annotations parsing") {
    testM("Annotation types should be distinguishable") {
      for {
        dosdp <- Config.inputDOSDPFrom("src/test/resources/org/monarchinitiative/dosdp/test_annotation_parsing.yaml")
        annotations = dosdp.annotations.toList.flatten.to(Set)
        ann1 = IRIValueAnnotation(Some(List(PrintfAnnotation(None, "image_comment", Some("%s"), Some(List("image_comment")), None))), "depicted_by", "depicted_by")
        ann2 = ListAnnotation(None, "created_by", "dbxref")
        ann3 = PrintfAnnotation(None, "comment", Some("%s"), Some(List("comment")), None)
      } yield assertTrue(annotations(ann1)) && assertTrue(annotations(ann2)) && assertTrue(annotations(ann3))
    }
  }.provideCustomLayer(Logging.consoleErr())

}
