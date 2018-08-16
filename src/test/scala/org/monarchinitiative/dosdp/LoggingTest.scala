package org.monarchinitiative.dosdp

import com.typesafe.scalalogging.LazyLogging

class LoggingTest extends UnitSpec with LazyLogging {

  "Using logging" should "not cause NoClassDefFoundError" in {

    logger.error("This is an expected error message for testing.")

  }

}