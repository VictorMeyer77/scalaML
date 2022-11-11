package com.victormeyer.scalaml

import org.apache.log4j.Logger

/** Trait to implement to log with log4j.
 *
 */
trait Logging {

  val logger: Logger = Logger.getLogger(this.getClass)

}
