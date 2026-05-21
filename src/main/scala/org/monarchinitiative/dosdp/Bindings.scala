package org.monarchinitiative.dosdp

import scala.util.matching.Regex.Match

sealed trait Binding extends Product with Serializable

final case class SingleValue(value: String) extends Binding

final case class MultiValue(value: Set[String]) extends Binding

final case class ExpandedRegexSub(regexSub: RegexSub) {

  private val groupFinder = raw"\\(\d+)".r

  private val regex = regexSub.`match`.r

  /** Apply `regexSub` to a value; if the pattern does not match, return the value unchanged. */
  def substitute(value: String): String =
    regex.findFirstMatchIn(value).map { valueMatch =>
      groupFinder.replaceAllIn(regexSub.sub, (placeholder: Match) => valueMatch.group(placeholder.group(1).toInt))
    }.getOrElse(value)

  def expandBindings(bindings: Map[String, Binding]): Map[String, Binding] =
    bindings.get(regexSub.in).map {
      case SingleValue(value) => regexSub.out -> SingleValue(substitute(value))
      case MultiValue(values) => regexSub.out -> MultiValue(values.map(substitute))
    }.fold(bindings)(bindings + _)

}
