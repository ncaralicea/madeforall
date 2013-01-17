package org.madeforall.extractor

import scala.util.matching.Regex

trait Recognizable

/**
 * @author Nicolae Caralicea
 * @version 1.0, 16/01/2013
 */
trait RecognitionPattern {
  val regexPattern: Regex
  def recognize(value: String): Option[Recognizable]
}

case class Email(user: String, domain: String) extends Recognizable {
  override def toString() = {
    user + "@" + domain
  }
}
object EmailRecognitionPattern extends RecognitionPattern {
  // The extraction method
  def unapply(str: String): Option[(String, String)] = {
    val parts = str split "@"
    if (parts.length == 2) Some(parts(0), parts(1)) else None
  }
  // email regex
  val regexPattern = """(?i)\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}\b""".r

  def recognize(value: String): Option[Recognizable] = {
    value match {
      case EmailRecognitionPattern(user, domain) =>
        Some(Email(user, domain))
      case _ =>
        None
    }
  }
}

case class Phone(areaCode: String, centralOffice: String, stationNumber: String) extends Recognizable {
  override def toString() = {
    areaCode + "-" + centralOffice + "-" + stationNumber
  }
}

// the area code and between groups of digits within a telephone number Canada: 819-555-5555
object PhoneRecognitionPattern extends RecognitionPattern {
  // The extraction method (mandatory)
  def unapply(str: String): Option[(String, String, String)] = {
    val parts = str split "-"
    if (parts.length == 3) Some((parts(0), parts(1), parts(2))) else None
  }
  // phone number regex
  val regexPattern = """(?i)\b[0-9]{3}-[0-9]{3}-[0-9]{4}\b""".r

  def recognize(value: String): Option[Recognizable] = {
    value match {
      case PhoneRecognitionPattern(areaCode, centralOffice, stationNumber) =>
        Some(Phone(areaCode, centralOffice, stationNumber))
      case _ =>
        None
    }
  }
}

case class Link(link: String, text: String) extends Recognizable {
  override def toString() = {
    link + " --> " + text
  }
}

object LinkRecognitionPattern extends RecognitionPattern {
  // The extraction method (mandatory)
  def unapply(str: String): Option[(String, String)] = {
    val regexPattern(link, text) = str
    Some(link, text)
  }
  // html link regex
  val regexPattern = """<a href=['"]([^"']*)['"].*>(.*)</a""".r

  def recognize(value: String): Option[Recognizable] = {
    value match {
      case LinkRecognitionPattern(link, text) =>
        Some(Link(link, text))
      case _ =>
        None
    }
  }
}

case class RecognizableItemsExtractor(recognitionPatterns: List[RecognitionPattern]) {
  def analyzeText(text: String): List[Recognizable] =
    recognitionPatterns.foldLeft(List[Recognizable]())((computed, item) => {
      val matches = for {
        s <- item.regexPattern findAllIn text
        recognized <- item.recognize(s)
      } yield recognized
      computed ::: matches.toList
    })

  def filterByType[T <: Recognizable](t: List[Recognizable])(implicit mf: Manifest[T]) =
    t.filter(e => mf.erasure.isInstance(e))
}



