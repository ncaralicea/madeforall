package org.madeforall.extractor

import scala.util.matching.Regex

/**
 * @author Nicolae Caralicea
 * @version 1.0, 16/01/2013
 */

trait Substitutable {
  def substitute(str: String): String = str
}

trait Recognizable

trait RecognizeAndSubstituteAble extends Recognizable with Substitutable

trait RecognitionPattern {
  val regexPattern: Regex
  def recognize(value: String): Option[RecognizeAndSubstituteAble]
  def substituteString(str: String): String = str
  def replaceAllIn(str: String, substFun: String => String) =
    regexPattern.replaceAllIn(str, m =>
      substituteString(substFun(m.group(0))))
}

case class RecognizableItemsExtractor(recognitionPatterns: List[RecognitionPattern]) {
  def analyzeText(text: String): List[RecognizeAndSubstituteAble] =
    recognitionPatterns.foldLeft(List[RecognizeAndSubstituteAble]())((computed, item) => {
      val matches = for {
        s <- item.regexPattern findAllIn text
        recognized <- item.recognize(s)
      } yield recognized
      computed ::: matches.toList
    })
  def substitute(text: String): String =
    analyzeText(text).foldLeft(text)((computed, item) => item.substitute(computed))

  def filterByType[T <: Recognizable](t: List[Recognizable])(implicit mf: Manifest[T]) =
    t.filter(e => mf.erasure.isInstance(e))
}



