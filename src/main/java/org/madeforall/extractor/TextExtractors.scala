package org.madeforall.extractor

import scala.util.matching.Regex

/**
 * @author Nicolae Caralicea
 * @version 1.0, 16/01/2013
 */


trait Recognizable {
  val recognitionPattern: RecognitionPattern
}

trait RecognizableAndSubstitutable extends Recognizable {
  def recognizeAndSubstitute(str: String): String = 
    recognitionPattern.recognizeAndSubstitute(str)
}

trait RecognitionPattern {
  val regexPattern: Regex
  def recognize(value: String): Option[RecognizableAndSubstitutable]
  def replaceAllIn(str: String, substFun: String => String) =
    regexPattern.replaceAllIn(str, m => substFun(m.group(0)))
  def recognizeAndSubstitute(str: String): String =
    str
}

case class RecognizableItemsExtractor(recognitionPatterns: List[RecognitionPattern]) {
  def analyzeText(text: String): List[RecognizableAndSubstitutable] =
    recognitionPatterns.foldLeft(List[RecognizableAndSubstitutable]())((computed, item) => {
      val matches = for {
        s <- item.regexPattern findAllIn text
        recognized <- item.recognize(s)
      } yield recognized
      computed ::: matches.toList
    })
  def makeSubstitutions(text: String): String =
    analyzeText(text).foldLeft(text)((computed, item) =>      
      item.recognizeAndSubstitute(computed)
  )

  def filterByType[T <: Recognizable](t: List[Recognizable])(implicit mf: Manifest[T]) =
    t.filter(e => mf.erasure.isInstance(e))
}



