package org.madeforall.extractor.test

import org.scalatest._
import org.scalatest.matchers._
import org.madeforall.extractor._

/**
 * @author Nicolae Caralicea
 * @version 1.0, 16/01/2013
 */

case class MacroProperty(propertyName: String) extends RecognizableAndSubstitutable {
  val recognitionPattern = MacroPropertyRecognitionPattern
}
object MacroPropertyRecognitionPattern extends RecognitionPattern {
  // The extraction method (mandatory)
  def unapply(str: String): Option[String] = {
    val regexPattern(propertyName) = str
    Some(propertyName)
  }
  // macro_extract_property regex
  val regexPattern = """#macro_extract_property\((.*)\)""".r

  def recognize(value: String): Option[RecognizableAndSubstitutable] =
    doRecognize(value)

  private def doRecognize(value: String): Option[MacroProperty] = {
    value match {
      case MacroPropertyRecognitionPattern(propertyName) =>
        Some(MacroProperty(propertyName))
      case _ =>
        None
    }
  }
  override def recognizeAndSubstitute(str: String): String = {
    def substituteString(toSubstitute: String): String =
      doRecognize(toSubstitute) match {
        case Some(MacroProperty(propertyName)) => "[" + propertyName + "]"
        case _ => toSubstitute
      }
    replaceAllIn(str, substituteString)
  }
}

/**
 * @author Nicolae Caralicea
 * @version 1.0, 16/01/2013
 */
class TextMacroLikeExtractorSpec extends FlatSpec with ShouldMatchers {

  val textToAnalyze = """
    Nicolae is at home. 
	  -<<noise>>- and email: abc@xyz.xy
	  -@@noise@-	  <a href="http://www.abc.com/">Visit abc</a> 	  -noise-
    
		  blah ...  #macro_extract_property(abc) blah...
		  #macro_extract_property(efg)
	  -noise-href..s.s.
      - 514-232-2647 -jjj
	  """

  val expectedSubstitutedText = """
    Nicolae is at home. 
	  -<<noise>>- and email: abc@xyz.xy
	  -@@noise@-	  <a href="http://www.abc.com/">Visit abc</a> 	  -noise-
    
		  blah ...  [abc] blah...
		  [efg]
	  -noise-href..s.s.
      - 514-232-2647 -jjj
	  """

  val expectedPropertyNameList = List(MacroProperty("abc"), MacroProperty("efg"))

  "A text extractor using a MacroPropertyRecognitionPattern" should "extract MacroProperties from a text" in {
    val recog = RecognizableItemsExtractor(List(MacroPropertyRecognitionPattern))
    val macroPropertyList = recog.analyzeText(textToAnalyze)
    assert(macroPropertyList === expectedPropertyNameList)
  }
  "A text extractor using a MacroPropertyRecognitionPattern" should "substitute each MacroProperty from a text" in {
    val recog = RecognizableItemsExtractor(List(MacroPropertyRecognitionPattern))
    
    val substituted = recog.makeSubstitutions(textToAnalyze)
    
    assert(expectedSubstitutedText === substituted)
  }
}