package org.madeforall.extractor.test

import org.scalatest._
import org.scalatest.matchers._
import org.madeforall.extractor._

/**
 * @author Nicolae Caralicea
 * @version 1.0, 16/01/2013
 */
class TextExtractorSpec extends FlatSpec with ShouldMatchers {

  val textToAnalyze = """
    Nicolae is at home. 
	  -<<noise>>- and email: abc@xyz.xy
	  -@@noise@-	  <a href="http://www.abc.com/">Visit abc</a> 	  -noise-
	  -noise-href..s.s.
      - 514-232-2647 -jjj
	  """

  val expectedEmail = Email("abc", "xyz.xy")
  val expectedLink = Link("http://www.abc.com/", "Visit abc")
  val expectedPhone = Phone("514", "232", "2647")
    
  "A text extractor using an EmailRecognitionPattern" should "extract email addresses from a text" in {
    val recog = RecognizableItemsExtractor(List(EmailRecognitionPattern))
    val emailList = recog.analyzeText(textToAnalyze)
    assert(emailList === List(expectedEmail))
  }
  
  "A text extractor using a LinkRecognitionPattern" should "extract links from a text" in {
    val recog = RecognizableItemsExtractor(List(LinkRecognitionPattern))
    val linkList = recog.analyzeText(textToAnalyze)
    assert(linkList === List(expectedLink))
  }
  
  "A text extractor using a PhoneRecognitionPattern" should "extract Canadian phone numbers from a text" in {
    val recog = RecognizableItemsExtractor(List(PhoneRecognitionPattern))
    val list = recog.analyzeText(textToAnalyze)
    assert(list === List(expectedPhone))
  }
  
  "A text extractor using a mix of EmailRecognitionPattern, LinkRecognitionPattern, and PhoneRecognitionPattern" should 
  "extract emails, links, and phones from a text in the same order like the one given in the recognition pattern list" in {
    val recog = RecognizableItemsExtractor(List(EmailRecognitionPattern, LinkRecognitionPattern, PhoneRecognitionPattern))
    val list = recog.analyzeText(textToAnalyze)
    assert(list === List(expectedEmail, expectedLink, expectedPhone))
  }
  
  "Filtering emails" should "return only emails" in {
    val recog = RecognizableItemsExtractor(List(EmailRecognitionPattern, LinkRecognitionPattern, PhoneRecognitionPattern))
    val list = recog.analyzeText(textToAnalyze)
    val filteredList = recog.filterByType[Email](list)
    assert(filteredList === List(expectedEmail))    
  }
  
  "Filtering links" should "return only links" in {
    val recog = RecognizableItemsExtractor(List(EmailRecognitionPattern, LinkRecognitionPattern, PhoneRecognitionPattern))
    val list = recog.analyzeText(textToAnalyze)
    val filteredList = recog.filterByType[Link](list)
    assert(filteredList === List(expectedLink))    
  }
  
  "Filtering phones" should "return only phones" in {
    val recog = RecognizableItemsExtractor(List(EmailRecognitionPattern, LinkRecognitionPattern, PhoneRecognitionPattern))
    val list = recog.analyzeText(textToAnalyze)
    val filteredList = recog.filterByType[Phone](list)
    assert(filteredList === List(expectedPhone))    
  }
}


