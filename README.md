madeforall
==========

Multi-purpose repository


Text Recognition
----------------

Let's assume that we have some text based content (txt, html, etc) and that our goal is to achieve 
the followings: 

- We want to be able to easily extract email addresses, links, images, etc from the above content
- We want to have a decent and common data format of what we extract
- We want to easily add new patterns to the existent text recognition patterns, so we would be able 
	to recognize more patterns in our text based contents


The madeforall's extractor library is a Scala based library that is meant to help us reach the above goals.

Here are some steps:

We create a RecognizableItemsExtractor object. We pass as a parameter an RecognitionPattern object
Any RecognitionPattern object should:
- implement the unapply extraction method (Scala based extractor)
- mix in the RecognitionPattern trait
  
To support email recognition, we could do the followings:

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

Then, to extract all the emails from our text we could do the followings:
val textToAnalyze: String = "<<some content>>"
val recog = RecognizableItemsExtractor(List(EmailRecognitionPattern))
val emailList: List[Recognizable] = recog.analyzeText(textToAnalyze)

If we want to extract emails and links we can also add more recognition patterns 
to the above RecognizableItemsExtractor.

RecognizableItemsExtractor(List(EmailRecognitionPattern, LinkRecognitionPattern))
val emailAndLinkList: List[Recognizable] = recog.analyzeText(textToAnalyze)

The above emailAndLinkList list contains both emails and links.
To filter out emails we could use the following filterByType function.

val onlyEmailList = recog.filterByType[Email](emailAndLinkList)

Here is the definition of the RecognizableItemsExtractor's filterByType function:

  def filterByType[T <: Recognizable](t: List[Recognizable])(implicit mf: Manifest[T]) =
    t.filter(e => mf.erasure.isInstance(e))

It is based on Scala class manifest feature, that is helping the runtime 
with the type information provided as hint by the Scala compiler. 
Type erasure is still present in Scala like in Java. 
The creators of Scala gave us this helpful manifest class support to overcome 
the JVM's type erasure limitations.

 


   
   
       
