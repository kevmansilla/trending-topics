package edu.paradigmas

import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import scala.io._

import scalaj.http.{Http, HttpResponse}
import scala.xml.XML


/*
 * Main class
 */
object TrendingNer extends App {

  // ************************* GET THE ARTICLES *************************
  // Tutorial https://alvinalexander.com/source-code/scala-how-to-http-download-xml-rss-feed-timeout/
  // get the xml content using scalaj-http
  val url = "https://www.chicagotribune.com/arcio/rss/category/sports/" +
    "?query=display_date:[now-2d+TO+now]&sort=display_date:desc"
  // Step 1: Make the HTTP Request to the chicago tribute website
  val response: HttpResponse[String] = Http(url)
    .timeout(connTimeoutMs = 2000, readTimeoutMs = 5000)
    .asString
  // Step 2: parse the XML in the response body
  val xmlString = response.body
  // convert the `String` to a `scala.xml.Elem`
  val xml = XML.loadString(xmlString)
  // Extract text from title and description
  val articlesContent: Seq[String] = (xml \\ "item").map {
    item => ((item \ "title").text + " " + (item \ "description").text)
  }
  // To see what's inside rssText, use the following line
  // rssText.foreach { println }


  // *********************** APPLY THE MODEL ***************************
  // We need this variables for the model
  val STOPWORDS = Seq (
    "i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you",
    "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she",
    "her", "hers", "herself", "it", "its", "itself", "they", "them", "your",
    "their", "theirs", "themselves", "what", "which", "who", "whom",
    "this", "that", "these", "those", "am", "is", "are", "was", "were",
    "be", "been", "being", "have", "has", "had", "having", "do", "does",
    "did", "doing", "a", "an", "the", "and", "but", "if", "or",
    "because", "as", "until", "while", "of", "at", "by", "for", "with",
    "about", "against", "between", "into", "through", "during", "before",
    "after", "above", "below", "to", "from", "up", "down", "in", "out",
    "off", "over", "under", "again", "further", "then", "once", "here",
    "there", "when", "where", "why", "how", "all", "any", "both", "each",
    "few", "more", "most", "other", "some", "such", "no", "nor", "not",
    "only", "own", "same", "so", "than", "too", "very", "s", "t", "can",
    "will", "just", "don", "should", "now", "on",
    // Contractions without '
    "im", "ive", "id", "Youre", "youd", "youve",
    "hes", "hed", "shes", "shed", "itd", "were", "wed", "weve",
    "theyre", "theyd", "theyve",
    "shouldnt", "couldnt", "musnt", "cant", "wont",
    // Common uppercase words
    "hi", "hello"
  )
  val punctuationSymbols = ".,()!?;:'`´\n"
  val punctuationRegex = "\\" + punctuationSymbols.split("").mkString("|\\")

  // This is the model that extracts the Named Entities from a single text
  def getNEsSingle(text: String): Seq[String] =
    text.replaceAll(punctuationRegex, "").split(" ")
      .filter { word:String => word.length > 1 &&
                Character.isUpperCase(word.charAt(0)) &&
                !STOPWORDS.contains(word.toLowerCase) }.toSeq

  val extractedNEs: Seq[Seq[String]] = articlesContent.map(getNEsSingle)

  (articlesContent zip extractedNEs).foreach {
    case (article, namedEntities) => {
      println("*********************************")
      println(article)
      println(namedEntities)
      println("*********************************")
    }
  }


  // ****************** COUNT AND SORT THE ENTITIES ************************
  val neCounts: Map[String, Int] = extractedNEs.flatten
    .foldLeft(Map.empty[String, Int]) {
      (count, word) => count + (word -> (count.getOrElse(word, 0) + 1)) }
  val sortedNEs: List[(String, Int)] = neCounts.toList
    .sortBy(_._2)(Ordering[Int].reverse)

  sortedNEs.foreach {
    case (namedEntity, count) => {
      println(s"$namedEntity : $count")
    }
  }
}