package edu.paradigmas

import scalaj.http.{Http, HttpResponse}
import scala.xml.XML

import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._

abstract class FeedRequester {

  val word = "(https?|ftp|file)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]".r
  
  def getRequest(url: String): HttpResponse[String] = {
    Http(url).timeout(connTimeoutMs = 2000, readTimeoutMs = 5000).asString
  }

  def parserRequest(url: String): Seq[String]
}

class parserXML extends FeedRequester {
  def parserRequest(url: String): Seq[String] = {
    val xmlString = getRequest(url).body 
    val xml = XML.loadString(xmlString)
    val conent = (xml \\ "item").map {item => ((item \ "title").text + " " + (item \ "description").text)}
    // replaceAllIn es un metodo de expresiones regulares, que dado un texto si encuentra alguna expresion la remplaza.
    conent.map(x => word.replaceAllIn(x, ""))
  }
}

class parserJSON extends FeedRequester {
  
  implicit val formats = DefaultFormats
  
  def parserRequest(url: String): Seq[String] = {
    val response = getRequest(url).body
    val result = (parse(response) \ "data" \ "children" \ "data").extract[List[Map[String, Any]]] 
    result.map (x =>
      x.get("title").getOrElse("").toString + 
      x.get("selftext").getOrElse("").toString 
    .toSeq).map(x => word.replaceAllIn(x, ""))
  }
}