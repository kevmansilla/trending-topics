package edu.paradigmas

import scalaj.http.{Http, HttpResponse}
import scala.xml.XML

import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._

// Obtencion de datos del feed
class FeedRequester(url: String) {

  def getRequest(url: String): HttpResponse[String] = {
    // Step 1: Make the HTTP Request to the url
    Http(url).timeout(connTimeoutMs = 2000, readTimeoutMs = 5000).asString
  }
  
  def parserContentXML(): Seq[String] = {
    // Step 2: parse the XML in the response body
    val xmlString = getRequest(url).body //preguntar bn body -> devuelve objeto ?
    // convert the `String` to a `scala.xml.Elem`
    val xml = XML.loadString(xmlString)
    // Extract text from title and description
    (xml \\ "item").map {item => ((item \ "title").text + " " + (item \ "description").text)}
  }
  
  def parserContentJSON(): Seq[String] = {
    implicit val formats = DefaultFormats
    
    val word = "(https?|ftp|file)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]".r
    val response = getRequest(url).body

    val result = (parse(response) \ "data" \ "children" \ "data").extract[List[Map[String, Any]]] 

    result.map(x => x.get("title").getOrElse("").toString + x.get("selftext").getOrElse("").toString).toSeq
                    .map(x => word replaceAllIn(x, ""))
  }
}