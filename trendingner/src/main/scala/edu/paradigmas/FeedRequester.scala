package edu.paradigmas

import scalaj.http.{Http, HttpResponse}
import scala.xml.XML

import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._

abstract class FeedRequester {

  def getRequest(url: String): String = {
    val CONN_TIMEOUT = 2000
    val READ_TIMEOUT = 5000
    Http(url).timeout(connTimeoutMs = CONN_TIMEOUT, readTimeoutMs = READ_TIMEOUT).asString.body
  }

  def cleanContent(texts: Seq[String]): Seq[String] = {
    val word = "(https?|ftp|file)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]".r
    val correctContent: Seq[String] = texts.map(content => word.replaceAllIn(content, " "))
    correctContent
  }

  def parserRequest(url: String): Seq[String]
}

class parserXML extends FeedRequester {

  def parserRequest(url: String): Seq[String] = {
    try {
      val xmlRequest = getRequest(url)
      val xmlContent = XML.loadString(xmlRequest)
      val texts = (xmlContent \\ "item").map {item =>
        (item \ "title").text + " " + (item \ "description").text
      }
      cleanContent(texts)
    }
    catch {
      case e: Exception => List()
    }
  }
}

class parserJSON extends FeedRequester {

  def parserRequest(url: String): Seq[String] = {
    try {
      implicit val formats = DefaultFormats
      val jsonRequest = getRequest(url)
      val jsonContent = (parse(jsonRequest) \ "data" \ "children" \ "data").extract[List[Map[String, Any]]]
      val texts = jsonContent.map { item =>
        item.get("title").getOrElse("").toString + " " + item.get("selftext").getOrElse("").toString
      }
      cleanContent(texts)
    }
    catch {
      case e: Exception => List()
    }
  }
}
