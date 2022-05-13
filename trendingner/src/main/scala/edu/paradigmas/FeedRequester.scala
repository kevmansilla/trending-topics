package edu.paradigmas

import scalaj.http.{Http, HttpResponse}
import scala.xml.XML
import scala.io.Source
import scala.util.{Try,Success,Failure}

import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._

abstract class FeedRequester {

  def getRequest(url: String): Try[String] = {
    val CONN_TIMEOUT = 2000
    val READ_TIMEOUT = 5000
    Try(Http(url).timeout(connTimeoutMs = CONN_TIMEOUT, readTimeoutMs = READ_TIMEOUT).asString.body)
  }

  def cleanContent(texts: Seq[String]): Try[Seq[String]] = {
    val word = "(https?|ftp|file)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]".r
    Try(texts.map(content => word.replaceAllIn(content, " ")))
  }

  def parserRequest(url: String): Try[Seq[String]]
}

class parserXML extends FeedRequester {

  def parserRequest(url: String): Try[Seq[String]] = {
    getRequest(url).flatMap { x =>
      val xmlContent = XML.loadString(x)
      val texts = (xmlContent \\ "item").map {item =>
        (item \ "title").text + " " + (item \ "description").text
      }
      cleanContent(texts)
    }
  }
}

class parserJSON extends FeedRequester {

  def parserRequest(url: String): Try[Seq[String]] = {

    implicit val formats = DefaultFormats
    getRequest(url).flatMap { x=>
      val jsonContent = (parse(x) \ "data" \ "children" \ "data").extract[List[Map[String, Any]]]
      val texts = jsonContent.map { item =>
        item.get("title").getOrElse("").toString + " " + item.get("selftext").getOrElse("").toString
      }
      cleanContent(texts)
    }
  }
}
