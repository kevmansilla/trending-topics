package edu.paradigmas

import scalaj.http.{Http, HttpResponse}
import scala.xml.XML

// Obtencion de datos del feed
class FeedRequester(url: String) {

    def getRequest(url: String): HttpResponse[String] = {
        // Step 1: Make the HTTP Request to the url
        Http(url).timeout(connTimeoutMs = 2000, readTimeoutMs = 5000).asString
    }

    def parserContent(): Seq[String] = {

        // Step 2: parse the XML in the response body
        val xmlString = getRequest(url).body
        
        // convert the `String` to a `scala.xml.Elem`
        val xml = XML.loadString(xmlString)
        
        // Extract text from title and description
        (xml \\ "item").map {item => ((item \ "title").text + " " + (item \ "description").text)}
    }
}