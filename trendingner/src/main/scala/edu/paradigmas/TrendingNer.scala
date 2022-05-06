package edu.paradigmas

import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import scala.io._

case class Subscription(
  url: String,
  urlParams: List[String],
  urlType: String
)

object TrendingNer extends App {

  implicit val formats = DefaultFormats

  val jsonContent = Source.fromFile("subscriptions.json")
  val subscriptions = (parse(jsonContent.mkString)).extract[List[Subscription]]
  
  var articlesContent: Seq[String] = Seq[String]()
  subscriptions.foreach {
    subs => {
      val request = subs.urlType match {
        case "rss" => new parserXML
        case "reddit" => new parserJSON
      }

      subs.urlParams.foreach {
        param => {
          var correctURL = subs.url.replace("%s", param) 
          val content = request.parserRequest(correctURL)
          articlesContent = articlesContent ++ content
        }
      }
    }
  }

  val model = new NERModel
  val extractedNEs: Seq[Seq[String]] = model.getNEs(articlesContent)

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