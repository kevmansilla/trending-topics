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

case class processSub(
  url: String,
  request: FeedRequester
)

object TrendingNer extends App {

  implicit val formats = DefaultFormats

  val jsonContent = Source.fromFile("subscriptions.json")
  val subscriptions = (parse(jsonContent.mkString)).extract[List[Subscription]]

  def getCorrectReq(urltype: String) = {
    urltype match {
      case "rss" => new parserXML
      case "reddit" => new parserJSON
    }
  }

  val correctsURL = subscriptions.map { subs => 
    if(subs.urlParams.length != 0) {
      subs.urlParams.map { p =>
        processSub(subs.url.replace("%s", p), getCorrectReq(subs.urlType))
      }
    } else {
      List(processSub(subs.url, getCorrectReq(subs.urlType)))
    }
  }.flatten

  val articlesContent = correctsURL.map { r =>
    r.request.parserRequest(r.url)
  }.flatten

  val model = new NERModel
  val extractedNEs: Seq[Seq[String]] = model.getNEs(articlesContent)

  // (articlesContent zip extractedNEs).foreach {
  //   case (article, namedEntities) => {
  //     println("*********************************")
  //     println(article)
  //     println(namedEntities)
  //     println("*********************************")
  //   }
  // }


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