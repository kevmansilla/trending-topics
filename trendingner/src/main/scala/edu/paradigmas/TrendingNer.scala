package edu.paradigmas

import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import scala.io._

object TrendingNer extends App {

  // val url = "https://www.chicagotribune.com/arcio/rss/category/sports/" +
  //           "?query=display_date:[now-2d+TO+now]&sort=display_date:desc"

  val url = "https://www.reddit.com/r/Android/hot/.json?count=10"
  val request = new FeedRequester(url)
  val articlesContent = request.parserContentJSON()

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