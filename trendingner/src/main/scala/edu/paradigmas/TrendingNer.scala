package edu.paradigmas

import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import scala.io._

object TrendingNer extends App {

  implicit val formats = DefaultFormats

  val subsFile = Source.fromFile("subscriptions.json")
  val subscriptions = (parse(subsFile.mkString)).extract[List[Subscription]]

  val processedSubscriptions = new ProcessAllSubscription
  val articlesContent = processedSubscriptions.processAll(subscriptions)

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

  println("\nThe 20 Most Named Entities: ")
  println("---------------------------\n")

  val INIT_INDEX = 0
  val MAX_FEEDS = 20
  sortedNEs.slice(INIT_INDEX, MAX_FEEDS).foreach {
    case (namedEntity, count) => {
      println(s"- $namedEntity: $count")
    }
  }
  println("\n")

}
