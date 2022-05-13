package edu.paradigmas
import scala.io.Source
import scala.util.{Try,Success,Failure}

case class Subscription(
  url: String,
  urlParams: Seq[String],
  urlType: String
)

case class CorrectSubscription(
  url: String,
  request: FeedRequester
)

class ProcessSubscription {

  def getCorrectRequest(urltype: String): FeedRequester = {
    urltype match {
        case "rss" => new parserXML
        case "reddit" => new parserJSON
      }
  }

  def getAllRequest(subscription: Subscription): Seq[CorrectSubscription] = {
    if(subscription.urlParams.length > 0) {
      subscription.urlParams.map { p =>
        CorrectSubscription(subscription.url.replace("%s", p), getCorrectRequest(subscription.urlType))
      }
    } else {
        Seq(CorrectSubscription(subscription.url, getCorrectRequest(subscription.urlType)))
      }
  }

  def getContent(subscriptions: Seq[CorrectSubscription]): Seq[String] = {
    subscriptions.flatMap { post =>
      post.request.parserRequest(post.url) match {
        case Success(x) => x
        case Failure(e) => List()
      }
    }
  }

  def process(subscription: Subscription): Seq[String] = {
    val allRequest = getAllRequest(subscription)
    getContent(allRequest)
  }
}

class ProcessAllSubscription extends ProcessSubscription {

  def getCorrectPost(posts: List[String]): Seq[String] = {
    posts.filter(_.length >= 300)
  }

  def processAll(subscriptions: List[Subscription]): Seq[String] = {
    val content = subscriptions.flatMap { subscription =>
      process(subscription)
    }
    getCorrectPost(content)
  }

}


