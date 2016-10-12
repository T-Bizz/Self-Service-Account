package au.gov.csc.model

import au.gov.csc.comet.{ NavigationMessage, PushActorManager }
import au.gov.csc.model.state.SessionState
import net.liftweb.common._
import com.google.cloud.pubsub.{ Message, PubSub, PubSubOptions, PushConfig, Subscription, Topic }
import com.google.cloud.pubsub.PubSub.MessageConsumer
import com.google.cloud.pubsub.PubSub.MessageProcessor
import com.google.cloud.pubsub.Topic
import com.google.cloud.pubsub.TopicInfo
import com.google.cloud.pubsub.PubSub
import com.google.cloud.pubsub.PubSubOptions
import com.google.cloud.pubsub.Subscription
import com.google.cloud.pubsub.SubscriptionInfo

import scala.pickling.Defaults._
import scala.pickling.json._

object SubscribeAndPullMessages extends Logger {

  var ps: Option[PubSub] = None
  var sub: Option[Subscription] = None
  var navigationTopic: Option[Topic] = None
  var tokenTopic: Option[Topic] = None
  var accessAttemptTopic: Option[Topic] = None
  var myTopic: Option[Topic] = None
  var mySubscription: Option[Subscription] = None

  private def getPubSub: PubSub = {
    ps match {
      case Some(p) => p
      case None => {
        info(PubSubOptions.defaultInstance().projectId())
        info(PubSubOptions.defaultInstance().applicationName())
        info(PubSubOptions.defaultInstance().authCredentials())

        ps = Some(PubSubOptions.defaultInstance().service())
        ps.get
      }
    }
  }

  private def getNavigationTopic(topic: String): Topic = {
    navigationTopic match {
      case Some(p) => p
      case None => {

        navigationTopic = Some(getPubSub.getTopic(topic))
        navigationTopic.get
      }
    }
  }

  private def getOrCreateTopic(topic: String): Topic = {
    myTopic match {
      case Some(p) => p
      case None => {

        try {
          myTopic = Some(getPubSub.getTopic(topic))
        } catch {
          case t: Topic => {
            myTopic = Some(getPubSub.create(TopicInfo.of(topic)))
          }
        }
        myTopic.get
      }
    }
  }

  private def getOrCreateSubscription(subscription: String, topic: String): Subscription = {
    mySubscription match {
      case Some(p) => p
      case None => {

        try {
          mySubscription = Some(getPubSub.getSubscription(subscription))
        } catch {
          case t: Subscription => {
            mySubscription = Some(getPubSub.create(SubscriptionInfo.of(topic, subscription)))
          }
        }
        mySubscription.get
      }
    }
  }

  private def pushMessage(topic: Topic, message: String) {
    info("Sending pubsub message (%s) to topic (%s)".format(message, topic))
    topic.publish(Message.of(message))
  }

  private def processNavigationMessage: MessageProcessor = {
    new MessageProcessor {
      override def process(message: Message) = {
        val msg = message.payloadAsString()
        info("Received gcloud pubsub message %s".format(msg))
        val pkl = JSONPickle(msg)
        val nm: NavigationMessage = pkl.unpickle[NavigationMessage]
        PushActorManager.!(nm)
        info("Sent navigation message to comet: %s".format(nm))
      }
    }
  }

  def pushNavigationMessage(message: NavigationMessage) {
    val pkl: String = message.pickle.value
    pushMessage(getNavigationTopic("serverSync"), pkl)
  }

  def pullNavigationMessages {
    // Create a message consumer and continuously pull messages
    val consumer: MessageConsumer = getPubSub.pullAsync("serverSync", processNavigationMessage)
  }

  def pullMessages(subscription: String) = {
    val consumer: MessageConsumer = getPubSub.pullAsync(subscription, processNavigationMessage)
  }
}
