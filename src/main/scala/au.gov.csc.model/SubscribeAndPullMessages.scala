package au.gov.csc.model

import au.gov.csc.comet.{ NavigationMessage, PushActorManager }
import au.gov.csc.model.state.SessionState
import net.liftweb.common._
import com.google.cloud.pubsub.{ Message, PubSub, PubSubOptions, PushConfig, Subscription, Topic }
import com.google.cloud.pubsub.PubSub.MessageConsumer
import com.google.cloud.pubsub.PubSub.MessageProcessor

import scala.pickling.Defaults._
import scala.pickling.json._

object SubscribeAndPullMessages extends Logger {

  var ps: Option[PubSub] = None

  def getPubSub: PubSub = {
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

  def getSubscription(subscription: String): Subscription = {
    getPubSub.getSubscription(subscription)
  }

  def getTopic(topic: String): Topic = {
    getPubSub.getTopic(topic)
  }

  def getMessages(subscription: Subscription) {

    val callback: MessageProcessor = new MessageProcessor {
      override def process(message: Message) = {
        info("Received gcloud pubsub message %s".format(message.payloadAsString()))
        processMessage(message.payloadAsString())
      }
    }

    // Create a message consumer and pull messages
    val consumer: MessageConsumer = subscription.pullAsync(callback)
    consumer.close()
  }

  def processMessage(message: String) = {
    val pkl = JSONPickle(message)
    val nm: NavigationMessage = pkl.unpickle[NavigationMessage]
    info("Unpickled gcloud pubsub message %s".format(nm))
    PushActorManager ! nm
  }

  def setupPushMessages(subscription: String) {
    val pushConfig: PushConfig = PushConfig.of("https://104.199.206.248/push");
    getPubSub.replacePushConfig(subscription, pushConfig);
  }

  def pushMessage(topic: Topic, message: String) {
    info("Sending pubsub message (%s) to topic (%s)".format(message, topic))

    val msg: Message = Message.of(message)
    topic.publish(msg)
  }
}
