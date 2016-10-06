package au.gov.csc.model
/*
import java.util.Date
import net.liftweb.common._
import scala.xml.{ NodeSeq, Text }
import net.liftweb.util.Helpers._

import au.gov.csc.model.SessionState._

package com.google.cloud.examples.pubsub.snippets

import com.google.cloud.pubsub.Message
import com.google.cloud.pubsub.PubSub
import com.google.cloud.pubsub.PubSub.MessageConsumer
import com.google.cloud.pubsub.PubSub.MessageProcessor
import com.google.cloud.pubsub.PubSubOptions
import com.google.cloud.pubsub.Subscription
import com.google.cloud.pubsub.SubscriptionInfo

/**
  * A snippet for Google Cloud Pub/Sub showing how to create a Pub/Sub pull subscription and
  * asynchronously pull messages from it.
  */

trait Synchronizer

  class SubscribeAndPullMessages{

    def PublishMessage(MessageText: String): option[Exception] = try{
      Message message1 = Message.of(Message)
      topic.publishAsync(message1)
    }

    def CreateSubscription

    Subscription subscription =
      pubsub.create(SubscriptionInfo.of("test-topic", "test-subscription"));



      memberFacts.get(memberNumber) match {
      case Some(m) => Right(m)
      case None    => Left(MemberNotFoundException)
    }

  }




def getMember(memberNumber: String): Either[Exception, Member]
*/ 