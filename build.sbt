name := "Commonwealth Superannuation Self Service Account"

version := "0.0.1"

scalaVersion := "2.11.8"

Seq(webSettings :_*)

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.4"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

libraryDependencies ++= {
  val liftVersion = "2.6.2"
  Seq(
    "net.liftweb"       %% "lift-webkit"        % liftVersion           % "compile",
    "net.liftweb"       %% "lift-testkit"       % liftVersion           % "compile",
    "net.liftweb"       %% "lift-mapper"        % liftVersion           % "compile",
    "net.liftmodules"   %% "fobo_2.6"           % "1.5"                 % "compile",
    "org.specs2" %% "specs2-core" % "3.8.4" % "test",
    "org.eclipse.jetty" % "jetty-webapp"        % "8.1.17.v20150415"    % "container,test",
    "org.eclipse.jetty" % "jetty-plus"          % "8.1.17.v20150415"    % "container,test", // For Jetty Config
    "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "container,test" artifacts Artifact("javax.servlet", "jar", "jar"),
    "commons-codec" % "commons-codec" % "1.10",
    "ch.qos.logback" % "logback-classic" % "1.1.+",
    "rhino" % "js" % "1.7R2",
    "com.google.cloud" % "google-cloud-pubsub" % "0.4.0"
  )
}

import scalariform.formatter.preferences._
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys

SbtScalariform.scalariformSettings

ScalariformKeys.preferences := ScalariformKeys.preferences.value
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DoubleIndentClassDeclaration, true)
  .setPreference(PreserveDanglingCloseParenthesis, true)
  .setPreference(IndentLocalDefs, true)
  .setPreference(AlignArguments, true)

