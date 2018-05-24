import sbt._

import scala.language.{higherKinds, implicitConversions, postfixOps}

trait TestDependencies { self: Versions =>
  final protected def scalaTest: ModuleID = "org.scalatest" %% "scalatest" % scalaTestVer % Test withSources()
}