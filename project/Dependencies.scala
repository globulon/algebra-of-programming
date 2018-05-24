import sbt._

import scala.language.{higherKinds, implicitConversions, postfixOps}

trait Dependencies { self: Versions =>
  final protected def scalaz: ModuleID = "org.scalaz" %% "scalaz-core" % scalazVersion withSources()
  final protected def scalaTesting: ModuleID = "org.scalatest" %% "scalatest" % scalaTestVer withSources()
}