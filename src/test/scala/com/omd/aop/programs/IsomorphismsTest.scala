package com.omd.aop.programs

import org.scalatest.{MustMatchers, WordSpec}
import scalaz.Isomorphism._

final class IsomorphismsTest extends WordSpec with MustMatchers with Fixture {
  "Isomorphism between Nat and Int" should {
    "find zeros" in {
      implicitly[Nat <=> Int].from(0) must be (Zero)
      implicitly[Nat <=> Int].to(Zero) must be (0)
    }

    "map values" in {
      implicitly[Nat <=> Int].from(1) must be (one[Nat])
      implicitly[Nat <=> Int].from(2) must be (two[Nat])
      implicitly[Nat <=> Int].from(3) must be (three[Nat])
      implicitly[Nat <=> Int].from(5) must be (five[Nat])
      implicitly[Nat <=> Int].from(7) must be (seven[Nat])

      implicitly[Nat <=> Int].to(one[Nat]) must be (1)
      implicitly[Nat <=> Int].to(two[Nat]) must be (2)
      implicitly[Nat <=> Int].to(three[Nat]) must be (3)
      implicitly[Nat <=> Int].to(five[Nat]) must be (5)
      implicitly[Nat <=> Int].to(seven[Nat]) must be (7)
    }
  }
}
