package com.omd.aop.programs

import org.scalatest.{MustMatchers, WordSpec}

final class RecursionsTest extends WordSpec with MustMatchers with Fixture {
  "plus" should {
    "add zero" in {
      plus(0)(0) must be (0)
      plus(zero[Nat])(zero[Nat]) must be (zero[Nat])
    }

    "add n" in {
      plus(4)(3) must be (7)
      plus(four[Nat])(three[Nat]) must be (seven[Nat])
    }
  }

  "mult" should {
    "mul by one" in {
      mul(1)(1) must be (1)
      mul(one[Nat])(one[Nat]) must be (one[Nat])
    }

    "mul n" in {
      mul(2)(3) must be (6)
      mul(two[Nat])(three[Nat]) must be (six[Nat])
    }
  }
  "exp" should {
    "give one for zero" in {
      exp(3)(0) must be (1)
      exp(three[Nat])(zero[Nat]) must be (one[Nat])
    }

    "mul n" in {
      exp(2)(3) must be (8)
      exp(two[Nat])(three[Nat]) must be (eight[Nat])
    }
  }
}