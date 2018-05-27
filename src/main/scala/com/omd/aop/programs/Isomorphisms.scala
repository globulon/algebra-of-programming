package com.omd.aop.programs

import scalaz.Isomorphism._
import scalaz.~>

import scala.annotation.tailrec

trait Isomorphisms { self: Recursion with Recursions with Naturalities =>
  implicit final def naturalInt: Nat <=> Int = IsoSet[Nat, Int](
    foldn[Int, Nat](zero[Int], succ[Int]),
    foldn[Nat, Int](zero[Nat], succ[Nat])
  )

  implicit final def isoLists(implicit lr: ListL ~> ListR, rl: ListR ~> ListL): ListL <~> ListR =
    IsoFunctor[ListL, ListR] ( lr, rl )
}

trait Naturalities {
  @tailrec
  private def convert[A](src: ListL[A], tgt: ListR[A]): ListR[A] = src match {
    case LNil        ⇒ tgt
    case Snoc(hs, r) ⇒ convert(hs, cons(r,tgt))
  }

  @tailrec
  private def convert[A](src: ListR[A], tgt: ListL[A]): ListL[A] = src match {
    case RNil       ⇒ tgt
    case Cons(h, t) ⇒ convert(t, snoc(tgt, h))
  }

  implicit final def leftRightL: ListL ~> ListR = new (ListL ~> ListR) {
    override def apply[A](fa: ListL[A]): ListR[A] = convert(fa, RNil)
  }

  implicit final def rightLeft: ListR ~> ListL = new (ListR ~> ListL) {
    override def apply[A](fa: ListR[A]): ListL[A] = convert(fa, LNil)
  }
}
