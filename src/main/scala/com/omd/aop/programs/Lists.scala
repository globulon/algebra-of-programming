package com.omd.aop.programs

import scalaz.Isomorphism.<~>

import scala.annotation.tailrec

trait Lists { self: Isomorphisms ⇒
  final def cons[A](h: A, r: ListR[A]): ListR[A] = Cons(h, r)
  final def nilR[A]: ListR[A] = RNil

  final def snoc[A](h: ListL[A], r: A): ListL[A] = Snoc(h, r)
  final def nilL[A]: ListL[A] = LNil

  final def convert[A](ll: ListL[A]): ListR[A] = implicitly[ListL <~> ListR].to(ll)
  final def convert[A](lr: ListR[A]): ListL[A] = implicitly[ListL <~> ListR].from(lr)

  @tailrec
  private def concat[A](as: ListL[A], acc: ListR[A]): ListR[A] = as match {
    case LNil        ⇒ acc
    case Snoc(hs, t) ⇒ concat(hs, Cons(t, acc))
  }

  final def concat[A](as: ListR[A], aas: ListR[A]): ListR[A] = concat(convert(as), aas)

  implicit class RichListR[A](as: ListR[A]) {
    def ⧺[AA <: A] (other: ListR[AA]): ListR[A] = concat(as, other)
  }

}