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
  private def concatR[A](as: ListL[A], acc: ListR[A]): ListR[A] = as match {
    case LNil        ⇒ acc
    case Snoc(hs, t) ⇒ concatR(hs, Cons(t, acc))
  }

  final def concat[A](as: ListR[A], aas: ListR[A]): ListR[A] = concatR(convert(as), aas)

  @tailrec
  private def concatL[A](acc: ListL[A], as: ListR[A]): ListL[A] = as match {
    case RNil       ⇒ acc
    case Cons(h, t) ⇒ concatL(Snoc(acc, h), t)
  }

  @tailrec
  private def foldR[A, B](c: B, f: (A, B) ⇒ B, acc: B)(as: ListL[A]): B = as match {
    case LNil        ⇒ acc
    case Snoc(hs, t) ⇒ foldR(c, f, f(t, acc))(hs)
  }

  def listr[A, B](f: A ⇒ B): ListR[A] ⇒ ListR[B] =
    as ⇒  foldR[A, ListR[B]](nilR, {(a, bs) ⇒ cons(f(a), bs) }, nilR)(convert(as))

  @tailrec
  private def foldL[A, B](c: B, f: (B, A) ⇒ B, acc: B)(as: ListR[A]): B = as match {
    case RNil ⇒ acc
    case Cons(h, t) ⇒ foldL(c, f, f(acc, h))(t)
  }

  def listl[A, B](f: A ⇒ B): ListL[A] ⇒ ListL[B] =
    as ⇒ foldL[A, ListL[B]](nilL, { (b, a) ⇒ snoc(b, f(a))}, nilL)(convert(as))

  final def concat[A](as: ListL[A], aas: ListL[A]): ListL[A] = concatL(as, convert(aas))

  implicit class RichListR[A](as: ListR[A]) {
    def ⧺[AA <: A] (other: ListR[AA]): ListR[A] = concat(as, other)

    def fmap[B](f: A ⇒ B): ListR[B] = listr(f)(as)
  }

  implicit class RichListL[A](as: ListL[A]) {
    def ⧺[AA <: A] (other: ListL[AA]): ListL[A] = concat(as, other)

    def fmap[B](f: A ⇒ B): ListL[B] = listl(f)(as)
  }
}