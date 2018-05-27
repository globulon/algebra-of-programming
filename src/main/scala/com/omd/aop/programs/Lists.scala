package com.omd.aop.programs

import scalaz.Isomorphism.<~>
import scalaz.Monoid

import scala.annotation.tailrec

trait Lists { self: Isomorphisms ⇒
  final def cons[A](h: A, r: ListR[A]): ListR[A] = Cons(h, r)
  final def nilR[A]: ListR[A] = RNil

  final def snoc[A](h: ListL[A], r: A): ListL[A] = Snoc(h, r)
  final def nilL[A]: ListL[A] = LNil

  final def convert[A](ll: ListL[A]): ListR[A] = implicitly[ListL <~> ListR].to(ll)
  final def convert[A](lr: ListR[A]): ListL[A] = implicitly[ListL <~> ListR].from(lr)

  @tailrec
  private def foldR[A, B](c: B, f: (A, B) ⇒ B, acc: B)(as: ListL[A]): B = as match {
    case LNil        ⇒ acc
    case Snoc(hs, t) ⇒ foldR(c, f, f(t, acc))(hs)
  }

  final def foldr[A, B](c: B, f: (A, B) ⇒ B): ListR[A] ⇒ B = as ⇒ foldR(c, f, c)(convert(as))

  final def listr[A, B](f: A ⇒ B): ListR[A] ⇒ ListR[B] = foldr[A, ListR[B]](nilR[B], {(a, bs) ⇒ cons(f(a), bs) })

  final def concat[A](as: ListR[A], aas: ListR[A]): ListR[A] = foldr[A, ListR[A]](as, cons)(aas)

  @tailrec
  private def foldL[A, B](c: B, f: (B, A) ⇒ B, acc: B)(as: ListR[A]): B = as match {
    case RNil ⇒ acc
    case Cons(h, t) ⇒ foldL(c, f, f(acc, h))(t)
  }

  final def foldl[A, B](c: B, h: (B, A) ⇒ B): ListL[A] ⇒ B = as ⇒ foldL(c, h, c)(convert(as))

  final def listl[A, B](f: A ⇒ B): ListL[A] ⇒ ListL[B] = foldl[A, ListL[B]](nilL[B], { (b, a) ⇒ snoc(b, f(a))})

  final def concat[A](as: ListL[A], aas: ListL[A]): ListL[A] = foldl[A, ListL[A]](as, snoc)(aas)

  implicit class RichListR[A](as: ListR[A]) {
    def ⧺[AA <: A] (other: ListR[AA]): ListR[A] = concat(as, other)

    def fmap[B](f: A ⇒ B): ListR[B] = listr(f)(as)

    def sum(implicit ev: Monoid[A]): A = foldr[A, A](ev.zero, ev.append(_, _))(as)

    def length: Int = fmap(_ ⇒ 1).sum
  }

  implicit class RichListL[A](as: ListL[A]) {
    def ⧺[AA <: A] (other: ListL[AA]): ListL[A] = concat(as, other)

    def fmap[B](f: A ⇒ B): ListL[B] = listl(f)(as)

    def sum(implicit ev: Monoid[A]): A = foldl[A, A](ev.zero, ev.append(_, _))(as)

    def length: Int = fmap(_ ⇒ 1).sum
  }
}