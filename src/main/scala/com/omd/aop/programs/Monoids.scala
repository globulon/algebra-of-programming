package com.omd.aop.programs

import scalaz.Monoid

trait Monoids { self: Recursion with Recursions ⇒
  implicit def recMonoid[A : Recursive]: Monoid[A] = new Monoid[A] {
    override def zero: A = implicitly[Recursive[A]].zero

    override def append(f1: A, f2: ⇒ A): A = plus[A](f1)(f2)
  }
}