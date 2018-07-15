package com.omd.aop.programs

import scalaz.Functor

trait Expressions {self: Algebras ⇒

  protected sealed trait ExprF[+A]
  protected case class Const(n: Int) extends ExprF[Nothing]
  protected case class Add[+A](a: A, b: A) extends ExprF[A]
  protected case class Mult[+A](a: A, b: A) extends ExprF[A]

  final protected def const(n: Int): ExprF[Nothing] = Const(n)

  implicit final protected def epxrFunc: Functor[ExprF] = new Functor[ExprF] {
    override def map[A, B](fa: ExprF[A])(f: A ⇒ B): ExprF[B] = fa match {
      case Const(n)   ⇒ Const(n)
      case Add(a, b)  ⇒ Add(f(a), f(b))
      case Mult(a, b) ⇒ Mult(f(a), f(b))
    }
  }

  final protected def alg: Algebra[ExprF, Int] = {
    case Const(i)             ⇒ i
    case Add(a: Int, b: Int)  ⇒ a + b
    case Mult(a: Int, b: Int) ⇒ a * b
  }

  final protected def eval: Fix[ExprF] ⇒ Int = cata(alg)
}