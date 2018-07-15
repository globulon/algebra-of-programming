package com.omd.aop.programs

import scalaz.Functor


trait Algebras {
  case class Fix[F[_]](f: F[Fix[F]])

  def fix[F[_]](f: F[Fix[F]]): Fix[F] = Fix(f)

  def unfix[F[_]](f: Fix[F]): F[Fix[F]] = f.f

  def cata[A, F[_] : Functor](alg: Algebra[F, A]): Fix[F] ⇒ A =
    fixF ⇒ alg(implicitly[Functor[F]].map(unfix(fixF))(cata[A, F](alg)))
}