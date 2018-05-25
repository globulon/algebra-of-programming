package com.omd.aop.programs

import scala.annotation.tailrec

protected[programs] trait Recursion {
  final def zero[N: Recursive]: N = implicitly[Recursive[N]].zero

  final def succ[N: Recursive](n: N): N = implicitly[Recursive[N]].succ(n)

  @tailrec
  private def foldnR[A, N](h: A ⇒ A, acc: A, next: N, goal: N, succ: N ⇒ N): A = next match {
    case n if n == goal ⇒ acc
    case n              ⇒ foldnR(h, h(acc), succ(n), goal, succ)
  }

  final def foldn[A, N : Recursive](c: A, h: A ⇒ A)(n: N):  A =
    foldnR(h, c, zero[N], n, succ[N])
}
