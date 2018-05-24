package com.omd.aop.programs

import scala.annotation.tailrec

trait Recursion {
  final def zero[N: Recursive]: N = implicitly[Recursive[N]].zero

  final def succ[N: Recursive]: N => N = implicitly[Recursive[N]].succ

  @tailrec
  private def foldnR[A, N](h: N ⇒ A, acc: A, next: N, goal: N, succ: N ⇒ N): A = next match {
    case n if n == goal ⇒ acc
    case n              ⇒ foldnR(h, h(n), succ(n), goal, succ)
  }

  final def foldn[A, N : Recursive](c: A, h: N ⇒ A): N ⇒ A =
    foldnR(h, c, zero[N], _, succ[N])
}
