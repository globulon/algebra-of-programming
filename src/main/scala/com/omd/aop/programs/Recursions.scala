package com.omd.aop.programs

protected[programs] trait Recursions {self: Recursion ⇒
  final def one[N: Recursive]: N = succ[N](zero[N])

  final def plus[N : Recursive](n: N)(m: N):  N = foldn(n, succ[N])(m)

  final def mul[N : Recursive](n: N)(m : N):  N = foldn(zero[N] , plus[N](n))(m)

  final def exp[N : Recursive](n: N)(m : N): N = foldn(succ[N](zero[N]), mul[N](n))(m)
}