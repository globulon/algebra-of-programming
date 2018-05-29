package com.omd.aop.programs

protected[programs] trait Fixture {
  final protected def two[N: Recursive]: N = succ[N](one[N])
  final protected def three[N: Recursive]: N = succ[N](two[N])
  final protected def four[N: Recursive]: N = succ[N](three[N])
  final protected def five[N: Recursive]: N = succ[N](four[N])
  final protected def six[N: Recursive]: N = succ[N](five[N])
  final protected def seven[N: Recursive]: N = succ[N](six[N])
  final protected def eight[N: Recursive]: N = succ[N](seven[N])
}
