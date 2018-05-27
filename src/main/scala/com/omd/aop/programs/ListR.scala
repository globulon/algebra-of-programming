package com.omd.aop.programs

sealed trait ListR[+A]
case class Cons[+A](h: A, t: ListR[A]) extends ListR[A]
case object RNil extends ListR[Nothing]