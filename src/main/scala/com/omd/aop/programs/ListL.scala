package com.omd.aop.programs

sealed trait ListL[+A]
case class Snoc[+A](h: ListL[A], t: A) extends ListL[A]
case object LNil extends ListL[Nothing]