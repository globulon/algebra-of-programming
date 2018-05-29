package com.omd.aop.programs

sealed trait Tree[+A]
case class Tip[+A](a: A) extends Tree[A]
case class Bin[+A](left: Tree[A], right: Tree[A]) extends Tree[A]
