package com.omd.aop.programs

trait RecursiveStructs {
  final implicit def natural: Recursive[Int] = new Recursive[Int] {
    final def zero: Int = 0

    final def succ: Int ⇒ Int =  _ + 1
  }

  final implicit def nat: Recursive[Nat] = new Recursive[Nat] {
    override def zero: Nat = Zero

    override def succ: Nat ⇒ Nat = Succ
  }
}