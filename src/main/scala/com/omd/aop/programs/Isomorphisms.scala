package com.omd.aop.programs

import scalaz.Isomorphism._

trait Isomorphisms { self: Recursion with Recursions =>
  implicit final def naturalInt: Nat <=> Int = IsoSet[Nat, Int] (
    foldn[Int, Nat](zero[Int], succ[Int]),
    foldn[Nat, Int](zero[Nat], succ[Nat])
  )
}
