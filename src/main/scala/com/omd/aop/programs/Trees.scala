package com.omd.aop.programs

import java.lang.Math.max

trait Trees {
  final def tip[A]: A ⇒ Tree[A] = Tip(_)
  final def bin[A](l: Tree[A], r: Tree[A]): Tree[A] = Bin(l, r)

  final def foldt[A, B](f: A ⇒ B, g: (B, B) ⇒ B): Tree[A] ⇒ B = {
    case Tip(a) ⇒ f(a)
    case Bin(l, r) ⇒ g(foldt(f, g)(l), foldt(f, g)(r))
  }

  final def tree[A, B](f: A ⇒ B): Tree[A] ⇒ Tree[B] = foldt( tip[B] ∘ f, bin[B] )

  final def size[A, N: Recursive](t: Tree[A]): N = foldt[A, N](_ ⇒ one[N], plus[N](_)(_))(t)

  final def depth[A]: Tree[A] ⇒ Int = foldt[A, Int](_ ⇒ one[Int], succ[Int] _ ∘ max )
}
