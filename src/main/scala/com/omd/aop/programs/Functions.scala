package com.omd.aop.programs

trait Functions {
  implicit class RichFuntion[A, B](f: A ⇒ B) {
    def ∘[AA](g: AA ⇒ A): AA ⇒ B = f compose g

    def ∘[T, S](g: (T, S) ⇒ A): (T, S) ⇒ B = (t, s) ⇒ f(g(t, s))
  }
}
