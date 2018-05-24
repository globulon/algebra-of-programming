package com.omd.aop.programs

trait Recursive[N] {
  def zero: N
  def succ: N ⇒ N
}