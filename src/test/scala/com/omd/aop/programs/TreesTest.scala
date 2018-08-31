package com.omd.aop.programs

import com.omd.aop.programs.{size ⇒ sz}
import org.scalatest.{MustMatchers, WordSpec}

final class TreesTest extends WordSpec with MustMatchers {
  "tree" must {
    "map structure" in {
      tree[Int, Int](_ * 2)(t) must be(bin(tip(8), bin(tip(6), bin(tip(8), tip(10)))))
    }
  }

  "depth" must {
    "process one leaf" in {
      depth(tip("a")) must be(1)
    }
    "process one tree" in {
      depth(t) must be(4)
    }
  }

  "size" must {
    "be one" in {
      sz[Int, Int](tip[Int](4)) must be(1)
    }
    "be correct" in {
      sz[Int, Int](t) must be(4)
    }
  }

  private def t = bin(tip(4), bin(tip(3), bin(tip(4), tip(5))))
}