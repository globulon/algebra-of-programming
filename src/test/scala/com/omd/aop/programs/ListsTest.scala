package com.omd.aop.programs

import org.scalatest.{MustMatchers, WordSpec}

final class ListsTest extends WordSpec with MustMatchers {
  "convert" must {
    "convert left list to right cumulated list" in {
      convert(leftList) must be (rightList)
    }

    "convert right list to left cumulated list" in {
      convert(rightList) must be (leftList)
    }
  }

  "concat" must {
    "append right list" in {
      rightList ⧺ rightList must be (cons(2, cons(3, cons(4, cons(2, cons(3, cons(4, nilR)))))))
    }

    "append left list" in {
      leftList ⧺ leftList must be (snoc(snoc(snoc[Int](snoc(snoc(snoc[Int](LNil, 2), 3), 4), 2), 3), 4))
    }
  }

  "fmap" must {
    "map function to right list" in {
      rightList.fmap(_ * 2) must be (cons(4, cons(6, cons(8, nilR))))
    }

    "map function to left list" in {
      leftList.fmap(_ * 2) must be(snoc(snoc(snoc[Int](LNil, 4), 6), 8))
    }
  }

  private def rightList =  cons(2, cons(3, cons(4, nilR)))

  private def leftList =  snoc(snoc(snoc[Int](LNil, 2), 3), 4)
}
