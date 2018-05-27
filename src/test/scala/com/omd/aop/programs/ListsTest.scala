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

  private def rightList =  cons(2, cons(3, cons(4, nilR)))

  private def leftList =  snoc(snoc(snoc[Int](LNil, 2), 3), 4)
}
