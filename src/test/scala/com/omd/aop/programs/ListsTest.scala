package com.omd.aop.programs

import org.scalatest.{MustMatchers, WordSpec}
import scalaz.Scalaz._

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

  "sum" must {
    "add elements of left list" in {
      rightList.sum must be (9)
    }

    "add elements of right list" in {
      leftList.sum must be (9)
    }
  }

  "lenght" must {
    "count elements of left list" in {
      rightList.length must be (3)
    }

    "count elements of right list" in {
      leftList.length must be (3)
    }
  }

  "filter" must {
    "rule in filtered elements" in {
      rightList.filter(_ % 2 == 0 ) must be (cons(2, cons(4, nilR)))
    }

    "inits" must {
      "generate initials in right list" in {
        rightList.inits must be  (Cons(Cons(2,RNil),Cons(Cons(2,Cons(3,RNil)),Cons(Cons(2,Cons(3,Cons(4,RNil))),RNil))))
      }

      "generate initials in left list" in {
        leftList.inits must be (snoc(Snoc(Snoc(LNil,Snoc(LNil,2)),Snoc(Snoc(LNil,2),3)),Snoc(Snoc(Snoc(LNil,2),3),4)))
      }

    }
  }

  private def rightList =  cons(2, cons(3, cons(4, nilR)))

  private def leftList =  snoc(snoc(snoc[Int](LNil, 2), 3), 4)
}
