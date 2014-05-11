package fpinscala
package chapter3

import org.scalacheck.{Arbitrary, Gen}

object TreeHelpers {
  import scala.collection.immutable.{List, Nil}

  implicit class ToScalaList[A](val xs: Tree[A]) extends AnyVal {
    def toScalaList: List[A] = {
      def convert(ys: Tree[A], acc: List[A]): List[A] =
        ys match {
          case Leaf(v) => v :: acc
          case Branch(left, right) => convert(left, acc) ::: convert(right, acc)
        }

      convert(xs, Nil)
    }
  }
}

class TreeSpec extends UnitSpec {

  import Tree._, TreeHelpers._

  /*implicit val arbitraryIntTree = Arbitrary {
    val treeGen1 = for {
      n <- Gen.choose(0, 10)
      leaf <- Gen.oneOf(None, Some(Leaf(n)))
    } yield leaf

    val treeGen2 = for {
      n <- Gen.choose(0, 10)
      leaf <- Gen.oneOf(None, Some(Leaf(n)))
    } yield leaf

    treeGen1.zip(treeGen2).map((a, b) => Branch(a, b))
  }*/

  val xs =
    Branch(
      Branch(
        Leaf(5),
        Leaf(7)),
      Branch(
        Branch(
          Leaf(1),
          Branch(
            Leaf(2),
            Leaf(6))),
        Leaf(3)))

  it should "return the size" in {
    /*forAll { (xs: Tree[Int]) =>*/
    Tree.size(xs) shouldBe xs.toScalaList.size
    /*}*/
  }

  it should "find the maximum value" in {
    Tree.maximum(xs) shouldBe 7
  }

  it should "find the depth" in {
    Tree.depth(xs) shouldBe 4
  }
}
