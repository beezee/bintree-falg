package main

import scalaz.Functor
import scala.language.higherKinds

trait Tree[A] {
  sealed trait TreeF[F]
  case class Node[F](value: A, left: F, right: F) extends TreeF[F]
  case class Leaf[F](value: A) extends TreeF[F]
  case class Empty[F]() extends TreeF[F]

  val r2lNodeFunctor: Functor[Node] = new Functor[Node] {
    def map[B, C](n: Node[B])(f: B => C): Node[C] = {
      val r = f(n.right)
      val l = f(n.left)
      Node(n.value, l, r)
    }
  }
  val l2rNodeFunctor: Functor[Node] = new Functor[Node] {
    def map[B, C](n: Node[B])(f: B => C): Node[C] =
      Node(n.value, f(n.left), f(n.right))
  }

  implicit def btFunctor(implicit NF: Functor[Node]): Functor[TreeF] =
    new Functor[TreeF] {
      def map[B, C](fa: TreeF[B])(f: B => C): TreeF[C] = fa match {
        case (n: Node[B]) => NF.map(n)(f)
        case Leaf(v) => Leaf(v)
        case Empty() => Empty()
      }
    }
}

object IntTree extends Tree[Int] {

  val print = (_: TreeF[Unit]) match {
    case Node(v, _, _) => println(v)
    case Leaf(v) => println(v)
    case Empty() => ()
  }

  val dflb = (_: TreeF[List[Int]]) match {
    case Node(v, ls, rs) => (ls :+ v) ++ rs
    case Leaf(v) => List(v)
    case Empty() => List()
  }

  val x: Fix[TreeF] =
    Fix(Node(42,
      Fix(Node(28,
        Fix(Leaf(12)),
        Fix(Node(32,
          Fix(Leaf(29)),
          Fix(Leaf(35)))))),
      Fix(Leaf(83))))

  def printr2l() = {
    implicit val nf = r2lNodeFunctor
    Cata[TreeF, Unit](print)(x)
  }

  def printl2r() = {
    implicit val nf = l2rNodeFunctor
    Cata[TreeF, Unit](print)(x)
  }

  def printdflb() = {
    implicit val nf = l2rNodeFunctor
    Cata[TreeF, List[Int]](dflb)(x).foreach(println)
  }
}

case class Fix[F[_]](unfix: F[Fix[F]])

object Cata {
  def apply[F[_]: Functor, A](alg: F[A] => A)(f: Fix[F]): A =
    alg(Functor[F].map(f.unfix)(apply(alg)))
}
