package main

import scalaz.Functor
import scala.language.higherKinds

trait Tree[A] {
  sealed trait TreeF[F]
  case class Node[F](value: A, left: F, right: F) extends TreeF[F]
  case class Leaf[F](value: A) extends TreeF[F]
  case class Empty[F]() extends TreeF[F]

  implicit val btFunctor: Functor[TreeF] =
    new Functor[TreeF] {
      def map[B, C](fa: TreeF[B])(f: B => C): TreeF[C] = fa match {
        case Node(v, l, r) => Node(v, f(l), f(r))
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

  def run() = Cata[TreeF, Unit](print)(x)

  def printdflb() = Cata[TreeF, List[Int]](dflb)(x).map(println)
}

case class Fix[F[_]](unfix: F[Fix[F]])

object Cata {
  def apply[F[_]: Functor, A](alg: F[A] => A)(f: Fix[F]): A =
    alg(Functor[F].map(f.unfix)(apply(alg)))
}
