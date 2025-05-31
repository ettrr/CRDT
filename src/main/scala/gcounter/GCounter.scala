package gcounter

import cats.instances.list.*
import cats.instances.map.*
import cats.kernel.CommutativeMonoid
import cats.syntax.foldable.*
import cats.syntax.semigroup.*

trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
  def combine(a1: A, a2: A): A

  def empty: A
}

object BoundedSemiLattice {

  given BoundedSemiLattice[Int] with {
    override def combine(a1: Int, a2: Int): Int = math.max(a1, a2)

    override def empty: Int = 0
  }

  given [A]: BoundedSemiLattice[Set[A]] with {
    override def combine(a1: Set[A], a2: Set[A]): Set[A] = a1 union a2

    override def empty: Set[A] = Set.empty
  }
}

final case class GCounter[A](counters: Map[String, A]) {

  def increment(machine: String, amount: A)(implicit m: CommutativeMonoid[A]): GCounter[A] = {
    val value = amount |+| counters.getOrElse(machine, m.empty)
    GCounter(counters + (machine -> value))
  }

  def merge(that: GCounter[A])(implicit b: BoundedSemiLattice[A]): GCounter[A] = {
    GCounter(counters |+| that.counters)
  }

  def total(implicit m: CommutativeMonoid[A]): A = counters.values.toList.combineAll
}