package org.gwgs.typeclass101

import scala.language.implicitConversions

/*
 * Type class semigroup must obay law of associativity
 *    append(a1, append(a2, a3)) == append(append(a1, a2), a3)
 */
trait Semigroup[A] {
  def append(a: A, b: A): A
}

trait SemigroupSyntax[A] {
  def self: A
  def F: Semigroup[A]
  def |+|(b: A): A = append(b) // alias for append
  def append(b: A): A = F.append(self, b)
}

object Semigroup {
  def apply[A](implicit F : Semigroup[A]) = F

  implicit val intSemigroup = new Semigroup[Int] {
    def append(a: Int, b: Int): Int = a + b
  }
  
  implicit val booleanSemigroup = new Semigroup[Boolean] {
    def append(a: Boolean, b: Boolean): Boolean = a && b
  }

  implicit def optionInstances[A: Semigroup] = new Semigroup[Option[A]] {
    def append(a: Option[A], b: Option[A]): Option[A] = {
      (a, b) match {
        case (Some(a1), Some(b1)) => Some(Semigroup[A].append(a1, b1))
        case (Some(_), None) => a
        case (None, Some(_)) => b
        case _ => None
      }
    }
  }
  
  implicit def ToSemigroupOps[A: Semigroup](a: A): SemigroupSyntax[A] =
    new SemigroupSyntax[A] {
      def self: A = a
      def F: Semigroup[A] = implicitly[Semigroup[A]]
    }

  //Can the above method also take care of Option[A]????
  implicit def ToSemigroupOpsForOption[A: Semigroup](a: Option[A]): SemigroupSyntax[Option[A]] =
    new SemigroupSyntax[Option[A]] {
      def self: Option[A] = a
      def F: Semigroup[Option[A]] = implicitly[Semigroup[Option[A]]]
    }
  
  def demo = {
    println("============Semigroup================")
    
    val x = Semigroup[Int].append(1,2)
    println("Using apply method: " + x)
    
    val x1 = Semigroup[Option[Int]].append(Some(1), Some(2))
    println("Using apply method for Option[A]: " + x1)
    
    val x2 = Semigroup[Option[Boolean]].append(Some(true), Some(false))
    println("Using apply method for Option[A]: " + x2)
    
    val y = 3 |+| 4
    println("Using implicit conversion for Int: " + y)
    
    val y2 = Some(3) |+| Some(4)
    println("Using implicit conversion for Option[Int]: " + y2)
    
    val bool = true |+| false
    println("Using implicit conversion for Boolean: " + bool)
    
    val bool2 = Some(true) |+| Some(false)
    println("Using implicit conversion for Option[Boolean]: " + bool2)
    
    println("")
  }
}
