package org.gwgs.typeclass101

import scala.language.higherKinds

/**
 * Laws of Functor
 * Identity: F[A].map(x => x) == F[A]
 * Composition:
 * 	Given f: A => B and g: B => C,
 * 	then F[A].map(g compose f) == F[B].map(g) compose F[A].map(f)
 */
trait Functor[F[_]] {
  def map[A, B](F: F[A])(f: A => B): F[B]
}

object Functor {
  
  def apply[A[_]](implicit f : Functor[A]) : Functor[A] = f
  
  implicit val optionFunctorInstance = new Functor[Option] {
    def map[A,B](F: Option[A])(f: A => B) : Option[B] = {
      F match {
        case Some(a) => Option(f(a))
        case None => None
      }
    }
  }
  
  def demo = {
    println("============Functor====================")
    
    val fx = Functor[Option].map(Some(3))((x: Int) => x + 2)
    println("Using apply method: " + fx)
    
    println("")
  }

}