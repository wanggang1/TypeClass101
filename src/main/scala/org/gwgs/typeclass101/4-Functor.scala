package org.gwgs.typeclass101

import scala.language.higherKinds

/**
 * Higher kinded type
 *
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
  
  //def apply[FT[_]](implicit f : Functor[FT]) : Functor[FT] = f
  def apply[FT[_]: Functor] = implicitly[Functor[FT]]
  
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
    
    val fx2 = Functor[Option].map(Some(3))((x: Int) => (y: Int) => x + y )
    println("Using apply method (return function because of curried f): " + fx2)
    val fx21 = Functor[Option].map(fx2)((x: Int => Int) => x(2) )
    println(".....continue mapping: " + fx21)
    
    //the work around....
    val addTuple2Fun = (add2 _).tupled
    val fx3 = Functor[Option].map(Some((3,2)))(addTuple2Fun)  //See Applicative
    println("work around... (tupled): " + fx3)
    
    println("")
  }
  
  private def add2(x: Int, y: Int) = x + y

}