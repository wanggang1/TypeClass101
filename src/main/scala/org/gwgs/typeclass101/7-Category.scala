package org.gwgs.typeclass101

import scala.language.higherKinds

/**
 * Category:
 * some objects of any type,  connect these objects using arrows.  So if we have an arrow
 * from f: A -> B and an arrow from g: B -> C, category theory implies that we also have
 * h: A -> C.
 */
trait Category[=>:[_,_]] {
  def id[A] : A =>: A // identity arrow
  def compose[A,B,C](g: B =>: C, f: A =>: B) : A =>: C // composition arrow
}

object Category {
  
  /**
   * A simple category for Scala Types and functions - Scala types as objects and functions
   * as arrows.
   *  
   * This was as simple as useless, Scala already has composition for functions.  But It can't
   * compose g: String => Option[String] and f: Int => Option[String]
   */
  val simpleCategory : Category[Function1] = new Category[Function1] {
    def id[A] : A => A = {a => a} // identity function
    def compose[A,B,C](g: B => C, f: A => B) : A => C = g.compose(f) // function composition
  }
  
  /**
   * This solve the Option return type composition
   */
  val fancyCategory: Category[Fancy] = new Category[Fancy] {
    def id[A] : Fancy[A,A] = Fancy(a => Some(a))
    def compose[A,B,C](g: Fancy[B,C], f: Fancy[A,B]) : Fancy[A,C] =  Fancy[A,C]{ a =>
      /*
      f.run(a) match {
        case Some(b) => g.run(b)
        case None => None
      }
      */
      f.run(a) flatMap g.run
    }
  }
  
  def positive(i: Int): Option[String] = {
    val f : Int => Option[Int] = a => if (a > 0) Some(a) else None
    val g: Int => Option[String] = a => Some(a.toString)
    val h = fancyCategory.compose(Fancy(g),Fancy(f))
    h.run(i)
  }
  
}

case class Fancy[A,B](run : A => Option[B])