package org.gwgs.typeclass101

import scala.language.higherKinds

/**
 * Category theory is a very general mathematical theory.  It's dealing with Abstraction and Composition
 * 
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
   * composition with arrow pointing to Option type
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
  
  /**
   * composition with arrow pointing to List type
   * 
   * This is almost identical implementation as Option.  Abstruct to Monad!!!
   */
  val fancy2Category : Category[Fancy2] = new Category[Fancy2] {
    def id[A] = Fancy2 { a => List(a)}
    def compose[A,B,C](g: Fancy2[B,C], f: Fancy2[A,B]) : Fancy2[A,C] = Fancy2[A,C] { a => f.run(a) flatMap g.run }
  }
  
  ///////////////////////// Demo ////////////////////////////////////
  def positive(i: Int): Option[String] = {
    val f : Int => Option[Int] = a => if (a > 0) Some(a) else None
    val g: Int => Option[String] = a => Some(a.toString)
    val h = fancyCategory.compose(Fancy(g),Fancy(f))
    h.run(i)
  }
  
  def demo = {
    println("============Category==================")
    
    println("Use Category: " + positive(5))
    println("Use Category: " + positive(-5))
    
    println("")
  }
  
}

case class Fancy[A,B](run : A => Option[B])
case class Fancy2[A,B](run: A => List[B])