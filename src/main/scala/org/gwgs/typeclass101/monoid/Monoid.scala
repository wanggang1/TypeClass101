package org.gwgs.typeclass101.monoid

import org.gwgs.typeclass101.semigroup.Semigroup
import scala.language.implicitConversions
/*
 * Type class monoid must obay law of identity
 *    empty append a == a
 *    a append empty == a
 */
trait Monoid[A] extends Semigroup[A] {
  def empty : A  //identity element
}

object Monoid {
  def apply[A](implicit F : Monoid[A]) : Monoid[A] = F
  
  implicit val intSemigroup = new Semigroup[Int] {
    def append(a: Int, b: Int): Int = a + b
  }
  
  implicit def intInstance(implicit F: Semigroup[Int]) = new Monoid[Int] {
    def empty : Int = 0
    def append(a : Int, b: Int) : Int = F.append(a, b)
  }
  
  /*
   * implicit Semigroup NOT necessaryly needed
   */
  implicit val boolMonoid = new Monoid[Boolean] {
    def empty = true
    def append(a: Boolean, b: Boolean) : Boolean = a && b
  }
  
  /*
   * Option sample
   */
  implicit def optionInstance[A](implicit A: Semigroup[A]) = new Monoid[Option[A]] {
    def empty : Option[A] = None
    def append(a : Option[A], b: Option[A]) : Option[A] = (a, b) match {
      case (Some(a1), Some(b1)) => Some(Semigroup[A].append(a1, b1))
      case (Some(_), None) => a
      case (None, Some(_)) => b
      case _ => empty
    }
  }
  
  implicit class SumOps[A : Monoid](list : List[A]) {
    val F = implicitly[Monoid[A]]
    def sumList() : A = list.foldLeft(F.empty)(F.append(_,_))
  }
  
  def demo = {
    val b = List(true, false, true, true).sumList()
    println("Boolean sumList: " + b)
  
    val i = List(1, 2, 3, 4, 5).sumList()
    println("Int sumList: " + i)
    
    /*
     * the parameter implicit A: Semigroup[Boolean] in optionInstance() 
     * is satisfied by implicit val boolMonoid because Monoid extends
     * Semigroup
     */
    val bl = List(Some(true), Some(false), None, Some(true)).sumList()
    println("Option[Boolean] sumList: " + bl)
    
    val il = List(Some(1), Some(2), None, Some(3)).sumList()
    println("Option[Int] sumList: " + il)
  }
  
}