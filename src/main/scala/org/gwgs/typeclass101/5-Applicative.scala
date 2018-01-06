package org.gwgs.typeclass101

import scala.language.higherKinds
import scala.concurrent.Future

/**
  * Higher kinded type
  */
trait Applicative[F[_]] extends Functor[F] {
  
  def pure[A](a: A): F[A]

  def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B]

  /**
    * Then Functor's identity and composition laws needs
    * to be satisfied by this method ???!!!
    */
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    ap(fa)(pure(f))
  
  /**
   * ff = map(f)(_.curried), maps F[(A,B) => C] to F[A => B => C]
   * ff2 = ap(fa)(ff), produces F[B => C]
   * ap(fb)(ff2) produces F[C]
   */
  def ap2[A,B,C](fa: => F[A],fb: => F[B])(f: F[(A,B) => C]) : F[C] =
    ap(fb)(ap(fa)(map(f)(_.curried)))
    
  def apply2[A,B,C](fa: => F[A],fb: => F[B])(f: (A,B) => C) : F[C] =
    ap2(fa,fb)(pure(f))
    
  def ap3[A,B,C,D](fa: => F[A],fb: => F[B],fc: => F[C])(f: F[(A,B,C) => D]) : F[D] =
    ap(fc)(ap(fb)(ap(fa)(map(f)(_.curried))))
    
  def apply3[A,B,C,D](fa: => F[A],fb: => F[B],fc: => F[C])(f: (A,B,C) => D) : F[D] =
    ap3(fa,fb,fc)(pure(f))

}

object Applicative {
  
  def apply[FT[_]: Applicative] = implicitly[Applicative[FT]]
  
  implicit val optionApplicativeInstance : Applicative[Option] = new Applicative[Option] {
    def pure[A](a: A): Option[A] = Some(a)
  
    def ap[A, B](fa: => Option[A])(f: => Option[A => B]): Option[B] = {
      (fa, f) match {
        case (Some(fa1), Some(f1)) => Some(f1(fa1))
        case _ => None
      }
    }
  }
  
  implicit val listApplicativeInstance : Applicative[List] = new Applicative[List] {
    override def pure[A](a: A): List[A] = List(a)
    override def ap[A, B](fa: => List[A])(f: => List[A => B]): List[B] = {
      for {
        fa1 <- fa
        f1 <- f
      } yield f1(fa1)
    }
  }
  
  implicit val futureApplicativeInstance : Applicative[Future] = new Applicative[Future] {
    import scala.concurrent.ExecutionContext.Implicits.global
    
    def pure[A](a: A): Future[A] = Future{a}
  
    def ap[A, B](fa: => Future[A])(ff: => Future[A => B]): Future[B] =
      for {
        a <- fa
        f <- ff
      } yield f(a)
  }
  
  
  /**
   * The difference between Applicative and for-comprehension:
   * for-comprehension retrieves values sequentially vs. Applicative gets values out of context independently
   */
  def demo = {
    println("============Applicative================")

    // see ApplicativeBuilder for eliminating Applicative[Option] prefix

    val x = Applicative[Option].apply2(Some(1), Some(2))((x: Int, y: Int) => x + y)
    println("Using Applicative: " + x)
    
    val y = Applicative[Option].apply2(Some(1), Some(2))(AB)
    println("Using Applicative with case class apply method: " + y) //See ApplicativeBuilder
    
    val xs = Applicative[List].ap(List(1,2,3))(List(add2, add3, add4))
    println("Using Applicative for List: " + xs)
    
    println("")
  }
  
  case class AB(a: Int, b: Int)
  
  def add(n: Int)(x: Int) = x + n
  
  val add2 = add(2) _
  val add3 = add(3) _
  val add4 = add(4) _
  
}