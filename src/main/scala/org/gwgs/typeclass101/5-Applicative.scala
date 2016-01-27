package org.gwgs.typeclass101

import scala.language.higherKinds

trait Applicative[F[_]] extends Functor[F] {
  
  def pure[A](a: A): F[A]

  def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B]
  
  def ap2[A,B,C](fa: => F[A],fb: => F[B])(f: F[(A,B) => C]) : F[C] =
    (ap(fb)(ap(fa)(map(f)(_.curried))))
    
  def apply2[A,B,C](fa: => F[A],fb: => F[B])(f: (A,B) => C) : F[C] =
    ap2(fa,fb)(pure(f))
  
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    ap(fa)(pure(f))
}

object Applicative {
  
  val optionApplicativeInstance : Applicative[Option] = new Applicative[Option] {
    def pure[A](a: A): Option[A] = Some(a)
  
    def ap[A, B](fa: => Option[A])(f: => Option[(A) => B]): Option[B] = {
      (fa, f) match {
        case (Some(fa1), Some(f1)) => Some(f1(fa1))
        case _ => None
      }
    }
  }
  
  val listApplicativeInstance : Applicative[List] = new Applicative[List] {
    override def pure[A](a: A): List[A] = List(a)
    override def ap[A, B](fa: => List[A])(f: => List[(A) => B]): List[B] = {
      for {
        fa1 <- fa
        f1 <- f
      } yield (f1(fa1))
    }
  }
  
  def demo = {
    println("============Applicative================")
    
    val x = optionApplicativeInstance.apply2(Some(1), Some(2))((x: Int, y: Int) => x + y)
    println("Using Applicative: " + x)
    
    val y = optionApplicativeInstance.apply2(Some(1), Some(2))(AB)
    println("Using Applicative with case class apply method: " + y) //See ApplicativeBuilder
    
    println("")
  }
  
  case class AB(a: Int, b: Int)
  
}