package org.gwgs.typeclass101

import scala.language.higherKinds

/**
 * Monad is just another typeclass, it enables composing fancy functions because it has a flatMap
 */
trait Monad[F[_]] { // F = Fancy type, like Option or List
  def pure[A](a: A) : F[A] // put something into a fancy
  def flatMap[A,B](fa: F[A])(f: A => F[B]) : F[B] // compose
}

object Monad {
  
  implicit val optionMonad : Monad[Option] = new Monad[Option] {
    def pure[A](a: A) = Some(a)
    def flatMap[A,B](fa: Option[A])(f: A => Option[B]) : Option[B] = fa.flatMap(f)
  }
  
  implicit val listMonad : Monad[List] = new Monad[List] {
    def pure[A](a: A) = List(a)
    def flatMap[A,B](fa: List[A])(f: A => List[B]) : List[B] = fa.flatMap(f)
  }
  
  implicit def monadCategory[M[_]](implicit M : Monad[M]) : Category[({type l[A,B] = Fancy3[M,A,B]})#l] =
    new Category[({type l[A,B] = Fancy3[M,A,B]})#l] {
      def id[A] = Fancy3[M,A,A] { a => M.pure(a)}
      def compose[A,B,C](g: Fancy3[M,B,C], f: Fancy3[M,A,B]) : Fancy3[M,A,C] = Fancy3 { a => M.flatMap(f.run(a))(g.run)}
    }
}

case class Fancy3[M[_], A, B](run : A => M[B])