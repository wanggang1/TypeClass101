package org.gwgs.typeclass101

import scala.language.higherKinds

/**
 * Law of Foldable:
 * the outcome of a foldLeft equals the foldMap, and a foldRight equals the foldMap as well
 */
trait Foldable[F[_]] {
  
  def foldMap[A, B](fa: F[A])(f: A => B)(implicit F: Monoid[B]): B
  def fold[M: Monoid](t: F[M]): M = foldMap[M, M](t)(x => x)
  def foldRight[A,B](fa: F[A], z: => B)(f: (A,B) => B) : B
 
}