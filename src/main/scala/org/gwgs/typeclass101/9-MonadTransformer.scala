package org.gwgs.typeclass101

import scala.language.higherKinds
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

case class OptionT[F[_], A](run : Future[Option[A]]) {
  def getOrElse(o: A) = run.map(option => option.getOrElse(o))
  def map[B](f: A => B) : OptionT[F, B] = OptionT(run.map(option => option map f))
}
