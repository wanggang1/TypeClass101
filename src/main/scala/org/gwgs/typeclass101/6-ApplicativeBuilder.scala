package org.gwgs.typeclass101

import scala.language.higherKinds

trait NaturalTransformation[-F[_], +G[_]] {
  def apply[A](F: F[A]): G[A]
}

/**
 * Higher order type transformation for Applicative
 */
abstract class NaturalTransformation2[-F[_[_]], +G[_[_]]] {
  def apply[M[_]](F: F[M]): G[M]
}


/**
 * Using play-json's implementation in our ApplicativeBuilder
 * 
 * THe goal is to have the syntax: (aOpt |@| bOpt |@| cOpt){ABC}
 */
object ApplicativeBuilder {
  
  import play.api.libs.functional.{ Applicative => PlayApplicative, Monoid => PlayMonoid, _ }
  
  type ~>[F[_], G[_]] = NaturalTransformation[F, G]
  
  type ~~>[F[_[_]], G[_[_]]] = NaturalTransformation2[F, G]
  
  // transform any play monoid to a simplez monoid
  object PlayMonoid2Monoid extends (PlayMonoid ~> Monoid) {
    def apply[A](playM: PlayMonoid[A]): Monoid[A] = new Monoid[A] {
      def empty: A = playM.identity
      def append(a: A, b: A): A = playM.append(a, b)
    }
  }

  // transform any concrete play monoid instance into the concrete simplez monoid instance
  // using the natural transformation ~>
  implicit def playMonoid2Monoid[A](implicit P: PlayMonoid[A], ev: PlayMonoid ~> Monoid): Monoid[A] = ev(P)
  
  // transform any play applicative into a simplez applicative
  object PlayApplicative2Applicative extends (PlayApplicative ~~> Applicative) {
    def apply[M[_]](F: PlayApplicative[M]): Applicative[M] = new Applicative[M] {
      def pure[A](a: A) = F.pure(a)
      def ap[A, B](fa: => M[A])(f: => M[A => B]): M[B] = F.apply(f, fa)
    }
  }

  // transform any concrete play applicative instance into a simplez applicative instance
  // using a higher kinded "natural transformat" ~~>
  implicit def playApplicative2Applicative[F[_]](implicit P: PlayApplicative[F],
    ev: PlayApplicative ~~> Applicative): Applicative[F] = ev(P)
    
  /*  
   * See ApplicativeBuilder, https://inoio.de/blog/2014/11/10/type-class-101-applicativebuilder/
   * 
   * scalaz, http://eed3si9n.com/learning-scalaz/Applicative+Builder.html
   * instead of Applicative[Option].apply2, scalaz ApplicativeBuilder has combinator, |@|
   * implicit val abcReads : Reads[ABC]= (
   * (__ \ "a").read[Int] |@|
   * (__ \ "b").read[Int] |@|
   * (__ \ "c").read[Int]) { ABC.apply _ }
   * 
   * play Json also uses ApplicativeBuilder pattern
   * ((__ \ 'code).read[String] and
   *  (__ \ 'name).read[String] and
   *  (__ \ 'timeZone).read[String])(ABC.apply _)
   */
  
}