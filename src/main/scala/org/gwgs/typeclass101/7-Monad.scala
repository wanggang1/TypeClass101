package org.gwgs.typeclass101

import scala.language.higherKinds

trait Category[=>:[_,_]] {
  def id[A] : A =>: A // identity arrow
  def compose[A,B,C](g: B =>: C, f: A =>: B) : A =>: C // composition arrow
}

object Category {
  
  val simpleCategory : Category[Function1] = new Category[Function1] {
    def id[A] : A => A = {a => a} // identity function
    def compose[A,B,C](g: B => C, f: A => B) : A => C = g.compose(f) // function composition
  }
  
}