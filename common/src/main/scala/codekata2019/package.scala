package codekata2019

import zio.{UIO, ZIO}
import zio.console.Console

package object common {

  implicit class _AnyOps[A](val in: A) extends AnyVal {
    /** Lift a value into an [[Option]], but with better type inference than `Some(in)` */
    def some: Option[A] = Option(in)

    /** Pipe operator.  Pipe a value `v` into function `f` with `v |> f`. */
    def |>[B](fn: A => B): B = fn(in)
    def zio: UIO[A] = UIO.succeed(in)
  }

  implicit class _ZIOOps[R, E, A](val in: ZIO[R, E, A]) extends AnyVal {
    /** Apply an aspect to an effectful program */
    def @@[R1 <: R, E1 >: E](aspect: ZAspect[R1, E1]): ZIO[R1, E1, A] = aspect(in)

    def shush(silent: Boolean): ZIO[R with Console, E, A] = if (silent) (in @@ NoConsoleOutput) else in
  }
}
