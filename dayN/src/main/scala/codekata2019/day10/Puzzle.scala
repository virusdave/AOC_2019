package codekata2019.day10

import codekata2019.common._
import enumeratum._
import scala.annotation.tailrec
import shapeless.tag
import shapeless.tag.@@
import zio._
import zio.console._

object Puzzle {
  trait _Location
  trait _Delta
  type Loc = (Int, Int) @@ _Location
  type Delta = (Int, Int) @@ _Delta
  implicit class _LocOps(from: Loc) {
    def +(towards: Delta): (Int, Int) @@ _Location = tag[_Location]((from._1 + towards._1, from._2 + towards._2))
    /** Return the minimal integral delta that points in the direction of `to` */
    def directionTowards(to: Loc): (Int, Int) @@ _Delta = {
      @tailrec def gcd(a: Int,b: Int): Int = if(b == 0) a else gcd(b, a%b)

      val mgcd = Math.abs(gcd(from._1 - to._1, from._2 - to._2))
      tag[_Delta]((to._1 - from._1) / mgcd, (to._2 - from._2) / mgcd)
    }
  }

  private val matIn: IndexedSeq[IndexedSeq[Int]] = in.linesIterator.toIndexedSeq.map(_.map(_.toString.count(_ == '#')))

  private val (xRange, yRange) = (matIn.head.size, matIn.size)

  private val asteroids = for {
    y <- matIn.indices
    x <- matIn(y).indices if matIn(y)(x) > 0
  } yield tag[_Location](x -> y)
  private val asteroidsSet = asteroids.toSet

  // Find the first asteroid visible from position `from`, looking in direction `towards`
  private def fromTowards(from: Loc, towards: Delta, asteroidsSet: Set[Loc] = asteroidsSet): Option[Loc] = {
    @tailrec def go(from: Loc, towards: Delta): Option[Loc] = {
      if (from._1 <0 || from._1 >= xRange || from._2 < 0 || from._2 >= yRange) None
      else if (asteroidsSet(from)) from.some
      else go(from + towards, towards)
    }
    // Obviously, don't count the one we're standing on.
    go(from + towards, towards)
  }

  object Part1 {
    def solution = {
      asteroids.map { a =>
        a -> (asteroidsSet - a).toSeq.map { b =>
          if (fromTowards(a, a.directionTowards(b)).contains(b)) 1 else 0
        }.sum
      }.maxBy(_._2)
    }.zio
  }

  object Part2 {
    val base = tag[_Location](11 -> 13)
    def solution = {
      val dirs = LazyList.continually((asteroidsSet - base)
        .map(base.directionTowards)
        .toSeq.map { dir =>
          {
            val theta = Math.atan2(dir._1, -dir._2)
            if (theta < 0) theta + 2 * Math.PI else theta
          } -> dir
        }.sortBy(_._1)
        .map(_._2))
        .flatten

      val result = dirs.flatMap { d =>
        fromTowards(base, d).map(d -> _)
      }
        .zipWithIndex
        // NB: This will only work correctly if there are at least 200 visible asteroids.  That is, since we're
        // not updating the asteroid list with each destroyed asteroid, we'd never be able to hit ones that were
        // previously blocked on the next pass.  Fixing this would be pretty straightforward (just propagate the
        // current asteroid set at each step of the scan), but it's more complex than is necessary for the'
        // specific problem instance.
        .take(200)

      result.scanLeft((asteroidsSet - base, "")) { case ((as, res), ((dir, loc), i)) =>
        val newAs = as - loc
        (newAs,
            matIn.indices.map { y =>
              matIn(y).indices.map { x =>
                if (base == (x, y)) 'X'
                else if (loc == (x, y)) '*'
                else if (newAs(tag[_Location](x, y))) '#'
                else '.'
              }.mkString("")
            }.mkString("\n", "\n", s"  ^^after $i ${loc} dir $dir\n")
          )
      }.map(_._2)
    }.zio
  }

  def in2: String = """.#..#
                      |.....
                      |#####
                      |....#
                      |...##""".stripMargin
  def in3: String = """.#....#####...#..
                      |##...##.#####..##
                      |##...#...#.#####.
                      |..#.....#...###..
                      |..#.#.....#....##""".stripMargin
  def in4: String = "104,1125899906842624,99"

  def in: String =
    """...###.#########.####
      |.######.###.###.##...
      |####.########.#####.#
      |########.####.##.###.
      |####..#.####.#.#.##..
      |#.################.##
      |..######.##.##.#####.
      |#.####.#####.###.#.##
      |#####.#########.#####
      |#####.##..##..#.#####
      |##.######....########
      |.#######.#.#########.
      |.#.##.#.#.#.##.###.##
      |######...####.#.#.###
      |###############.#.###
      |#.#####.##..###.##.#.
      |##..##..###.#.#######
      |#..#..########.#.##..
      |#.#.######.##.##...##
      |.#.##.#####.#..#####.
      |#.#.##########..#.##.""".stripMargin

}
