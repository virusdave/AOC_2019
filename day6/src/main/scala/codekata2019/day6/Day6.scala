package codekata2019.day6

import scala.annotation.tailrec
import zio._
import zio.console._

object Day6 {
  private val Root = "COM"

  val edges = in.linesIterator.map(_.split(')')).map(l => l(1) -> l(0)).toMap
  def depth(node: String) = {
    @tailrec def go(node: Option[String], depth: Int): Int = node match {
      case None => depth
      case Some(Root) => depth
      case Some(n) => go(edges.get(n), depth + 1)
    }
    go(Option(node), 0)
  }

  object Part1 {
    def solution(silent: Boolean): UIO[Int] = {
      val nonRootNodes = edges.keySet
      ZIO.succeed(nonRootNodes.toSeq.map(depth).sum)
    }
  }

  object Part2 {
    private val You = "YOU"
    private val Santa = "SAN"

    private val youDepth = depth(You)
    private val santaDepth = depth(Santa)

    def solution(silent: Boolean): RIO[Console, Int] = {
      /*@tailrec*/ def up(node: Option[String]): List[String] = node match {
        case Some(Root) | None => Nil
        case Some(n) => n :: up(edges.get(n))
      }

      val yours = up(Option(You))
      val his = up(Option(Santa))

      val common = (yours.reverse zip his.reverse).takeWhile(x => x._1 == x._2)

      putStrLn(s"yours $yours") *>
      putStrLn(s"his $his") *>
      putStrLn(s"common $common") *>
      ZIO.succeed(yours.size + his.size - 2 * common.size - 2)
    }
  }

  def in2:String = """COM)B
                     |B)C
                     |C)D
                     |D)E
                     |E)F
                     |B)G
                     |G)H
                     |D)I
                     |E)J
                     |J)K
                     |K)L""".stripMargin
  def in3:String = """COM)B
                     |B)C
                     |C)D
                     |D)E
                     |E)F
                     |B)G
                     |G)H
                     |D)I
                     |E)J
                     |J)K
                     |K)L
                     |K)YOU
                     |I)SAN""".stripMargin
  def in4:String = "2,4,4,5,99,0"
  // 5110675, (48,47)
  def in:String =
    """""".stripMargin

}
