package ru.spbau.jirkov.calcgeom.insidepoly

/*Задача первая - полигон

На вход N вершин полигона в формате (x, y) [abs(x), abs(y) <= 10^5] и M точек запроса. На выходе -- M строк yes/no. Полигон всегда корректный, закрученный против часовой стрелки. Полигон считать замкнутым, т.е. для точек на границе ожидаемый ответ -- yes. Пример входа:
3
(0, 0)
(3, 0)
(0, 2)
3
(0, 1)
(1, 1)
(2, 1)
Ожидаемый выход:
yes
yes
no
*/

object InsidePolygon {

  case class Dot(x: Double, y: Double)

  type Edge = (Dot, Dot)

  def alignX(edge: Edge) = if (edge._1.x <= edge._2.x) edge else edge.swap

  def crossYCoord(y: Double)(edge: Edge): Option[Double] = {
    def _cross(e: Edge) = e._1.y <= y && e._2.y >= y
    if (_cross(edge) || _cross(edge.swap)) {
      val e = alignX(edge)
      val x0 = e._1.x
      val x1 = e._2.x
      val y0 = e._1.y
      val y1 = e._2.y
      Some((x1 - x0) * (y - y0) / (y1 - y0))
    } else None
  }

  def left(e: Edge) = e._1.x min e._2.x

  def check(poly: List[Dot])(dot: Dot): Boolean = {
    val edges = poly.zip(poly.tail ++ List(poly.head)).map(alignX)
    val interestingPoints = edges.map(crossYCoord(dot.y)).flatten
    interestingPoints.contains(dot.x) || interestingPoints.count(_ < dot.x) % 2 != 0
  }
}


import InsidePolygon._

import scala.io.StdIn.{readDouble, readInt, readLine}

object Solver extends App {

  def readDots() : List[Dot] = {
    def readDot() : Dot = {
      val regexp =  """\((.+), *(.+)\)""".r
      readLine() match {
        case regexp(x, y) => Dot(x.toDouble, y.toDouble)
        case _ => throw new IllegalArgumentException
      }
    }
    val count = readInt()
    var lst = List.empty[Dot]
    for(i <- 0 until count)
      lst::= readDot()
    lst.reverse
  }

  val poly = readDots()
  val queries = readDots()

  val checker = check(poly) _

  for (query <- queries)
    println(if ( checker(query)) "yes" else "no")

}