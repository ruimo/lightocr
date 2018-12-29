package com.ruimo.lightocr

import java.awt.image.BufferedImage

import com.ruimo.graphics.twodim.{Bits2d, Rectangle}
import com.ruimo.scoins.Percent

import scala.annotation.tailrec
import scala.util.control.TailCalls._
import scala.collection.{immutable => imm}
import scala.util.control.TailCalls.TailRec

object CharSplitter {
  val MaxWidth = 2048
  val MaxHeight = 256

  class MaxWidthExceededException(val width: Int) extends Exception
  class MaxHeightExceededException(val height: Int) extends Exception

  def splitChars(
    img: Bits2d,
    maxWidth: Int = MaxWidth, maxHeight: Int = MaxHeight,
    hEdgeThreshold: Percent = Percent(5), vEdgeThresholdPerHeight: Percent = Percent(5),
    acceptableYgap: Percent = Percent(5),
    hSplitter: Bits2d => imm.Seq[Int]
  ): imm.Seq[Rectangle] = {
    val w = img.width
    val h = img.height

    if (w > maxWidth) throw new MaxWidthExceededException(w)
    if (h > maxHeight) throw new MaxHeightExceededException(h)

    findVerticalRange(img, vEdgeThresholdPerHeight, acceptableYgap) match {
      case None => imm.Seq()
    }

    imm.Seq()
  }

  // Returns y start(inclusive) and y end(exclusive)
  def findVerticalRange(
    img: Bits2d, vEdgeThresholdPerHeight: Percent, acceptableYgap: Percent
  ): Option[(Int, Int)] = {
    val charExistsRange: imm.Seq[Range] = {
      @tailrec def loop(y: Int = 0, start: Option[Int] = None, sum: imm.Seq[Range] = imm.Seq()): imm.Seq[Range] =
        if (y >= img.height) {
          start match {
            case None => sum
            case Some(s) => sum :+ Range(s, y)
          }
        } else {
          val mayCharExists: Boolean = Percent(pixcelCountH(img, y) * 100 / img.height) >= vEdgeThresholdPerHeight
          if (mayCharExists) {
            start match {
              case None => loop(y + 1, Some(y), sum)
              case Some(s) => loop(y + 1, start, sum)
            }
          } else {
            start match {
              case None => loop(y + 1, start, sum)
              case Some(s) => loop(y + 1, None, sum :+ Range(s, y))
            }
          }
        }

      loop()
    }
    val converged: imm.Seq[Range] = {
      val acceptableYgapDots = acceptableYgap.of(img.height)

      @tailrec def loop(list: List[Range], sum: imm.Seq[Range] = imm.Seq()): imm.Seq[Range] = list match {
        case h0::h1::tail =>
          if (h1.start - h0.end <= acceptableYgapDots) loop(tail, sum :+ Range(h0.start, h1.end))
          else loop(h1::tail, sum :+ h0)
        case head::tail =>
          sum :+ head
        case Nil => sum
      }

      loop(charExistsRange.toList)
    }

    if (converged.isEmpty) None
    else {
      val max = converged.maxBy(_.length)
      Some(max.start, max.end)
    }
  }

  def pixcelCountH(img: Bits2d, y: Int): Int = {
    @tailrec def loop(x: Int = 0, sum: Int = 0): Int =
      if (x >= img.width)
        sum
      else
        loop(x + 1, sum + (if (img(x, y)) 0 else 1))

    loop()
  }
}
