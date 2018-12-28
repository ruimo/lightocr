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
    hEdgeThreshold: Percent = Percent(5), vEdgeThreshold: Percent = Percent(1),
    acceptableYgap: Percent = Percent(1),
    hSplitter: Bits2d => imm.Seq[Int]
  ): imm.Seq[Rectangle] = {
    val w = img.width
    val h = img.height

    if (w > maxWidth) throw new MaxWidthExceededException(w)
    if (h > maxHeight) throw new MaxHeightExceededException(h)

    findVerticalRange(img, vEdgeThreshold, acceptableYgap) match {
      case None => imm.Seq()
    }

    imm.Seq()
  }

  // Returns y start(inclusive) and y end(exclusive)
  def findVerticalRange(
    img: Bits2d, vEdgeThreshold: Percent, acceptableYgap: Percent
  ): Option[(Int, Int)] = {
    val acceptableYgapDots = acceptableYgap.of(img.height)
    def isCharExists(y: Int) = Percent(pixcelCountH(img, y) * 100 / img.width) >= vEdgeThreshold

    def init(y: Int): TailRec[Option[(Int, Int)]] =
      if (y >= img.height) {
        done(None)
      } else {
        if (isCharExists(y))
          tailcall(foundYstart(y, y))
        else
          tailcall(init(y + 1))
      }

    def foundYstart(y: Int, yStart: Int): TailRec[Option[(Int, Int)]] = {
      if (y >= img.height) {
        done(Some(yStart, y))
      } else {
        if (isCharExists(y))
          tailcall(foundYstart(y + 1, yStart))
        else {
          if (y - yStart > acceptableYgapDots)
            tailcall(init(y + 1))
          else
            tailcall(foundYend(y + 1, yStart, y))
        }
      }
    }

    def foundYend(y: Int, yStart: Int, yEnd: Int): TailRec[Option[(Int, Int)]] = {
      if (y >= img.height) {
        done(Some(yStart, yEnd))
      } else {
        if (isCharExists(y))
          tailcall(foundYstart(y + 1, yStart))
        else
          tailcall(foundYend(y + 1, yStart, yEnd))
      }
    }

    init(0).result
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
