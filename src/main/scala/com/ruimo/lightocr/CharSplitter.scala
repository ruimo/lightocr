package com.ruimo.lightocr

import java.awt.image.BufferedImage
import java.nio.file.Paths

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
    hEdgeThresholdPerHeight: Percent = Percent(5), vEdgeThresholdPerHeight: Percent = Percent(5),
    acceptableYgap: Percent = Percent(5),
    minCharWidthPerHeight: Percent = Percent(50), maxCharWidthPerHeight: Percent = Percent(90)
  ): imm.Seq[Bits2d] = {
    val w = img.width
    val h = img.height

    if (w > maxWidth) throw new MaxWidthExceededException(w)
    if (h > maxHeight) throw new MaxHeightExceededException(h)

    findVerticalRange(img, vEdgeThresholdPerHeight, acceptableYgap) match {
      case None => imm.Seq()
      case Some((vStart, vEnd)) =>
        val vCropped = Bits2d.subImage(img, 0, vStart, img.width, vEnd - vStart)
        val intMinCharWidth = minCharWidthPerHeight.of(vCropped.height).toInt
        val intMaxCharWidth = maxCharWidthPerHeight.of(vCropped.height).toInt


        findHorizontalRange(vCropped, hEdgeThresholdPerHeight) match {
          case None => imm.Seq()
          case Some((hStart, hEnd)) =>
            val vhCropped = Bits2d.subImage(vCropped, hStart, 0, hEnd - hStart, vCropped.height)
            val n = findCharSplitCount(vhCropped, minCharWidthPerHeight, maxCharWidthPerHeight)
            if (n == 0) imm.Seq()
            else if (n == 1) imm.Seq(vhCropped)
            else {
              (1 to n).map { i =>
                val x0 = vhCropped.width * (i - 1) / n
                val x1 = vhCropped.width * i / n
                Bits2d.subImage(vhCropped, x0, 0, x1 - x0, vhCropped.height)
              }
            }
        }
    }
  }

  def findTrueRange(isTrueFunc: Int => Boolean, indexUntil: Int): imm.Seq[Range] = {
    @tailrec def loop(i: Int = 0, start: Option[Int] = None, sum: imm.Seq[Range] = imm.Seq()): imm.Seq[Range] =
      if (i >= indexUntil) {
        start match {
          case None => sum
          case Some(s) => sum :+ Range(s, i)
        }
      } else {
        if (isTrueFunc(i)) {
          start match {
            case None => loop(i + 1, Some(i), sum)
            case Some(s) => loop(i + 1, start, sum)
          }
        } else {
          start match {
            case None => loop(i + 1, start, sum)
            case Some(s) => loop(i + 1, None, sum :+ Range(s, i))
          }
        }
      }

    loop()
  }

  // Returns y start(inclusive) and y end(exclusive)
  def findVerticalRange(
    img: Bits2d, vEdgeThresholdPerHeight: Percent, acceptableYgap: Percent
  ): Option[(Int, Int)] = {
    val charExistsRange: imm.Seq[Range] = findTrueRange(
      y => Percent(blackPixcelCountH(img, y) * 100 / img.height) >= vEdgeThresholdPerHeight,
      img.height
    )
    val aggregated: imm.Seq[Range] = aggregateRange(charExistsRange.toList, acceptableYgap.of(img.height).toInt)

    if (aggregated.isEmpty) None
    else {
      val max = aggregated.maxBy(_.length)
      Some(max.start, max.end)
    }
  }

  // Returns x start(inclusive) and x end(exclusive)
  def findHorizontalRange(
    img: Bits2d, hEdgeThresholdPerHeight: Percent
  ): Option[(Int, Int)] = {
    img.save(Paths.get("/tmp/test.png"))

    val charExistsRange: imm.Seq[Range] = findTrueRange(
      x => Percent(blackPixcelCountV(img, x) * 100 / img.height) >= hEdgeThresholdPerHeight,
      img.width
    )


    def isCharExists(x: Int): Boolean =
      Percent(blackPixcelCountV(img, x) * 100 / img.height) >= hEdgeThresholdPerHeight

    @tailrec def findLeft(x: Int = 0): Option[Int] =
      if (x >= img.width)
        None
      else
        if (isCharExists(x)) Some(x) else findLeft(x + 1)

    @tailrec def findRight(x: Int = img.width - 1): Option[Int] =
      if (x < 0)
        None
      else
        if (isCharExists(x)) Some(x) else findRight(x - 1)

    findLeft().flatMap { l =>
      findRight().flatMap { r =>
        if (l >= r) None else Some(l, r + 1)
      }
    }
  }

  def findCharSplitCount(img: Bits2d, minCharWidthPerHeight: Percent, maxCharWidthPerHeight: Percent): Int = {
    val w = img.visibleRect.width
    val h = img.visibleRect.height

    val minCharWidth = minCharWidthPerHeight.of(h).toInt
    val maxCharWidth = maxCharWidthPerHeight.of(h).toInt

    val maxCount: Int = ((w + minCharWidth - 1) / minCharWidth)
    val minCount = w / maxCharWidth

    def errorCount(splitCount: Int): Int =
      if (splitCount == 1) 0 else (1 until splitCount).foldLeft(0) { (sum, i) =>
        val x = w * i / splitCount
        sum + blackPixcelCountV(img, x)
      }

    (minCount to maxCount).map { cnt =>
      (cnt, errorCount(cnt))
    }.minBy { case (c, e) =>
      e / c
    }._1
  }

  private[this] def aggregateRange(ranges: List[Range], acceptableGap: Int): imm.Seq[Range] = {
    @tailrec def loop(list: List[Range], work: Option[Range] = None, sum: imm.Seq[Range] = imm.Seq()): imm.Seq[Range] = list match {
      case Nil => sum ++ work

      case h::t =>
        work match {
          case None => loop(t, Some(h), sum)
          case Some(w) =>
            if (h.start - w.end <= acceptableGap) loop(t, Some(Range(w.start, h.end)), sum)
            else loop(t, Some(h), sum :+ w)
        }
    }

    loop(ranges)
  }

  private[this] def blackPixcelCountH(img: Bits2d, y: Int): Int = countFalse(x => img(x, y), img.width)

  private[this] def blackPixcelCountV(img: Bits2d, x: Int): Int = countFalse(y => img(x, y), img.height)

  private[this] def countFalse(f: Int => Boolean, idxUntil: Int): Int = {
    (0 until idxUntil).foldLeft(0) { (sum, idx) =>
      sum + (if (f(idx)) 0 else 1)
    }
  }
}
