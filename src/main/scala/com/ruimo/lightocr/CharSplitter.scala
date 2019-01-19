package com.ruimo.lightocr

import java.awt.image.BufferedImage
import java.nio.file.Paths
import scala.math.abs
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
    acceptableYgap: Percent = Percent(5), acceptableXgap: Percent = Percent(15),
    minCharBodyWidthPerHeight: Percent = Percent(15),
    minCharWidthPerHeight: Percent = Percent(50), maxCharWidthPerHeight: Percent = Percent(110)
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

        splitCharHorizontally(
          vCropped, hEdgeThresholdPerHeight, acceptableXgap, minCharBodyWidthPerHeight,
          minCharWidthPerHeight, maxCharWidthPerHeight
        )
    }
  }

  private[lightocr] def complementRange(rangeTable: imm.Seq[Range], indexUntil: Int): imm.Seq[Range] = {
    @tailrec def loop(i: Int = 0, ranges: imm.Seq[Range], result: imm.Seq[Range] = imm.Seq()): imm.Seq[Range] = {
      if (ranges.isEmpty) {
        if (indexUntil <= i) result
        else result :+ Range(i, indexUntil)
      } else {
        val head = ranges.head
        if (i < head.start) {
          loop(head.end, ranges.tail, result :+ Range(i, head.start))
        } else {
          loop(head.end, ranges.tail, result)
        }
      }
    }

    loop(ranges = rangeTable)
  }

  private[lightocr] def findTrueRange(isTrueFunc: Int => Boolean, indexUntil: Int): imm.Seq[Range] = {
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
  def splitCharHorizontally(
    img: Bits2d, hEdgeThresholdPerHeight: Percent, acceptableXgap: Percent,
    minCharBodyWidthPerHeight: Percent,
    minCharWidthPerHeight: Percent, maxCharWidthPerHeight: Percent
  ): imm.Seq[Bits2d] = {
    val minCharBodyWidth = minCharBodyWidthPerHeight.of(img.height)
    val charExistsRange: imm.Seq[Range] = findTrueRange(
      x => Percent(blackPixcelCountV(img, x) * 100 / img.height) >= hEdgeThresholdPerHeight,
      img.width
    ).filter { _.length >= minCharBodyWidth }
    val edgeChoppedRange: Option[Range] = {
      def chopEdge(ranges: imm.Seq[Range]): imm.Seq[Range] = {
        val minWidth = minCharBodyWidthPerHeight.of(img.height).toInt

        val leftChopped = ranges.headOption match {
          case None => ranges
          case Some(h) => if (h.length < minWidth) ranges.tail else ranges
        }

        leftChopped.lastOption match {
          case None => leftChopped
          case Some(l) => if (l.length < minWidth) leftChopped.dropRight(1) else leftChopped
        }
      }

      val aggregated: imm.Seq[Range] = chopEdge(aggregateRange(charExistsRange.toList, acceptableXgap.of(img.height).toInt))
      aggregated.size match {
        case 0 => None
        case 1 => Some(aggregated.head)
        case _ => Some(Range(aggregated.head.start, aggregated.last.end))
      }
    }

    edgeChoppedRange.map { range =>
      val charNotExistsRange: imm.Seq[Range] = complementRange(charExistsRange, img.width).filter { r =>
        range.start <= r.start && r.end <= range.end
      }
      if (charNotExistsRange.isEmpty) {
        imm.Seq(Bits2d.subImage(img, range.start, 0, range.length, img.height))
      } else if (charNotExistsRange.size == 1) {
        val r = charNotExistsRange.head
        val x = r.start + (r.end - r.start) / 2
        imm.Seq(
          Bits2d.subImage(img, range.start, 0, x - range.start, img.height),
          Bits2d.subImage(img, x, 0, range.end - x, img.height)
        )
      } else {
        val splitCandPoints: imm.Seq[Double] = charNotExistsRange.map(r => r.start + (r.end - r.start).toDouble / 2)
        estimatedWidth(
          splitCandPoints, minCharWidthPerHeight.of(img.height), maxCharWidthPerHeight.of(img.height)
        ).map { charWidth =>
          val p = findMostApplicableSplitPoint(splitCandPoints, charWidth)
          val d = ((p - range.start) / charWidth).toInt
          val startX = p - d * charWidth
          val minCharWidth = minCharWidthPerHeight.of(img.height)

          def loop(result: imm.Seq[Bits2d] = imm.Seq(), n: Int = 1): imm.Seq[Bits2d] = {
            val x0 = (startX + (n - 1) * charWidth + 0.5).toInt
            val x1 = (startX + n * charWidth + 0.5).toInt
            if (x1 >= range.end) {
              if (minCharWidth <= range.end - x0) result :+ Bits2d.subImage(img, x0, 0, range.end - x0, img.height)
              else result
            } else {
              loop(result :+ Bits2d.subImage(img, x0, 0, x1 - x0, img.height), n + 1)
            }
          }

          if (minCharBodyWidth <= startX - range.start)
            loop(imm.Seq(Bits2d.subImage(img, range.start, 0, (startX - range.start).toInt, img.height)))
          else
            loop()
        }
      }.getOrElse(imm.Seq())
    }.getOrElse(imm.Seq())
  }

  private[lightocr] def findMostApplicableSplitPoint(
    splitPoints: imm.Seq[Double], estimatedCharWidth: Double, acceptableError: Percent = Percent(10)
  ): Double = {
    if (splitPoints.isEmpty) throw new IllegalArgumentException("splitPoints empty")
    if (splitPoints.size == 1) splitPoints(0)
    else {
      def errorSum(sp: Double): Double = splitPoints.foldLeft(0.0) { case (sum, p) =>
        if (p == sp) sum
        else {
          val diff = abs(sp - p)
          val d = (0.5 + diff / estimatedCharWidth).toInt
          val e = abs(diff - estimatedCharWidth * d)
          sum + e
        }
      }

      splitPoints.map { p => (p, errorSum(p)) }.minBy(_._2)._1
    }
  }

  private[lightocr] def estimatedWidth(
    splitPoints: imm.Seq[Double], minCharWidth: Double, maxCharWidth: Double
  ): Option[Double] = {
    val splitWidths: imm.Seq[Double] =
      splitPoints.sliding(2).map(e => e(1) - e(0)).filter { w => minCharWidth <= w && w <= maxCharWidth }.toVector

    if (splitWidths.isEmpty) None
    else {
      case class QuantizedWidth(quantized: Double, realWidth: Double)
      object QuantizedWidth {
        val N = 10

        def apply(w: Double): QuantizedWidth = QuantizedWidth((w * N).toInt / N, w)
      }
      val quantizedWidths: imm.Seq[QuantizedWidth] = splitWidths.map(QuantizedWidth.apply)
      val sortedWidthCounts: imm.Seq[(Double, Int)] = quantizedWidths.foldLeft(
        imm.Map[Double, Int]().withDefaultValue(0)
      ) { case (sum, e) =>
        sum.updated(e.quantized, sum(e.quantized) + 1)
      }.toVector.sortBy(e => -e._2)
      val countSum = sortedWidthCounts.map(_._2).sum
      val Rank = Percent(60)
      val countThreshold = Rank.of(countSum)

      @tailrec def loop(counts: imm.Seq[(Double, Int)], count: Int = 0, result: imm.Set[Double] = imm.Set()): imm.Set[Double] = {
        if (counts.isEmpty) result
        else if (countThreshold <= count) result
        else loop(counts.tail, count + counts.head._2, result + counts.head._1)
      }

      val ws = loop(sortedWidthCounts)
      val rankHigh: imm.Seq[QuantizedWidth] = quantizedWidths.filter(q => ws.contains(q.quantized))
      Some(rankHigh.map(_.realWidth).sum / rankHigh.size)
    }
  }

  // returns X offset and split count.
  def findCharSplitCount(img: Bits2d, minCharWidthPerHeight: Percent, maxCharWidthPerHeight: Percent): (Int, Int) = {
    val w = img.visibleRect.width
    val h = img.visibleRect.height

    val minCharWidth = minCharWidthPerHeight.of(h).toInt
    val maxCharWidth = maxCharWidthPerHeight.of(h).toInt

    val maxCount: Int = ((w + minCharWidth - 1) / minCharWidth)
    val minCount = w / maxCharWidth

    case class ErrorSum(count: Int = 0, errorSum: Int = 0) {
      def average: Double = errorSum.toDouble / count
      def addError(error: Int): ErrorSum = copy(count + 1, errorSum + error)
    }

    def errorSum(splitCount: Int, offset: Int): ErrorSum =
      if (splitCount == 1) ErrorSum() else {
        val initialError = if (offset == 0) ErrorSum() else ErrorSum().addError(blackPixcelCountV(img, offset - 1))

        (1 until splitCount).foldLeft(initialError) { (sum, i) =>
          val x = offset + w * i / splitCount
          if (x >= w) sum else sum.addError(blackPixcelCountV(img, x))
        }
      }

    val result = (minCount to maxCount).map { cnt =>
      val charWidth = w / cnt
      val offsetAndMinErr  = (0 until charWidth).foldLeft((0, Double.MaxValue)) { (min, offset) =>
        val err = errorSum(cnt, offset).average
        if (err < min._2) (offset, err) else min
      }
      (cnt, offsetAndMinErr)
    }.minBy { case (c, oame) =>
      oame._2
    }

    (result._2._1, result._1)
  }

  private[this] def aggregateRange(ranges: List[Range], acceptableGap: Int): imm.Seq[Range] = {
    @tailrec def loop(
      list: List[Range], work: Option[Range] = None, sum: imm.Seq[Range] = imm.Seq()
    ): imm.Seq[Range] = list match {
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
