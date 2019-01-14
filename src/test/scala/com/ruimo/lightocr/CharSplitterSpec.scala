package com.ruimo.lightocr

import java.io.File
import java.nio.file.Paths

import com.ruimo.graphics.twodim.Bits2d
import com.ruimo.scoins.Percent
import javax.imageio.ImageIO
import org.specs2.mutable.Specification
import scala.collection.{immutable => imm}

class CharSplitterSpec extends Specification {
  "CharSplitterSpec" should {
    "Can detect y range" in {
      val img001 = Bits2d(ImageIO.read(new File("testdata/char-splitter/001.png")))
      CharSplitter.findVerticalRange(img001, Percent(5), Percent(5)) === Some(4, 16)

      val img002 = Bits2d(ImageIO.read(new File("testdata/char-splitter/002.png")))
      CharSplitter.findVerticalRange(img002, Percent(5), Percent(5)) === Some(11, 24)

      val img003 = Bits2d(ImageIO.read(new File("testdata/char-splitter/003.png")))
      CharSplitter.findVerticalRange(img003, Percent(5), Percent(5)) === Some(6, 19)

      val img004 = Bits2d(ImageIO.read(new File("testdata/char-splitter/004.png")))
      CharSplitter.findVerticalRange(img004, Percent(5), Percent(5)) === Some(6, 19)

      val img005 = Bits2d(ImageIO.read(new File("testdata/char-splitter/005.png")))
      CharSplitter.findVerticalRange(img005, Percent(5), Percent(5)) === Some(6, 19)

      val img006 = Bits2d(ImageIO.read(new File("testdata/char-splitter/006.png")))
      CharSplitter.findVerticalRange(img006, Percent(5), Percent(5)) === Some(5, 21)

      val img007 = Bits2d(ImageIO.read(new File("testdata/char-splitter/007.png")))
      CharSplitter.findVerticalRange(img007, Percent(5), Percent(5)) === Some(4, 18)

      val img008 = Bits2d(ImageIO.read(new File("testdata/char-splitter/008.png")))
      CharSplitter.findVerticalRange(img008, Percent(5), Percent(5)) === Some(6, 19)

      val img009 = Bits2d(ImageIO.read(new File("testdata/char-splitter/009.png")))
      CharSplitter.findVerticalRange(img009, Percent(5), Percent(5)) === Some(1, 14)
    }

    "Can split chars count" in {
      val img001 = Bits2d(ImageIO.read(new File("testdata/char-splitter/001.png")))
      val vClip001 = Bits2d.subImage(img001, 0, 4, img001.width, 16 - 4)
      val vhClip001 = Bits2d.subImage(vClip001, 7, 0, 78 - 7, vClip001.height)
      CharSplitter.findCharSplitCount(vhClip001, Percent(50), Percent(90)) === (0, 7)

      val img002 = Bits2d(ImageIO.read(new File("testdata/char-splitter/002.png")))
      val vClip002 = Bits2d.subImage(img002, 0, 11, img002.width, 24 - 11)
      val vhClip002 = Bits2d.subImage(vClip002, 9, 0, 82 - 9, vClip002.height)
      CharSplitter.findCharSplitCount(vhClip002, Percent(50), Percent(90)) === (1, 7)

      val img003 = Bits2d(ImageIO.read(new File("testdata/char-splitter/003.png")))
      val vClip003 = Bits2d.subImage(img003, 0, 6, img003.width, 19 - 6)
      val vhClip003 = Bits2d.subImage(vClip003, 17, 0, 57 - 17, vClip003.height)
      CharSplitter.findCharSplitCount(vhClip003, Percent(50), Percent(90)) === (9, 4)

      val img004 = Bits2d(ImageIO.read(new File("testdata/char-splitter/004.png")))
      val vClip004 = Bits2d.subImage(img004, 0, 6, img004.width, 19 - 6)
      val vhClip004 = Bits2d.subImage(vClip004, 34, 0, 116 - 34, vClip004.height)
      CharSplitter.findCharSplitCount(vhClip004, Percent(50), Percent(90)) === (0, 8)

      val img005 = Bits2d(ImageIO.read(new File("testdata/char-splitter/005.png")))
      val vClip005 = Bits2d.subImage(img005, 0, 6, img005.width, 19 - 6)
      val vhClip005 = Bits2d.subImage(vClip005, 9, 0, 92 - 9, vClip005.height)
      CharSplitter.findCharSplitCount(vhClip005, Percent(50), Percent(90)) === (0, 8)

      val img006 = Bits2d(ImageIO.read(new File("testdata/char-splitter/006.png")))
      val vClip006 = Bits2d.subImage(img006, 0, 5, img006.width, 21 - 5)
      val vhClip006 = Bits2d.subImage(vClip006, 9, 0, 230 - 9, vClip006.height)
      CharSplitter.findCharSplitCount(vhClip006, Percent(50), Percent(90)) === (0, 21)

      val img007 = Bits2d(ImageIO.read(new File("testdata/char-splitter/007.png")))
      val vClip007 = Bits2d.subImage(img007, 0, 4, img007.width, 18 - 4)
      val vhClip007 = Bits2d.subImage(vClip007, 38, 0, 98 - 38, vClip007.height)
      CharSplitter.findCharSplitCount(vhClip007, Percent(50), Percent(90)) === (0, 6)

      val img008 = Bits2d(ImageIO.read(new File("testdata/char-splitter/008.png")))
      val vClip008 = Bits2d.subImage(img008, 0, 6, img008.width, 19 - 6)
      val vhClip008 = Bits2d.subImage(vClip008, 17, 0, 57 - 17, vClip008.height)
      CharSplitter.findCharSplitCount(vhClip008, Percent(50), Percent(90)) === (9, 4)

      val img009 = Bits2d(ImageIO.read(new File("testdata/char-splitter/009.png")))
      val vClip009 = Bits2d.subImage(img009, 0, 1, img009.width, 14 - 1)
      val vhClip009 = Bits2d.subImage(vClip009, 12, 0, 52 - 12, vClip009.height)
      CharSplitter.findCharSplitCount(vhClip009, Percent(50), Percent(90)) === (9, 4)
    }

    "Can split chars 001" in {
      val imgs = CharSplitter.splitChars(
        Bits2d(ImageIO.read(new File("testdata/char-splitter/001.png"))),
        hEdgeThresholdPerHeight = Percent(5), vEdgeThresholdPerHeight = Percent(5),
        acceptableYgap = Percent(5),
        minCharWidthPerHeight = Percent(50), maxCharWidthPerHeight = Percent(90)
      )

      imgs.size === 7

      (0 until 7).foreach { i =>
        Bits2d(ImageIO.read(new File(f"testdata/char-splitter/001/$i%03d.png"))).isSameImage(imgs(i)) === true
      }

      1 === 1
    }

    "Can split chars 002" in {
      val imgs = CharSplitter.splitChars(
        Bits2d(ImageIO.read(new File("testdata/char-splitter/002.png"))),
        hEdgeThresholdPerHeight = Percent(5), vEdgeThresholdPerHeight = Percent(5),
        acceptableYgap = Percent(5),
        minCharWidthPerHeight = Percent(50), maxCharWidthPerHeight = Percent(90)
      )

      imgs.size === 7

      (0 until 7).foreach { i =>
        Bits2d(ImageIO.read(new File(f"testdata/char-splitter/002/$i%03d.png"))).isSameImage(imgs(i)) === true
      }

      1 === 1
    }

    "Can split chars 003" in {
      val imgs = CharSplitter.splitChars(
        Bits2d(ImageIO.read(new File("testdata/char-splitter/003.png"))),
        hEdgeThresholdPerHeight = Percent(5), vEdgeThresholdPerHeight = Percent(5),
        acceptableYgap = Percent(5),
        minCharWidthPerHeight = Percent(50), maxCharWidthPerHeight = Percent(90)
      )

      imgs.size === 4

      (0 until 4).foreach { i =>
        Bits2d(ImageIO.read(new File(f"testdata/char-splitter/003/$i%03d.png"))).isSameImage(imgs(i)) === true
      }

      1 === 1
    }

    "Can split chars 004" in {
      val imgs = CharSplitter.splitChars(
        Bits2d(ImageIO.read(new File("testdata/char-splitter/004.png"))),
        hEdgeThresholdPerHeight = Percent(5), vEdgeThresholdPerHeight = Percent(5),
        acceptableYgap = Percent(5),
        minCharWidthPerHeight = Percent(50), maxCharWidthPerHeight = Percent(90)
      )

      imgs.size === 8

      (0 until 8).foreach { i =>
        Bits2d(ImageIO.read(new File(f"testdata/char-splitter/004/$i%03d.png"))).isSameImage(imgs(i)) === true
      }

      1 === 1
    }

    "Can split chars 005" in {
      val imgs = CharSplitter.splitChars(
        Bits2d(ImageIO.read(new File("testdata/char-splitter/005.png"))),
        hEdgeThresholdPerHeight = Percent(5), vEdgeThresholdPerHeight = Percent(5),
        acceptableYgap = Percent(5),
        minCharWidthPerHeight = Percent(50), maxCharWidthPerHeight = Percent(90)
      )

      imgs.size === 8

      (0 until 8).foreach { i =>
        Bits2d(ImageIO.read(new File(f"testdata/char-splitter/005/$i%03d.png"))).isSameImage(imgs(i)) === true
      }

      1 === 1
    }

    "Can split chars 006" in {
      val imgs = CharSplitter.splitChars(
        Bits2d(ImageIO.read(new File("testdata/char-splitter/006.png"))),
        hEdgeThresholdPerHeight = Percent(5), vEdgeThresholdPerHeight = Percent(5),
        acceptableYgap = Percent(5),
        minCharWidthPerHeight = Percent(50), maxCharWidthPerHeight = Percent(90)
      )

      imgs.size === 21

      (0 until 21).foreach { i =>
        Bits2d(ImageIO.read(new File(f"testdata/char-splitter/006/$i%03d.png"))).isSameImage(imgs(i)) === true
      }

      1 === 1
    }

    "Can split chars 007" in {
      val imgs = CharSplitter.splitChars(
        Bits2d(ImageIO.read(new File("testdata/char-splitter/007.png"))),
        hEdgeThresholdPerHeight = Percent(5), vEdgeThresholdPerHeight = Percent(5),
        acceptableYgap = Percent(5),
        minCharWidthPerHeight = Percent(50), maxCharWidthPerHeight = Percent(90)
      )

      imgs.size === 6

      (0 until 6).foreach { i =>
        Bits2d(ImageIO.read(new File(f"testdata/char-splitter/007/$i%03d.png"))).isSameImage(imgs(i)) === true
      }

      1 === 1
    }

    "Can split chars 008" in {
      val imgs = CharSplitter.splitChars(
        Bits2d(ImageIO.read(new File("testdata/char-splitter/008.png"))),
        hEdgeThresholdPerHeight = Percent(5), vEdgeThresholdPerHeight = Percent(5),
        acceptableYgap = Percent(5),
        minCharWidthPerHeight = Percent(50), maxCharWidthPerHeight = Percent(90)
      )

      imgs.size === 4

      (0 until 4).foreach { i =>
        Bits2d(ImageIO.read(new File(f"testdata/char-splitter/008/$i%03d.png"))).isSameImage(imgs(i)) === true
      }

      1 === 1
    }

    "Can split chars 009" in {
      val imgs = CharSplitter.splitChars(
        Bits2d(ImageIO.read(new File("testdata/char-splitter/009.png"))),
        hEdgeThresholdPerHeight = Percent(5), vEdgeThresholdPerHeight = Percent(5),
        acceptableYgap = Percent(5),
        minCharWidthPerHeight = Percent(50), maxCharWidthPerHeight = Percent(90)
      )

      imgs.size === 4

      (0 until 4).foreach { i =>
        Bits2d(ImageIO.read(new File(f"testdata/char-splitter/009/$i%03d.png"))).isSameImage(imgs(i)) === true
      }

      1 === 1
    }

    "Can split chars 010" in {
      val imgs = CharSplitter.splitChars(
        Bits2d(ImageIO.read(new File("testdata/char-splitter/010/test.png"))),
        hEdgeThresholdPerHeight = Percent(5), vEdgeThresholdPerHeight = Percent(5),
        acceptableYgap = Percent(5),
        minCharWidthPerHeight = Percent(50), maxCharWidthPerHeight = Percent(90)
      )

      imgs.size === 6

      (0 until 6).foreach { i =>
        Bits2d(ImageIO.read(new File(f"testdata/char-splitter/010/$i%03d.png"))).isSameImage(imgs(i)) === true
      }

      1 === 1
    }

    "Can split chars 011" in {
      val imgs = CharSplitter.splitChars(
        Bits2d(ImageIO.read(new File("testdata/char-splitter/011/test.png"))),
        hEdgeThresholdPerHeight = Percent(5), vEdgeThresholdPerHeight = Percent(5),
        acceptableYgap = Percent(5),
        minCharWidthPerHeight = Percent(50), maxCharWidthPerHeight = Percent(90)
      )

      imgs.size === 6

      (0 until 6).foreach { i =>
        Bits2d(ImageIO.read(new File(f"testdata/char-splitter/011/$i%03d.png"))).isSameImage(imgs(i)) === true
      }

      1 === 1
    }

    "Complement ranges" in {
      CharSplitter.complementRange(imm.Seq(), 0) === imm.Seq()
      CharSplitter.complementRange(imm.Seq(), 10) === imm.Seq(Range(0, 10))
      CharSplitter.complementRange(imm.Seq(Range(0, 10)), 10) === imm.Seq()
      CharSplitter.complementRange(imm.Seq(Range(1, 10)), 10) === imm.Seq(Range(0, 1))
      CharSplitter.complementRange(imm.Seq(Range(1, 8)), 10) === imm.Seq(Range(0, 1), Range(8, 10))
      CharSplitter.complementRange(imm.Seq(Range(1, 3), Range(3, 5)), 10) === imm.Seq(Range(0, 1), Range(5, 10))
      CharSplitter.complementRange(imm.Seq(Range(1, 3), Range(5, 7)), 10) === imm.Seq(Range(0, 1), Range(3, 5), Range(7, 10))
      CharSplitter.complementRange(imm.Seq(Range(1, 3), Range(5, 7)), 7) === imm.Seq(Range(0, 1), Range(3, 5))
      CharSplitter.complementRange(imm.Seq(Range(0, 3), Range(5, 7)), 7) === imm.Seq(Range(3, 5))
      CharSplitter.complementRange(imm.Seq(Range(0, 3), Range(5, 7)), 8) === imm.Seq(Range(3, 5), Range(7, 8))
    }
  }
}
