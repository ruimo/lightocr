package com.ruimo.lightocr

import java.io.File

import com.ruimo.graphics.twodim.Bits2d
import com.ruimo.scoins.Percent
import javax.imageio.ImageIO
import org.specs2.mutable.Specification

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
  }
}
