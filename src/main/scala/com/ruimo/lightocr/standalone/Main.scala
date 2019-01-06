package com.ruimo.lightocr.standalone

import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j
import java.awt.{Color, Graphics2D, RenderingHints}
import java.awt.image.BufferedImage
import java.io.File
import java.nio.file.{Files, Paths}
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.api.ops.impl.indexaccum.IAMax
import org.nd4j.linalg.factory.Nd4j

import com.ruimo.graphics.twodim.Bits2d
import com.ruimo.lightocr.CharSplitter
import com.ruimo.scoins.Percent
import javax.imageio.ImageIO
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork
import org.deeplearning4j.datasets.iterator.impl.MnistDataSetIterator
import org.nd4j.linalg.dataset.api.iterator.DataSetIterator
import org.deeplearning4j.nn.conf.MultiLayerConfiguration
import org.deeplearning4j.nn.conf.NeuralNetConfiguration
import org.deeplearning4j.nn.conf.layers.DenseLayer
import org.deeplearning4j.nn.conf.layers.OutputLayer
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork
import org.deeplearning4j.nn.weights.WeightInit
import org.nd4j.linalg.activations.Activation
import org.nd4j.linalg.learning.config.Nesterovs
import org.nd4j.linalg.lossfunctions.LossFunctions.LossFunction

object Main {
  def main(args: Array[String]) {
    if (args.length == 0) help()
    else {
      args(0).toLowerCase match {
        case "split" => split(args.tail.toList)
        case "ocr" => ocr(args.tail.toList)
        case "help" => help()
        case _ =>
          throw new IllegalArgumentException("Function " + args(0) + " is invalid. Specify help for usage.")
      }
    }
  }

  def help() {
    println(
      """
Usage:
  help
    Show this message

  split imageFile0 imageFile1...
    Split digits in each imageFileN. The split digits are store in a directory named base name of imageFileN.
    ex) split test001.png
      Split the test001.png into the directory 'test0001'. File names of digits are 000.png, 001.png ...

  ocr imageFile0 imageFile1...
    OCR the specified image as digits. The OCRed digits are store in a file named the same base name having ".txt" extension.
    ex) ocr test001.png
      OCR the test001.png into test001.txt
"""
    )
  }

  def baseName(fname: String): String = {
    fname.lastIndexOf('.') match {
      case -1 => fname
      case n: Int => fname.substring(0, n)
    }
  }

  def split(args: List[String]) {
    args.foreach { imgFile =>
      val outDir = Paths.get(imgFile).resolveSibling(baseName(imgFile))
      Files.createDirectories(outDir)

      CharSplitter.splitChars(
        Bits2d(ImageIO.read(new File(imgFile))),
        hEdgeThresholdPerHeight = Percent(5), vEdgeThresholdPerHeight = Percent(5),
        acceptableYgap = Percent(5),
        minCharWidthPerHeight = Percent(50), maxCharWidthPerHeight = Percent(90)
      ).zipWithIndex.foreach { case (split, i) =>
          split.save(outDir.resolve(f"$i%03d.png"))
      }
    }
  }

  def trainMnist(): MultiLayerNetwork = {
    val batchSize = 64
    val rngSeed = 123
    val rate = 0.0015
    val numRows = 28
    val numColumns = 28
    val outputNum = 10
    val numEpochs = 15 // number of epochs to perform

    val conf = new NeuralNetConfiguration.Builder()
      .seed(rngSeed)
      .activation(Activation.RELU)
      .weightInit(WeightInit.XAVIER)
      .updater(new Nesterovs(rate, 0.98))
      .l2(rate * 0.005) // regularize learning model
      .list()
      .layer(0, new DenseLayer.Builder() //create the first input layer.
        .nIn(numRows * numColumns)
        .nOut(500)
        .build())
      .layer(1, new DenseLayer.Builder() //create the second input layer
        .nIn(500)
        .nOut(100)
        .build())
      .layer(2, new OutputLayer.Builder(LossFunction.NEGATIVELOGLIKELIHOOD) //create hidden layer
        .activation(Activation.SOFTMAX)
        .nIn(100)
        .nOut(outputNum)
        .build())
      .build()

    val model = new MultiLayerNetwork(conf)
    model.init()

    val mnistTrain = new MnistDataSetIterator(batchSize, true, rngSeed)
    (0 until numEpochs).foreach { n =>
      model.fit(mnistTrain)
    }

    model
  }

  def ocr(args: List[String]) {
    val model = trainMnist()

    args.foreach { imgFile =>
      val outFile = Paths.get(imgFile).resolveSibling(baseName(imgFile) + ".txt")
      CharSplitter.splitChars(
        Bits2d(ImageIO.read(new File(imgFile))),
        hEdgeThresholdPerHeight = Percent(5), vEdgeThresholdPerHeight = Percent(5),
        acceptableYgap = Percent(5),
        minCharWidthPerHeight = Percent(50), maxCharWidthPerHeight = Percent(90)
      ).zipWithIndex.foreach { case (charImg, i) =>
        val resizedImg = resizeImage(charImg.toBufferedImage)
        invertImage(resizedImg)
        ImageIO.write(resizedImg, "png", new File("/tmp/test" + i + ".png"))

        val data = new Array[Double](28 * 28)
        (0 until 28).foreach { y =>
          (0 until 28).foreach { x =>
            val color = new Color(resizedImg.getRGB(x, y))
            val r = color.getRed
            val g = color.getGreen
            val b = color.getBlue
            val greyScale = (r + g + b) / 3
            data(x + y * 28) = greyScale
          }
        }

        val arr = Nd4j.create(data)
        val eval = model.output(arr)
        val idx = Nd4j.getExecutioner.execAndReturn(new IAMax(eval)).getFinalResult

        println("digit = " + idx)
      }
    }
  }

  def resizeImage(image: BufferedImage): BufferedImage = {
    val width = 28
    val height = 28
    val hborder = 2
    val vborder = 2

    val resizedImage = new BufferedImage(width, height, image.getType)
    val g2d = resizedImage.createGraphics
    g2d.setColor(Color.WHITE)
    g2d.fillRect(0, 0, width, height)
    g2d.setRenderingHint(RenderingHints.KEY_ALPHA_INTERPOLATION, RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY)
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g2d.setRenderingHint(RenderingHints.KEY_COLOR_RENDERING, RenderingHints.VALUE_COLOR_RENDER_QUALITY)
    g2d.setRenderingHint(RenderingHints.KEY_DITHERING, RenderingHints.VALUE_DITHER_ENABLE)
    g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
    g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
    g2d.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON)
    g2d.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_NORMALIZE)
    g2d.drawImage(image, hborder, vborder, width - 2 * hborder, height - 2 * vborder, null)
    resizedImage
  }

  def invertImage(image: BufferedImage): Unit = {
    (0 until image.getHeight).foreach { y =>
      (0 until image.getWidth).foreach { x =>
        val c = new Color(image.getRGB(x, y))
        val invC = new Color(255 - c.getRed, 255 - c.getGreen, 255 - c.getBlue)
        image.setRGB(x, y, invC.getRGB)
      }
    }
  }
}

