package me.eax.wav_player_example

import java.awt._
import java.awt.geom._
import java.awt.image._
import java.io.File
import javax.imageio.ImageIO

import me.eax.wav_player_example.wav_file._
import me.eax.wav_player_example.wav_file.utils._

object WavPlayerExample extends App {

  def processFile(inputFile: String, outputFile: String, channelNumber: Int, width: Int, height: Int): Unit = {
    var optWavFile: Option[WavFile] = None
    try {
      optWavFile = Some(new WavFile(inputFile))
      optWavFile foreach { wavFile =>
        val info = wavFile.info()
        val rawData = wavFile.rawData()
        val channels = rawDataToChannels(info, rawData)

        /* ****
        import javax.sound.sampled._
        val sampleSizeInBytes = (info.blockSize / info.channels).toInt
        val sampleSizeInBits = sampleSizeInBytes * 8
        val signed = { sampleSizeInBytes == 2 /* 16 bit */ }
        val af = new AudioFormat(info.sampleRate.toFloat, sampleSizeInBits, info.channels.toInt, signed, false)
        val sdl = AudioSystem.getSourceDataLine(af)
        sdl.open()
        sdl.start()
        sdl.write(rawData, 0, rawData.length)
        sdl.drain()
        sdl.stop()
        **** */

        val img = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
        val graphics = img.createGraphics()

        graphics.setColor(new Color(255, 255, 255))
        graphics.fillRect(0, 0, width, height)

        graphics.setColor(new Color(0, 0, 255))

        val totalSamples = channels(channelNumber).length

        for(x <- 0 until width) {
          val fromSample = x * (totalSamples / width)
          val toSample = (x + 1) * (totalSamples / width)
          var min = 0.0
          var max = 0.0
          for(sn <- fromSample to toSample) {
            val sample = channels(channelNumber)(sn)
            min = Math.min(min, sample)
            max = Math.max(max, sample)
          }
          // (0;0) point is top left corner, be careful and don't draw an image upside down!
          graphics.draw(new Line2D.Double(x.toDouble, (height/2).toDouble - max*(height/2).toDouble, x.toDouble, (height/2).toDouble - min*(height/2).toDouble))
        }

        ImageIO.write(img, "PNG", new File(outputFile))
      }
    } finally {
      optWavFile.foreach(_.close())
    }
  }

  if(args.length < 2) {
    println("Usage: waveform <infile> <outfile> [channel=0] [width=512] [height=100]")
  } else {
    val channelNumber = {
      if(args.length < 3) 0 else args(2).toInt
    }
    val width = {
      if(args.length < 4) 512 else args(3).toInt
    }
    val height = {
      if(args.length < 5) 100 else args(4).toInt
    }
    processFile(args(0), args(1), channelNumber, width, height)
  }
}
