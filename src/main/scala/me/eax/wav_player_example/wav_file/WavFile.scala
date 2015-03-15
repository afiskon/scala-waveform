package me.eax.wav_player_example.wav_file

import java.io._
import me.eax.wav_player_example.wav_file.utils._

object WavFile {
  val headerSize: Int = 44
  val riffSignature = signatureToLong("RIFF")
  val waveFmtSignature = signatureToLong("WAVEfmt ")
  val dataSignature = signatureToLong("data")

  private def signatureToLong(signature: String): Long = {
    if(signature.length > 8) throw new RuntimeException(s"Invalid signature length: $signature")
    signature.reverse.foldLeft(0L) { case(acc, x) => (acc << 8) | (x.toLong & 0xFF) }
  }
}

class WavFile(fileName: String) {
  private val fileStream = new BufferedInputStream(new FileInputStream(fileName))

  private val wavInfo = {
    val fileSize = new File(fileName).length()
    val rawData = Array.ofDim[Byte](WavFile.headerSize)
    val bytesRead = fileStream.read(rawData, 0, WavFile.headerSize)
    if (bytesRead != rawData.size) throw new RuntimeException("Failed to read wav header")
    decodeWavHeader(rawData, fileSize)
  }

  private val wavRawData = {
    val rawData = Array.ofDim[Byte](wavInfo.dataSize.toInt)
    val bytesRead = fileStream.read(rawData, 0, wavInfo.dataSize.toInt)
    if(bytesRead != wavInfo.dataSize.toInt) throw new RuntimeException("bytesRead != wavInfo.dataSize.toInt")
    rawData
  }

  def info(): WavInfo = wavInfo

  def rawData(): Array[Byte] = wavRawData

  def close(): Unit = Option(fileStream).foreach(_.close())

  private def decodeWavHeader(rawData: Array[Byte], fileSize: Long): WavInfo = {
    if(rawData.size < WavFile.headerSize) throw new RuntimeException(s"Invalid wav header size ${rawData.size}}")
    checkSignature(rawData,  0,  4, WavFile.riffSignature)
    checkSignature(rawData,  4,  8, fileSize - 8L)
    checkSignature(rawData,  8, 16, WavFile.waveFmtSignature)
    checkSignature(rawData, 16, 20, 16L)
    checkSignature(rawData, 20, 22, 1L)
    val channels         = arraySliceToLong(rawData, 22, 24)
    val sampleRate       = arraySliceToLong(rawData, 24, 28)
    val bytesPerSecond   = arraySliceToLong(rawData, 28, 32)
    val blockSize        = arraySliceToLong(rawData, 32, 34)
    val bitsPerSample    = arraySliceToLong(rawData, 34, 36)
    checkSignature(rawData, 36, 40, WavFile.dataSignature)
    val expectedDataSize = fileSize - WavFile.headerSize
    checkSignature(rawData, 40, 44, expectedDataSize)

    if(sampleRate * blockSize != bytesPerSecond)
      throw new RuntimeException(
        "Error in wav header: sampleRate * blockSize != bytesPerSecond " +
          s"($sampleRate * $blockSize != $bytesPerSecond)"
      )

    if((bytesPerSecond / sampleRate / channels) * 8 != bitsPerSample)
      throw  new RuntimeException(
        "Error in wav header: (bytesPerSecond / sampleRate) * 8 != bitsPerSample " +
          s"($bytesPerSecond / $sampleRate) * 8 != $bitsPerSample"
      )

    WavInfo(channels, sampleRate, blockSize, expectedDataSize)
  }

  private def checkSignature(rawData: Array[Byte], from: Int, until: Int, expected: Long): Unit = {
    val actual = arraySliceToLong(rawData, from, until)
    if(actual != expected) {
      val err = "Invalid signature from %d until %d: 0x%08X expected, but 0x%08X found" format (from, until, expected, actual)
      throw new RuntimeException(err)
    }
  }
}
