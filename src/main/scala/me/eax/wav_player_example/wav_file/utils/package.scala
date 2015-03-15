package me.eax.wav_player_example.wav_file

package object utils {
  case class WavInfo(
                      channels: Long,
                      sampleRate: Long,
                      blockSize: Long,
                      dataSize: Long)

  type ChannelData = Array[Double]
  type Channels = Array[ChannelData]

  def rawDataToChannels(wavInfo: WavInfo, rawData: Array[Byte]): Channels = {
    val samplesNumber = (wavInfo.dataSize / wavInfo.blockSize).toInt
    val wavChannels: Channels = (for(_ <- 1L to wavInfo.channels) yield Array.ofDim[Double](samplesNumber)).toArray

    val sampleSizeInBytes = (wavInfo.blockSize / wavInfo.channels).toInt
    val maxAmplitude = Math.pow(2, 8 * sampleSizeInBytes - 1)

    for(i <- 0 until samplesNumber) {
      for(ch <- 0 until wavInfo.channels.toInt) {
        val from = (i * wavInfo.blockSize + ch * (wavInfo.blockSize / wavInfo.channels)).toInt
        val sampleLong = arraySliceToLong(rawData, from, from + sampleSizeInBytes)
        val sampleDouble = {
          if(sampleSizeInBytes == 2) sampleLong.toShort.toLong // toShort converts 0xFFFF to -1, etc
          else (Byte.MaxValue.toLong - sampleLong).toDouble
        }
        wavChannels(ch)(i) = sampleDouble / maxAmplitude
      }
    }

    wavChannels
  }

  def arraySliceToLong(array: Array[Byte], from: Int, until: Int): Long = {
    val slice = array.slice(from, until)
    if(slice.size > 8) throw new RuntimeException(s"Invalid array slice length: $slice")
    slice.reverse.foldLeft(0L) { case(acc, x) => (acc << 8) | (x.toLong & 0xFF) }
  }
}
