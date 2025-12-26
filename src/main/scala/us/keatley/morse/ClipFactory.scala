/*
 * Copyright (C) 2025 Jonathan Keatley
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program. If not, see <https://www.gnu.org/licenses/>.
 */

package us.keatley.morse

import java.io.{ByteArrayOutputStream, DataOutputStream}
import javax.sound.sampled.{AudioFormat, AudioSystem, Clip, Mixer}

import us.keatley.signal.*

enum Element:
  case DIT, DAH, SPACE, FSPACE

/**
  * @author jkeatley
  */
class ClipFactory(amplitude: Double, frequency: Double, charRate: Double, rate: Double, attackTime: Time,
                  decayTime: Time, mixer: Option[Mixer.Info] = None):
  import Element.*

  private val ditLength: Double = 1.2/charRate
  private val fditLength: Double = (300.0*charRate - 186.0*rate)/(95.0*charRate*rate)

  private val morseMap: Map[Char, List[Element]] = Map(
    '.' -> List(DIT, SPACE, DAH, SPACE, DIT, SPACE, DAH, SPACE, DIT, SPACE, DAH, FSPACE, FSPACE, FSPACE),
    ',' -> List(DAH, SPACE, DAH, SPACE, DIT, SPACE, DIT, SPACE, DAH, SPACE, DAH, FSPACE, FSPACE, FSPACE),
    '?' -> List(DIT, SPACE, DIT, SPACE, DAH, SPACE, DAH, SPACE, DIT, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    '\'' -> List(DIT, SPACE, DAH, SPACE, DAH, SPACE, DAH, SPACE, DAH, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    '!' -> List(DAH, SPACE, DIT, SPACE, DAH, SPACE, DIT, SPACE, DAH, SPACE, DAH, FSPACE, FSPACE, FSPACE),
    '/' -> List(DAH, SPACE, DIT, SPACE, DIT, SPACE, DAH, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    '(' -> List(DAH, SPACE, DIT, SPACE, DAH, SPACE, DAH, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    ')' -> List(DAH, SPACE, DIT, SPACE, DAH, SPACE, DAH, SPACE, DIT, SPACE, DAH, FSPACE, FSPACE, FSPACE),
    '&' -> List(DIT, SPACE, DAH, SPACE, DIT, SPACE, DIT, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    ':' -> List(DAH, SPACE, DAH, SPACE, DAH, SPACE, DIT, SPACE, DIT, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    ';' -> List(DAH, SPACE, DIT, SPACE, DAH, SPACE, DIT, SPACE, DAH, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    '=' -> List(DAH, SPACE, DIT, SPACE, DIT, SPACE, DIT, SPACE, DAH, FSPACE, FSPACE, FSPACE),
    '+' -> List(DIT, SPACE, DAH, SPACE, DIT, SPACE, DAH, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    '-' -> List(DAH, SPACE, DIT, SPACE, DIT, SPACE, DIT, SPACE, DIT, SPACE, DAH, FSPACE, FSPACE, FSPACE),
    '_' -> List(DIT, SPACE, DIT, SPACE, DAH, SPACE, DAH, SPACE, DIT, SPACE, DAH, FSPACE, FSPACE, FSPACE),
    '\"' -> List(DIT, SPACE, DAH, SPACE, DIT, SPACE, DIT, SPACE, DAH, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    '$' -> List(DIT, SPACE, DIT, SPACE, DIT, SPACE, DAH, SPACE, DIT, SPACE, DIT, SPACE, DAH, FSPACE, FSPACE, FSPACE),
    '@' -> List(DIT, SPACE, DAH, SPACE, DAH, SPACE, DIT, SPACE, DAH, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    ' ' -> List(FSPACE, FSPACE, FSPACE, FSPACE),
    '\n' -> List(FSPACE, FSPACE, FSPACE, FSPACE),
    'A' -> List(DIT, SPACE, DAH, FSPACE, FSPACE, FSPACE),
    'B' -> List(DAH, SPACE, DIT, SPACE, DIT, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    'C' -> List(DAH, SPACE, DIT, SPACE, DAH, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    'D' -> List(DAH, SPACE, DIT, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    'E' -> List(DIT, FSPACE, FSPACE, FSPACE),
    'F' -> List(DIT, SPACE, DIT, SPACE, DAH, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    'G' -> List(DAH, SPACE, DAH, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    'H' -> List(DIT, SPACE, DIT, SPACE, DIT, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    'I' -> List(DIT, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    'J' -> List(DIT, SPACE, DAH, SPACE, DAH, SPACE, DAH, FSPACE, FSPACE, FSPACE),
    'K' -> List(DAH, SPACE, DIT, SPACE, DAH, FSPACE, FSPACE, FSPACE),
    'L' -> List(DIT, SPACE, DAH, SPACE, DIT, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    'M' -> List(DAH, SPACE, DAH, FSPACE, FSPACE, FSPACE),
    'N' -> List(DAH, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    'O' -> List(DAH, SPACE, DAH, SPACE, DAH, FSPACE, FSPACE, FSPACE),
    'P' -> List(DIT, SPACE, DAH, SPACE, DAH, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    'Q' -> List(DAH, SPACE, DAH, SPACE, DIT, SPACE, DAH, FSPACE, FSPACE, FSPACE),
    'R' -> List(DIT, SPACE, DAH, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    'S' -> List(DIT, SPACE, DIT, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    'T' -> List(DAH, FSPACE, FSPACE, FSPACE),
    'U' -> List(DIT, SPACE, DIT, SPACE, DAH, FSPACE, FSPACE, FSPACE),
    'V' -> List(DIT, SPACE, DIT, SPACE, DIT, SPACE, DAH, FSPACE, FSPACE, FSPACE),
    'W' -> List(DIT, SPACE, DAH, SPACE, DAH, FSPACE, FSPACE, FSPACE),
    'X' -> List(DAH, SPACE, DIT, SPACE, DIT, SPACE, DAH, FSPACE, FSPACE, FSPACE),
    'Y' -> List(DAH, SPACE, DIT, SPACE, DAH, SPACE, DAH, FSPACE, FSPACE, FSPACE),
    'Z' -> List(DAH, SPACE, DAH, SPACE, DIT, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    '0' -> List(DAH, SPACE, DAH, SPACE, DAH, SPACE, DAH, SPACE, DAH, FSPACE, FSPACE, FSPACE),
    '1' -> List(DIT, SPACE, DAH, SPACE, DAH, SPACE, DAH, SPACE, DAH, FSPACE, FSPACE, FSPACE),
    '2' -> List(DIT, SPACE, DIT, SPACE, DAH, SPACE, DAH, SPACE, DAH, FSPACE, FSPACE, FSPACE),
    '3' -> List(DIT, SPACE, DIT, SPACE, DIT, SPACE, DAH, SPACE, DAH, FSPACE, FSPACE, FSPACE),
    '4' -> List(DIT, SPACE, DIT, SPACE, DIT, SPACE, DIT, SPACE, DAH, FSPACE, FSPACE, FSPACE),
    '5' -> List(DIT, SPACE, DIT, SPACE, DIT, SPACE, DIT, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    '6' -> List(DAH, SPACE, DIT, SPACE, DIT, SPACE, DIT, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    '7' -> List(DAH, SPACE, DAH, SPACE, DIT, SPACE, DIT, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    '8' -> List(DAH, SPACE, DAH, SPACE, DAH, SPACE, DIT, SPACE, DIT, FSPACE, FSPACE, FSPACE),
    '9' -> List(DAH, SPACE, DAH, SPACE, DAH, SPACE, DAH, SPACE, DIT, FSPACE, FSPACE, FSPACE)
  )
  private val sineFunc: SineFunction = SineFunction(frequency)
  private val sineSeries: TimeSeries = TimeSeries(sineFunc)
  private val quantizer: Quantizer = Quantizer(-1.0D, 1.0D, -32767 to 32767)
  private val quantizerMapper: Mapper[Double, Int] = Mapper(quantizer)
  private val format: AudioFormat = AudioFormat(AudioFormat.Encoding.PCM_SIGNED, 44100.0F, 16, 1, 2, 44100.0F, true)
  private var clipCache = Map.empty[Char, Clip]
  private val timeBase: TimeBase = TimeBase(44100.0D)
  private val pulseTrain: PulseTrain = PulseTrain(attackTime, decayTime)

  def createClip(s: String): Clip =
    createClipFromIntStream(createMorseStream(s.map(_.toUpper).flatMap(morseMap.get).flatten.toList))

  def createClip(c: Char): Option[Clip] =
    val uc = c.toUpper
    clipCache.get(uc) match
      case Some(clip) => Some(clip)
      case _ =>
        morseMap.get(uc) match
          case Some(elems) =>
            val clip = createClipFromIntStream(createMorseStream(elems))
            clipCache = clipCache + (uc -> clip)
            Some(clip)
          case _ => None
        end match
    end match

  private def createMorseStream(elems: List[Element]): Series[Int] =
    val ampStream: Constant[Double] = new Constant[Double](amplitude)
    val analogStream: Series[Double] = Multiplier(sineSeries(timeBase),
      Multiplier(pulseTrain(elemsToTiming(elems), timeBase), ampStream.output))
    quantizerMapper(analogStream)

  private def elemsToTiming(elems: List[Element], start: Double = 0.0): Series[(Boolean, Double, Double)] =
    elems match
      case DIT :: rest =>
        (true, start, start + ditLength) #:: elemsToTiming(rest, start + ditLength)
      case DAH :: rest =>
        (true, start, start + 3.0 * ditLength) #:: elemsToTiming(rest, start + 3.0 * ditLength)
      case SPACE :: rest =>
        (false, start, start + ditLength) #:: elemsToTiming(rest, start + ditLength)
      case FSPACE :: rest =>
        (false, start, start + fditLength) #:: elemsToTiming(rest, start + fditLength)
      case _ =>
        LazyList()
    end match

  private def createClipFromIntStream(values: Series[Int]): Clip =
    val baos: ByteArrayOutputStream = new ByteArrayOutputStream
    val dos: DataOutputStream = new DataOutputStream(baos)
    values.foreach(value => dos.writeShort(value.toShort))
    dos.flush()
    dos.close()
    val buffer: Array[Byte] = baos.toByteArray
    val clip: Clip = mixer.map(AudioSystem.getClip).getOrElse(AudioSystem.getClip)
    clip.open(format, buffer, 0, buffer.length)
    clip

  def printSamples[T](stream: Series[T], count: Int): Unit =
    stream.take(count).foreach(println)

end ClipFactory
