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
  case DIT, DAH, SP, FSP

/**
 * Creates a ClipFactory.
 * @param amplitude The amplitude of the waveform.
 * @param frequency The frequency (in Hertz)
 * @param charRate The rate of each individual character (words/minute)
 * @param rate The overall rate of text (words/minute)
 * @param attackTime The attack time of a pulse (seconds)
 * @param decayTime The decay time of a pulse (seconds)
 * @param sampleRate The sample rate (samples/second)
 * @param mixer The optional mixer
 * @author jkeatley
 */
class ClipFactory(amplitude: Double, frequency: Double, charRate: Double, rate: Double, attackTime: Time,
                  decayTime: Time, sampleRate: Double, mixer: Option[Mixer.Info] = None):
  import Element.*

  private val ditLength: Double = 1.2/charRate
  private val fditLength: Double = (300.0*charRate - 186.0*rate)/(95.0*charRate*rate)

  private val morseMap: Map[Char, List[Element]] = Map(
    '.' -> List(DIT, SP, DAH, SP, DIT, SP, DAH, SP, DIT, SP, DAH, FSP, FSP, FSP),           // . _ . _ . _
    ',' -> List(DAH, SP, DAH, SP, DIT, SP, DIT, SP, DAH, SP, DAH, FSP, FSP, FSP),           // _ _ . . _ _
    '?' -> List(DIT, SP, DIT, SP, DAH, SP, DAH, SP, DIT, SP, DIT, FSP, FSP, FSP),           // . . _ _ . .
    '\'' -> List(DIT, SP, DAH, SP, DAH, SP, DAH, SP, DAH, SP, DIT, FSP, FSP, FSP),          // . _ _ _ _ .
    '!' -> List(DAH, SP, DIT, SP, DAH, SP, DIT, SP, DAH, SP, DAH, FSP, FSP, FSP),           // _ . _ . _ _
    '/' -> List(DAH, SP, DIT, SP, DIT, SP, DAH, SP, DIT, FSP, FSP, FSP),                    // _ . . _ .
    '(' -> List(DAH, SP, DIT, SP, DAH, SP, DAH, SP, DIT, FSP, FSP, FSP),                    // _ . _ _ .
    ')' -> List(DAH, SP, DIT, SP, DAH, SP, DAH, SP, DIT, SP, DAH, FSP, FSP, FSP),           // _ . _ _ . _
    '&' -> List(DIT, SP, DAH, SP, DIT, SP, DIT, SP, DIT, FSP, FSP, FSP),                    // . _ . . .
    ':' -> List(DAH, SP, DAH, SP, DAH, SP, DIT, SP, DIT, SP, DIT, FSP, FSP, FSP),           // _ _ _ . . .
    ';' -> List(DAH, SP, DIT, SP, DAH, SP, DIT, SP, DAH, SP, DIT, FSP, FSP, FSP),           // _ . _ . _ .
    '=' -> List(DAH, SP, DIT, SP, DIT, SP, DIT, SP, DAH, FSP, FSP, FSP),                    // _ . . . _
    '+' -> List(DIT, SP, DAH, SP, DIT, SP, DAH, SP, DIT, FSP, FSP, FSP),                    // . _ . _ .
    '-' -> List(DAH, SP, DIT, SP, DIT, SP, DIT, SP, DIT, SP, DAH, FSP, FSP, FSP),           // _ . . . . _
    '_' -> List(DIT, SP, DIT, SP, DAH, SP, DAH, SP, DIT, SP, DAH, FSP, FSP, FSP),           // . . _ _ . _
    '\"' -> List(DIT, SP, DAH, SP, DIT, SP, DIT, SP, DAH, SP, DIT, FSP, FSP, FSP),          // . _ . . _ .
    '$' -> List(DIT, SP, DIT, SP, DIT, SP, DAH, SP, DIT, SP, DIT, SP, DAH, FSP, FSP, FSP),  // . . . _ . . _
    '@' -> List(DIT, SP, DAH, SP, DAH, SP, DIT, SP, DAH, SP, DIT, FSP, FSP, FSP),           // . _ _ . _ .
    ' ' -> List(FSP, FSP, FSP, FSP),                                                        //
    '\n' -> List(FSP, FSP, FSP, FSP),                                                       //
    'A' -> List(DIT, SP, DAH, FSP, FSP, FSP),                                               // . _
    'B' -> List(DAH, SP, DIT, SP, DIT, SP, DIT, FSP, FSP, FSP),                             // _ . . .
    'C' -> List(DAH, SP, DIT, SP, DAH, SP, DIT, FSP, FSP, FSP),                             // _ . _ .
    'D' -> List(DAH, SP, DIT, SP, DIT, FSP, FSP, FSP),                                      // _ . .
    'E' -> List(DIT, FSP, FSP, FSP),                                                        // .
    'F' -> List(DIT, SP, DIT, SP, DAH, SP, DIT, FSP, FSP, FSP),                             // . . _ .
    'G' -> List(DAH, SP, DAH, SP, DIT, FSP, FSP, FSP),                                      // _ _ .
    'H' -> List(DIT, SP, DIT, SP, DIT, SP, DIT, FSP, FSP, FSP),                             // . . . .
    'I' -> List(DIT, SP, DIT, FSP, FSP, FSP),                                               // . .
    'J' -> List(DIT, SP, DAH, SP, DAH, SP, DAH, FSP, FSP, FSP),                             // . _ _ _
    'K' -> List(DAH, SP, DIT, SP, DAH, FSP, FSP, FSP),                                      // _ . _
    'L' -> List(DIT, SP, DAH, SP, DIT, SP, DIT, FSP, FSP, FSP),                             // . _ . .
    'M' -> List(DAH, SP, DAH, FSP, FSP, FSP),                                               // _ _
    'N' -> List(DAH, SP, DIT, FSP, FSP, FSP),                                               // _ .
    'O' -> List(DAH, SP, DAH, SP, DAH, FSP, FSP, FSP),                                      // _ _ _
    'P' -> List(DIT, SP, DAH, SP, DAH, SP, DIT, FSP, FSP, FSP),                             // . _ _ .
    'Q' -> List(DAH, SP, DAH, SP, DIT, SP, DAH, FSP, FSP, FSP),                             // _ _ . _
    'R' -> List(DIT, SP, DAH, SP, DIT, FSP, FSP, FSP),                                      // . _ .
    'S' -> List(DIT, SP, DIT, SP, DIT, FSP, FSP, FSP),                                      // . . .
    'T' -> List(DAH, FSP, FSP, FSP),                                                        // _
    'U' -> List(DIT, SP, DIT, SP, DAH, FSP, FSP, FSP),                                      // . . _
    'V' -> List(DIT, SP, DIT, SP, DIT, SP, DAH, FSP, FSP, FSP),                             // . . . _
    'W' -> List(DIT, SP, DAH, SP, DAH, FSP, FSP, FSP),                                      // . _ _
    'X' -> List(DAH, SP, DIT, SP, DIT, SP, DAH, FSP, FSP, FSP),                             // _ . . _
    'Y' -> List(DAH, SP, DIT, SP, DAH, SP, DAH, FSP, FSP, FSP),                             // _ . _ _
    'Z' -> List(DAH, SP, DAH, SP, DIT, SP, DIT, FSP, FSP, FSP),                             // _ _ . .
    '0' -> List(DAH, SP, DAH, SP, DAH, SP, DAH, SP, DAH, FSP, FSP, FSP),                    // _ _ _ _ _
    '1' -> List(DIT, SP, DAH, SP, DAH, SP, DAH, SP, DAH, FSP, FSP, FSP),                    // . _ _ _ _
    '2' -> List(DIT, SP, DIT, SP, DAH, SP, DAH, SP, DAH, FSP, FSP, FSP),                    // . . _ _ _
    '3' -> List(DIT, SP, DIT, SP, DIT, SP, DAH, SP, DAH, FSP, FSP, FSP),                    // . . . _ _
    '4' -> List(DIT, SP, DIT, SP, DIT, SP, DIT, SP, DAH, FSP, FSP, FSP),                    // . . . . _
    '5' -> List(DIT, SP, DIT, SP, DIT, SP, DIT, SP, DIT, FSP, FSP, FSP),                    // . . . . .
    '6' -> List(DAH, SP, DIT, SP, DIT, SP, DIT, SP, DIT, FSP, FSP, FSP),                    // _ . . . .
    '7' -> List(DAH, SP, DAH, SP, DIT, SP, DIT, SP, DIT, FSP, FSP, FSP),                    // _ _ . . .
    '8' -> List(DAH, SP, DAH, SP, DAH, SP, DIT, SP, DIT, FSP, FSP, FSP),                    // _ _ _ . .
    '9' -> List(DAH, SP, DAH, SP, DAH, SP, DAH, SP, DIT, FSP, FSP, FSP)                     // _ _ _ _ .
  )
  private val sineFunc: SineFunction = SineFunction(frequency)
  private val sineSeries: TimeSeries = TimeSeries(sineFunc)
  private val quantizer: Quantizer = Quantizer(-1.0D, 1.0D, -32767 to 32767)
  private val quantizerMapper: Mapper[Double, Int] = Mapper(quantizer)
  private val format: AudioFormat = AudioFormat(AudioFormat.Encoding.PCM_SIGNED, sampleRate.toFloat, 16, 1, 2,
    sampleRate.toFloat, true)
  private val timeBase: TimeBase = TimeBase(sampleRate)
  private val pulseTrain: PulseTrain = PulseTrain(attackTime, decayTime)
  private var clipCache = Map.empty[Char, Clip]

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

  private def createMorseStream(elems: List[Element]): LazyList[Int] =
    val ampStream: Constant[Double] = new Constant[Double](amplitude)
    val analogStream: LazyList[Double] = Multiplier(sineSeries(timeBase),
      Multiplier(pulseTrain(elemsToTiming(elems), timeBase), ampStream.output))
    quantizerMapper(analogStream)

  private def elemsToTiming(elems: List[Element], start: Double = 0.0): LazyList[(Boolean, Double, Double)] =
    elems match
      case DIT :: rest =>
        (true, start, start + ditLength) #:: elemsToTiming(rest, start + ditLength)
      case DAH :: rest =>
        (true, start, start + 3.0 * ditLength) #:: elemsToTiming(rest, start + 3.0 * ditLength)
      case SP :: rest =>
        (false, start, start + ditLength) #:: elemsToTiming(rest, start + ditLength)
      case FSP :: rest =>
        (false, start, start + fditLength) #:: elemsToTiming(rest, start + fditLength)
      case _ =>
        LazyList()
    end match

  private def createClipFromIntStream(values: LazyList[Int]): Clip =
    val baos: ByteArrayOutputStream = new ByteArrayOutputStream
    val dos: DataOutputStream = new DataOutputStream(baos)
    values.foreach(value => dos.writeShort(value.toShort))
    dos.flush()
    dos.close()
    val buffer: Array[Byte] = baos.toByteArray
    val clip: Clip = mixer.map(AudioSystem.getClip).getOrElse(AudioSystem.getClip)
    clip.open(format, buffer, 0, buffer.length)
    clip

  def printSamples[T](stream: LazyList[T], count: Int): Unit =
    stream.take(count).foreach(println)

end ClipFactory
