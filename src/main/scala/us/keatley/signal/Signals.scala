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

package us.keatley.signal

import scala.math.*

type Time = Double

type TimeFunction = (Time => Double)

/**
  * A trait representing a source of data.
  */
trait Source[T]:
  /**
    * Gets the next sample from the Source.
    *
    * @return The next sample
    */
  def output: LazyList[T]

/**
  * An interface for any object that generates a stream of constant values.
  *
  * @param level The constant value to generate
  */
class Constant[T](level: T) extends Source[T]:
  override def output: LazyList[T] = level #:: output

/**
  * A time base generator, generating time in seconds.
  *
  * @param rate The rate at which the timebase advances, in ticks/second.
  */
class TimeBase(rate: Double) extends Source[Time]:
  override def output: LazyList[Time] = currentTick(0L).map(_ / rate)

  private def currentTick(tick: Long): LazyList[Long] = tick #:: currentTick(tick + 1L)

class TimeSeries(function: TimeFunction) extends (TimeBase => LazyList[Time]):
  def apply(timeBase: TimeBase): LazyList[Time] = timeBase.output.map(function)

class Mapper[T, U](op: T => U) extends (LazyList[T] => LazyList[U]):
  override def apply(input: LazyList[T]): LazyList[U] = input map { v => op(v) }

/**
  * A function that takes two streams and combines them, using an operator specified
  * to operate on the two streams to produce a single output stream.
  *
  * @param op The operator that operates on two values, producing a single value
  * @tparam T The type of the incoming and output stream
  */
class Combiner[T](op: (T, T) => T) extends ((LazyList[T], LazyList[T]) => LazyList[T]):
  override def apply(input1: LazyList[T], input2: LazyList[T]): LazyList[T] = input1.zip(input2) map { (v1, v2) =>
    op(v1, v2)
  }

/**
  * A combiner that multiplies the two incoming streams.
  */
object Multiplier extends Combiner[Double](_ * _)

/**
  * A combiner that adds the two incoming streams.
  */
object Adder extends Combiner[Double](_ + _)

/**
  * A sine wave generator.
  *
  * @param frequency The frequency of the sine function (Hz).
  */
class SineFunction(frequency: Double) extends TimeFunction:
  override def apply(input: Time): Double = sin(2.0D * Pi * frequency * input)

/**
  * A square wave generator.
  *
  * @param frequency The frequency of the square waves (Hz).
  */
class SquareWaveFunction(frequency: Double) extends TimeFunction:
  override def apply(time: Time): Double =
    val output = frequency * time
    floor(2.0D * (output - floor(output)))

/**
  * A duration gate, which truncates the incoming stream after the timebase reaches the duration.
  *
  * @param timeBase The timebase to use for timing
  * @param duration the length of time to pass the incoming stream
  */
class DurationGate(timeBase: TimeBase, duration: Double) extends (LazyList[Double] => LazyList[Double]):
  override def apply(input: LazyList[Double]): LazyList[Double] = gatedStream(input.zip(timeBase.output))

  private def gatedStream(inputAndTime: LazyList[(Double, Double)]): LazyList[Double] =
    inputAndTime match
      case (value, time) #:: rest =>
        if (time <= duration) {
          value #:: gatedStream(rest)
        } else {
          LazyList()
        }
      case LazyList() => LazyList()

/**
  * A pulse function, with pulse length, attack time, and decay time.  Attack and decay are linear changes
  * from 0 to 1 and back again.  Amplitude is 1.0.
  *
  * @param pulseLength The total length of the pulse, including attack and decay
  * @param attackTime  The length of time it takes for the amplitude to increase from 0 to 1
  * @param decayTime   The length of time it takes for the amplitude to decrease from 1 to 0
  * @param startTime   The start time of the pulse.
  */
class PulseFunction(val pulseLength: Time, val attackTime: Time, val decayTime: Time, val startTime: Time = 0.0D) extends TimeFunction:
  override def apply(time: Time): Double =
    val lTime = time - startTime
    if (lTime >= 0.0 && lTime < attackTime)
      lTime / attackTime
    else if (lTime >= attackTime && lTime < (pulseLength - decayTime))
      1.0
    else if (lTime >= (pulseLength - decayTime) && lTime < pulseLength)
      (pulseLength - lTime) / decayTime
    else
      0.0

  def isFuture(time: Time): Boolean = time < startTime

  def isInRange(time: Time): Boolean =
    time >= startTime && time < startTime + pulseLength

class PulseTrain2:
  def apply(pulses: LazyList[PulseFunction], timeBase: TimeBase): LazyList[Double] =
    pulseTrain(pulses, timeBase.output)

  def pulseTrain(pulses: LazyList[PulseFunction], timeSeries: LazyList[Time]): LazyList[Double] =
    timeSeries match
      case time #:: timeBaseRest =>
        pulses match
          case pulse #:: pulsesRest if (pulse.isFuture(time)) =>
            0.0 #:: pulseTrain(pulses, timeBaseRest)
          case pulse #:: pulsesRest if (pulse.isInRange(time)) =>
            pulse(time) #:: pulseTrain(pulses, timeBaseRest)
          case pulse #:: pulsesRest =>
            pulseTrain(pulsesRest, timeBaseRest)
          case LazyList() =>
            LazyList()
      case _ =>
        LazyList()

class PulseTrain(attackTime: Double, decayTime: Double):
  def apply(toneStartEnd: LazyList[(Boolean, Double, Double)], timeBase: TimeBase): LazyList[Double] =
    toneStartEnd match
      case (tone, start, end) #:: toneStartEndRest =>
        pulseTrain(timeBase.output, tone, start, end, toneStartEndRest)
      case LazyList() => LazyList()

  private def pulseTrain(timeStream: LazyList[Time], tone: Boolean, startTime: Time, endTime: Time,
      toneStartEnd: LazyList[(Boolean, Double, Double)]): LazyList[Double] =
    timeStream match
      case time #:: timeStreamRest =>
        val pulseTime = time - startTime
        val pulseEnd = endTime - startTime
        if (pulseTime >= 0 && pulseTime < pulseEnd)
          val value =
            if (tone && pulseTime >= 0.0 && pulseTime < attackTime)
              pulseTime/attackTime
            else if (tone && pulseTime >= attackTime && pulseTime < (pulseEnd - decayTime))
              1.0
            else if (tone && pulseTime >= (pulseEnd - decayTime) && pulseTime < pulseEnd)
              (pulseEnd - pulseTime)/decayTime
            else
              0.0
          value #:: pulseTrain(timeStreamRest, tone, startTime, endTime, toneStartEnd)
        else
          toneStartEnd match
            case (nextTone, nextStart, nextEnd) #:: toneStartEndRest =>
              pulseTrain(timeStreamRest, nextTone, nextStart, nextEnd, toneStartEndRest)
            case LazyList() => LazyList()

/**
  * A quantizer, which forces the input double stream into a discrete set of values.
  *
  * @param minInput The minimum value accepted.
  * @param maxInput The maximum value accepted.
  * @param outRange The output range, i.e. 0 to 65535
  */
class Quantizer(minInput: Double, maxInput: Double, outRange: Range) extends (Double => Int):
  override def apply(input: Double): Int =
    val out = ((input - minInput) * outRange.length / (maxInput - minInput)).toInt + outRange.start
    if (out < outRange.start) {
      outRange.start
    } else if (out > outRange.end) {
      outRange.end
    } else {
      out
    }

// vi:set ts=4 sw=4 noet:
