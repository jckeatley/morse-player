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

type Series[T] = LazyList[T]

/**
  * A trait representing a source of data.
  */
trait Source[T]:
  /**
    * Gets the next sample from the Source.
    *
    * @return The next sample
    */
  def output: Series[T]

/**
  * An interface for any object that generates a stream of constant values.
  *
  * @param level The constant value to generate
  */
class Constant[T](level: T) extends Source[T]:
  override def output: Series[T] = level #:: output

/**
  * A time base generator, generating time in seconds.
  *
  * @param rate The rate at which the timebase advances, in ticks/second.
  */
class TimeBase(rate: Double) extends Source[Time]:
  override def output: Series[Time] = currentTick(0L).map(_ / rate)

  private def currentTick(tick: Long): Series[Long] = tick #:: currentTick(tick + 1L)

class TimeSeries(function: Time => Double) extends (TimeBase => Series[Time]):
  def apply(timeBase: TimeBase): Series[Time] = timeBase.output.map(function)

class Mapper[T, U](op: T => U) extends (Series[T] => Series[U]):
  override def apply(input: Series[T]): Series[U] = input map { v => op(v) }

/**
  * A function that takes two streams and combines them, using an operator specified
  * to operate on the two streams to produce a single output stream.
  *
  * @param op The operator that operates on two values, producing a single value
  * @tparam T The type of the incoming and output stream
  */
class Combiner[T](op: (T, T) => T) extends ((Series[T], Series[T]) => Series[T]):
  override def apply(input1: Series[T], input2: Series[T]): Series[T] = input1.zip(input2) map { (v1, v2) =>
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
class SineFunction(frequency: Double) extends (Time => Double):
  override def apply(input: Time): Double = sin(2.0D * Pi * frequency * input)

/**
  * A square wave generator.
  *
  * @param frequency The frequency of the square waves (Hz).
  */
class SquareWaveFunction(frequency: Double) extends (Time => Double):
  override def apply(time: Time): Double =
    val output = frequency * time
    floor(2.0D * (output - floor(output)))

/**
  * A duration gate, which truncates the incoming stream after the timebase reaches the duration.
  *
  * @param timeBase The timebase to use for timing
  * @param duration the length of time to pass the incoming stream
  */
class DurationGate(timeBase: TimeBase, duration: Double) extends (Series[Double] => Series[Double]):
  override def apply(input: Series[Double]): Series[Double] = gatedStream(input.zip(timeBase.output))

  private def gatedStream(inputAndTime: Series[(Double, Double)]): Series[Double] =
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
  */
class PulseFunction(pulseLength: Time, attackTime: Time, decayTime: Time, startTime: Time = 0.0D) extends (Time => Double):
  override def apply(time: Time): Double =
    if ((time - startTime) >= 0.0 && (time - startTime) < attackTime) {
      (time - startTime) / attackTime
    } else if ((time - startTime) >= attackTime && (time - startTime) < (pulseLength - decayTime)) {
      1.0
    } else if ((time - startTime) >= (pulseLength - decayTime) && (time - startTime) < pulseLength) {
      (pulseLength - time + startTime) / decayTime
    } else {
      0.0
    }

class PulseTrain(attackTime: Double, decayTime: Double):
  def apply(toneStartEnd: Series[(Boolean, Double, Double)], timeBase: TimeBase): Series[Double] =
    toneStartEnd match
      case (tone, start, end) #:: toneStartEndRest =>
        pulseTrain(timeBase.output, tone, start, end, toneStartEndRest)
      case LazyList() => LazyList()

  private def pulseTrain(timeStream: Series[Time], tone: Boolean, startTime: Time, endTime: Time,
      toneStartEnd: Series[(Boolean, Double, Double)]): Series[Double] =
    timeStream match
      case time #:: timeRest =>
        val pulseTime = time - startTime
        val pulseEnd = endTime - startTime
        if (pulseTime >= 0 && pulseTime < pulseEnd) {
          val value = if (tone && pulseTime >= 0.0 && pulseTime < attackTime) {
            pulseTime/attackTime
          } else if (tone && pulseTime >= attackTime && pulseTime < (pulseEnd - decayTime)) {
            1.0
          } else if (tone && pulseTime >= (pulseEnd - decayTime) && pulseTime < pulseEnd) {
            (pulseEnd - pulseTime)/decayTime
          } else {
            0.0
          }
          value #:: pulseTrain(timeRest, tone, startTime, endTime, toneStartEnd)
        } else {
          toneStartEnd match
            case (nextTone, nextStart, nextEnd) #:: toneStartEndRest =>
              pulseTrain(timeRest, nextTone, nextStart, nextEnd, toneStartEndRest)
            case LazyList() => LazyList()
        }

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
