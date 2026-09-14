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

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}
import scala.io.Source
import java.io.BufferedInputStream
import java.util.MissingResourceException
import java.util.zip.GZIPInputStream
import javax.sound.sampled.*

import jline.console.ConsoleReader

/**
  * @author jkeatley
  */
object MorsePlayer:
  def main(args: Array[String]): Unit =
    val params = parseArgs(args.toList)
    val minLength = params.get("MIN").asInstanceOf[Option[Int]]
    val maxLength = params.get("MAX").asInstanceOf[Option[Int]]
    val words = params.getOrElse("WORDS", Nil).asInstanceOf[List[String]].reverse
    val file = params.get("FILE").asInstanceOf[Option[String]]
    val tone = params.getOrElse("TONE", 800.0).asInstanceOf[Double]
    val count = params.getOrElse("COUNT", if (words.nonEmpty) words.length else 10).asInstanceOf[Int]
    val charRate = params.getOrElse("CHARRATE", 18.0).asInstanceOf[Double]
    val rate = params.getOrElse("RATE", 13.0).asInstanceOf[Double]
    val genRandom = params.contains("RANDOM")
    val letters = params.contains("LETTERS")
    val digits = params.contains("DIGITS")
    val punctuation = params.contains("PUNCT")
    val quiz = params.contains("QUIZ")
    val fuzzy = params.contains("FUZZY")
    val dictFile = params.get("DICTFILE").asInstanceOf[Option[String]]
    val sampleRate = params.getOrElse("SAMPLERATE", 44100.0).asInstanceOf[Double]
    val mixerName = params.get("MIXER").asInstanceOf[Option[String]]
    val consoleReader = new ConsoleReader()
    val mixerInfos = AudioSystem.getMixerInfo
    val mixerInfo = mixerName.flatMap(nm => mixerInfos.find(p => p.getName == nm))
      .orElse(mixerInfos.find(mi => mi.getName.contains("[default]")))
    val aTime = params.getOrElse("ATIME", 0.002).asInstanceOf[Double]
    val dTime = params.getOrElse("DTIME", 0.002).asInstanceOf[Double]
    val verboseFlag = params.contains("VERBOSE")
    val showVersion = params.contains("VERSION")
    val morsePlayer = new MorsePlayer(1.0, tone, if (rate > charRate) rate else charRate, rate, sampleRate, aTime,
      dTime, mixerInfo)

    if (showVersion)
      val pkg = getClass.getClassLoader.getDefinedPackage("us.keatley.morse")
      println(s"morse-player v.${pkg.getImplementationVersion}")
    else
      if (file.nonEmpty)
        val src = file.map(f => if (f == "-") Source.stdin else Source.fromFile(f)).get

        for
          line <- src.getLines()
        do
          morsePlayer.playString(line.strip() + " ")
          Await.ready(morsePlayer.completed(), Duration.Inf)
      else
        val wordList =
          if (words.nonEmpty)
            words
          else
            val wordGenerator =
              if (genRandom)
                new RandomWordGenerator(maxLength.getOrElse(8), minLength.getOrElse(2), letters, digits, punctuation)
              else
                val source =
                  dictFile.map(Source.fromFile).getOrElse {
                    val path = "english.dict.gz"
                    val classLoader = getClass.getClassLoader
                    val is = classLoader.getResourceAsStream(path)
                    if (is == null)
                      throw new MissingResourceException(s"Cannot load resource: $path", getClass.getName, path)
                    Source.fromInputStream(new BufferedInputStream(new GZIPInputStream(is)))
                  }
                new DictWordGenerator(source, maxLength, minLength)
            wordGenerator.genWords(count)

        if (quiz)
          if (verboseFlag)
            println(s"Word rate: $rate wpm, Character rate: $charRate wpm")

          val quiz =
            if (fuzzy)
              FuzzyQuiz(morsePlayer, consoleReader, wordList)
            else
              StandardQuiz(morsePlayer, consoleReader, wordList)
          quiz.runQuiz()
        else
          for
            word <- wordList
          do
            morsePlayer.playString(word + " ")
            Await.ready(morsePlayer.completed(), Duration.Inf)
        end if
      end if
    end if

  @tailrec
  private def parseArgs(args: List[String], accum: Map[String,Any] = Map.empty): Map[String,Any] =
    args match
      case ("-m" | "--minlength") :: min :: rest =>
        parseArgs(rest, accum + ("MIN" -> min.toInt))
      case ("-M" | "--maxlength") :: max :: rest =>
        parseArgs(rest, accum + ("MAX" -> max.toInt))
      case ("-t" | "--tone") :: tone :: rest =>
        parseArgs(rest, accum + ("TONE" -> tone.toDouble))
      case ("-C" | "--charrate") :: wpm :: rest =>
        parseArgs(rest, accum + ("CHARRATE" -> wpm.toDouble))
      case ("-r" | "--rate") :: rate :: rest =>
        parseArgs(rest, accum + ("RATE" -> rate.toDouble))
      case ("-c" | "--count") :: count :: rest =>
        parseArgs(rest, accum + ("COUNT" -> count.toInt))
      case ("-R" | "--random") :: rest =>
        parseArgs(rest, accum + ("RANDOM" -> true))
      case ("-l" | "--letters") :: rest =>
        parseArgs(rest, accum + ("LETTERS" -> true))
      case ("-D" | "--digits") :: rest =>
        parseArgs(rest, accum + ("DIGITS" -> true))
      case ("-p" | "--punctuation") :: rest =>
        parseArgs(rest, accum + ("PUNCT" -> true))
      case ("-q" | "--quiz") :: rest =>
        parseArgs(rest, accum + ("QUIZ" -> true))
      case ("-d" | "--dictionary") :: dictFile :: rest =>
        parseArgs(rest, accum + ("DICTFILE" -> dictFile))
      case ("-f" | "--file") :: file :: rest =>
        parseArgs(rest, accum + ("FILE" -> file))
      case ("-s" | "--samplerate") :: sampleRate :: rest =>
        parseArgs(rest, accum + ("SAMPLERATE" -> sampleRate.toDouble))
      case "--fuzzy" :: rest =>
        parseArgs(rest, accum + ("FUZZY" -> true))
      case "--mixer" :: mixer :: rest =>
        parseArgs(rest, accum + ("MIXER" -> mixer))
      case "--attacktime" :: atime :: rest =>
        parseArgs(rest, accum + ("ATIME" -> atime.toDouble))
      case "--decaytime" :: dtime :: rest =>
        parseArgs(rest, accum + ("DTIME" -> dtime.toDouble))
      case "--verbose" :: rest =>
        parseArgs(rest, accum + ("VERBOSE" -> true))
      case "--version" :: rest =>
        parseArgs(rest, accum + ("VERSION" -> true))
      case "--help" :: rest =>
        usage()
      case word :: rest =>
        val words = accum.getOrElse("WORDS", Nil).asInstanceOf[List[String]]
        parseArgs(rest, accum + ("WORDS" -> (word :: words)))
      case Nil => accum
    end match

  private def usage(): Nothing =
    println(
      """morse-player - Morse code player and quiz generator
        |Usage:
        |   morse-player [options]
        |Where [options] include:
        |   -m | --minlength <minLength> - The minimum length of words.
        |   -M | --maxlength <maxLength> - The maximum length of words.
        |   -t | --tone <tone>           - The frequency of the tone, in hertz [800].
        |   -C | --charrate <wpm>        - The rate of each morse character, in words/minute [18].
        |   -r | --rate <wpm>            - The rate, in words/minute [13].
        |   -c | --count <count>         - The number of words to quiz.
        |   -R | --random                - Use randomly-generated words [default: dictionary].
        |   -l | --letters               - Use letters [true].
        |   -D | --digits                - Include digits in the random words [false].
        |   -p | --punctuation           - Include punctuation in the random words [false].
        |   -q | --quiz                  - Present a quiz.
        |   -d | --dictionary <dictfile> - The list of words to load [default: built-in dictionary].
        |   -f | --file <file>           - Input file, '-' for stdin.
        |   -s | --samplerate <srate>    - Sample rate [44100.0]
        |   --fuzzy                      - Use fuzzy comparison of answers.
        |   --mixer <mixer-name>         - The name of the mixer to use.
        |   --attacktime <atime>         - Attack time (seconds) [0.002]
        |   --decaytime <dtime>          - Decay time (seconds) [0.002]
        |   --verbose                    - Show more details.
        |   --version                    - Display the app version.
        |   --help                       - This help text.""".stripMargin)
    sys.exit(1)

/**
  * Creates a MorsePlayer.
  * @param amplitude The amplitude of the waveform, above and below 0
  * @param frequency The frequency in Hertz
  * @param charRate The rate of each individual character (words/minute)
  * @param rate The overall rate of Morse characters (words/minute)
  * @param sampleRate The sample rate of the digital audio
  * @param attackTime The attack time (seconds)
  * @param decayTime The decay time (seconds)
  * @param mixer The mixer to play the audio through
  */
class MorsePlayer(amplitude: Double, frequency: Double, charRate: Double, rate: Double, sampleRate: Double,
                  attackTime: Double, decayTime: Double, mixer: Option[Mixer.Info] = None) extends LineListener:
  private var promise: Promise[MorsePlayer] = Promise[MorsePlayer]()
  private val clipFactory = new ClipFactory(amplitude, frequency, charRate, rate, attackTime, decayTime, sampleRate, mixer)

  def playString(s: String): Unit =
    val clip = clipFactory.createClip(s)
    clip.addLineListener(this)
    clip.setFramePosition(0)
    clip.start()

  override def update(event: LineEvent): Unit =
    if (event.getType == LineEvent.Type.STOP)
      val line = event.getLine
      line.removeLineListener(this)
      if (line.isOpen)
        line.close()
      promise.success(this)
      promise = Promise[MorsePlayer]()

  def completed(): Future[MorsePlayer] = promise.future
