package us.keatley.morse

import scala.concurrent.Await
import scala.concurrent.duration.Duration

import jline.console.ConsoleReader

abstract class Quiz(morsePlayer: MorsePlayer, consoleReader: ConsoleReader, wordList: Seq[String]):
  def runQuiz(): Unit =
    val statusColumn = wordList.map(_.length).max + 16

    for
      (word, n) <- wordList.zipWithIndex
    do
      morsePlayer.playString(word)
      Await.ready(morsePlayer.completed(), Duration.Inf)
      val indexFmt = String.format("%3d", n + 1)
      val prompt = s"$indexFmt: "
      val input = consoleReader.readLine(prompt).trim()
      consoleReader.getOutput.write(s"\u001b[A\u001b[${statusColumn}G   \u001b[K")
      scoreWord(word, input)
      consoleReader.println()
      consoleReader.flush()
    end for

    printScore()

  def scoreWord(word: String, guessedWord: String): Unit

  def printScore(): Unit

class StandardQuiz(morsePlayer: MorsePlayer, consoleReader: ConsoleReader, wordList: Seq[String])
  extends Quiz(morsePlayer, consoleReader, wordList):
  private var score: Int = 0

  override def scoreWord(word: String, guessedWord: String): Unit =
    if (guessedWord.compareToIgnoreCase(word) == 0)
      consoleReader.getOutput.write(s"${Console.GREEN}Correct!${Console.RESET}")
      score += 1
    else
      consoleReader.getOutput.write(s"${Console.RED}Wrong!${Console.RESET} -- $word")

  override def printScore(): Unit =
    println(s"Score: $score/${wordList.length}")

class FuzzyQuiz(morsePlayer: MorsePlayer, consoleReader: ConsoleReader, wordList: Seq[String])
  extends Quiz(morsePlayer: MorsePlayer, consoleReader: ConsoleReader, wordList: Seq[String]):
  private var fuzzyScore: Double = 0.0

  override def scoreWord(word: String, guessedWord: String): Unit =
    val diff = StringCompareUtility.stringCompareFuzzy(word, guessedWord)
    if (diff == 1.0)
      consoleReader.getOutput.write(s"${Console.GREEN}Correct!${Console.RESET}")
    else if (diff >= 0.75 && diff < 1.0)
      consoleReader.getOutput.write(s"${Console.YELLOW}Close... ($diff)${Console.RESET} -- $word")
    else
      consoleReader.getOutput.write(s"${Console.RED}Wrong... ($diff)${Console.RESET} -- $word")
    fuzzyScore += diff

  override def printScore(): Unit =
    println(s"Score: $fuzzyScore/${wordList.length}")
