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

import scala.io.Source
import scala.util.Random
import java.io.BufferedInputStream
import java.util.MissingResourceException
import java.util.zip.GZIPInputStream

trait WordGenerator:
  def genWords(count: Int): Seq[String]

class DictWordGenerator(path: String, maxLength: Option[Int], minLength: Option[Int]) extends WordGenerator:
  private lazy val dictionary: IndexedSeq[String] = loadDictionary()
  private lazy val random = new Random

  override def genWords(count: Int): Seq[String] =
    (1 to count).map(_ => selectWord())

  /**
   * Loads the dictionary resource.
   */
  private def loadDictionary(): IndexedSeq[String] =
    val is = getClass.getClassLoader.getResourceAsStream(path)
    if (is == null)
      throw new MissingResourceException(s"Cannot load resource: $path", getClass.getName, path)
    val src = Source.fromInputStream(new GZIPInputStream(new BufferedInputStream(is)))
    val words = src.getLines().filter(w => minLength.forall(w.length >= _) && maxLength.forall(w.length <= _))
    words.toIndexedSeq

  private def selectWord(): String =
    val index = random.nextInt(dictionary.length)
    dictionary(index)

class RandomWordGenerator(maxLength: Int, minLength: Int, letters: Boolean, numbers: Boolean, punctuation: Boolean)
    extends WordGenerator:
  private lazy val chars: Seq[Char] = genCharList()
  private lazy val random = new Random

  override def genWords(count: Int): Seq[String] =
    (1 to count).map(_ => genWord())

  private def genCharList(): Seq[Char] =
    (if (letters || !(letters || numbers || punctuation)) ('A' to 'Z') else Seq.empty) ++
    (if (numbers) ('0' to '9') else Seq.empty) ++
    (if (punctuation) Seq('.', ',', '?', '\'', '!', '/', '(', ')', '&',
                          ':', ';', '=', '+', '-', '_', '\"', '$', '@') else Seq.empty)

  private def genWord(): String =
    val length = random.between(minLength, maxLength + 1)
    val w =
      for
        n <- 1 to length
      yield
        chars(random.nextInt(chars.length))
    w.mkString
