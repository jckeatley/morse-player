/*
 * Copyright (C) 2026 Jonathan Keatley
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

object StringCompareUtility:
  def damerauLevenshtein(a: String, b: String): Int =
    val aLen = a.length
    val bLen = b.length
    if (aLen == 0) return bLen
    if (bLen == 0) return aLen

    val da = new Array[Int](Character.MAX_VALUE + 1)  // last seen index of each char

    // d(i,j) = distance using first i chars of a and first j of b
    val d = Array.ofDim[Int](aLen + 1, bLen + 1)

    // Initialize
    for (i <- 0 to aLen) d(i)(0) = i
    for (j <- 0 to bLen) d(0)(j) = j

    var db = 0

    for
      i <- 1 to aLen
    do
      var lastB = 0
      val chA = a(i - 1)

      for
        j <- 1 to bLen
      do
        val chB = b(j - 1)
        val lastAForChB = da(chB.toInt)
        val cost = if (chA == chB) 0 else 1

        if (chA == chB) lastB = j

        val transposition =
          if (lastAForChB > 0 && lastB > 0)
            d(lastAForChB - 1)(lastB - 1) + (i - lastAForChB - 1) + cost + (j - lastB - 1)
          else
            Int.MaxValue / 2  // large value so it's never chosen unless valid

        d(i)(j) = Seq(
          d(i-1)(j-1) + cost,     // substitute / match
          d(i  )(j-1) + 1,        // insert
          d(i-1)(j  ) + 1,        // delete
          transposition
        ).min
      end for

      da(chA.toInt) = i
      db = lastB
    end for

    d(aLen)(bLen)

  def stringCompareFuzzy(a: String, b: String): Double =
    (a.length - damerauLevenshtein(a, b))/a.length.toDouble
