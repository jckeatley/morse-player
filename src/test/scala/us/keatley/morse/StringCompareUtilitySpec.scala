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

import org.scalatest.flatspec.AnyFlatSpec

class StringCompareUtilitySpec extends AnyFlatSpec:
  "StringCompareUtility" should "return 0" in:
    assert(StringCompareUtility.damerauLevenshtein("", "") == 0)
    assert(StringCompareUtility.damerauLevenshtein("abcdefghijklmnopqrstuvwxyz", "abcdefghijklmnopqrstuvwxyz") == 0)

  "StringCompareUtility" should "return 1" in :
    assert(StringCompareUtility.damerauLevenshtein(" ", "") == 1)
    assert(StringCompareUtility.damerauLevenshtein("abcdefghijkl mnopqrstuvwxyz", "abcdefghijklmnopqrstuvwxyz") == 1)
    assert(StringCompareUtility.damerauLevenshtein("abcdefghijklmnopqrtsuvwxyz", "abcdefghijklmnopqrstuvwxyz") == 1)

  "StringCompareUtility" should "return 3" in:
      assert(StringCompareUtility.damerauLevenshtein("bacdefghijklmnopqrtsuwvxyz", "abcdefghijklmnopqrstuvwxyz") == 3)
