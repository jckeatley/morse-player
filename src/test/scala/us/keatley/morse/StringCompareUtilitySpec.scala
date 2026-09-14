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
