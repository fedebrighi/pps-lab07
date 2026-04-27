package ex1

import ex1.Parsers.charParser
import org.scalatest.matchers.should.Matchers.{shouldBe, *}


class ScalaTestParser extends org.scalatest.flatspec.AnyFlatSpec:
  "A Basic Parser" should "parse only the given characters" in:
    def parser = new BasicParser(Set('a', 'b', 'c'))
    parser.parseAll("aabc".toList) shouldBe true
    parser.parseAll("aabcdc".toList) shouldBe false
    parser.parseAll("".toList) shouldBe true

  "A NotEmpty Parser" should "parse only non-empty characters" in:
    def parserNE = new NonEmptyParser(Set('0', '1'))
    parserNE.parseAll("0101".toList) shouldBe true
    parserNE.parseAll("0123".toList) shouldBe false
    parserNE.parseAll(List()) shouldBe false

  "A NotTwoConsecutiveParser" should "parse only characters that are not consecutive" in:
    def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
    parserNTC.parseAll("XYZ".toList) shouldBe true
    parserNTC.parseAll("XYYZ".toList) shouldBe false
    parserNTC.parseAll("".toList) shouldBe true

  "A NotEmptyAndNotTwoConsecutiveParser" should "parse only characters that are not consecutive and non-empty" in :
    def parserNTCNE = new BasicParser(Set('X', 'Y', 'Z')) with NotTwoConsecutive[Char] with NonEmpty[Char]
    parserNTCNE.parseAll("XYZ".toList) shouldBe true
    parserNTCNE.parseAll("XYYZ".toList) shouldBe false
    parserNTCNE.parseAll("".toList) shouldBe false

  "A String Parser" should "parse only characters that are present in the given string" in:
    def sparser: Parser[Char] = "abc".charParser()
    sparser.parseAll("aabc".toList) shouldBe true
    sparser.parseAll("aabcdc".toList) shouldBe false
    sparser.parseAll("".toList) shouldBe true

