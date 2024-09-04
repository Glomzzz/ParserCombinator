package com.skillw

import org.scalatest.funsuite.AnyFunSuiteLike
import Parser.*

class ParserTest extends AnyFunSuiteLike {

  test("testPred") {
    pred(_ == 'a').parse("a") match {
      case Ok(value) => assert(value == 'a')
      case e:Err => e.error
    }
  }

  test("testLetter") {
    letter.parse("a") match {
      case Ok(value) => assert(value == 'a')
      case e:Err => e.error
    }
  }

  test("testChar") {
    char('a').parse("a") match {
      case Ok(value) => assert(value == 'a')
      case e:Err => e.error
    }
  }

  test("testId") {
    id().parse("abc") match {
      case Ok(value) => assert(value == 'a')
      case e:Err => e.error
    }
  }

  test("testOk") {
    ok(1).parse("asdas") match {
      case Ok(value) => assertResult(1)(value)
      case e:Err => e.error
    }
  }

  test("testDigit") {
    digit.parse("1231123") match {
      case Ok(value) => assert(value == '1')
      case e:Err => e.error
    }
  }

}
