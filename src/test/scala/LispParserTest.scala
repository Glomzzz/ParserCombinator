package com.skillw

import org.scalatest.funsuite.AnyFunSuiteLike
import LispParser.*
import Expr.*

class LispParserTest extends AnyFunSuiteLike {



  test("testWhitespace") {
    whitespace.parse("  ") match {
      case Ok(value) =>
      case e:Err => e.error
    }
  }

  test("testComment") {
    comment.parse("#comment\n  ") match {
      case Ok(value) =>
      case e:Err => e.error
    }
    (comment thenSkip comment).parse("#comment\n#comment\n  ") match {
      case Ok(value) =>
      case e:Err => e.error
    }
  }

  test("testBlank"){
    blank.parse("  ") match {
      case Ok(value) =>
      case e:Err => e.error
    }
    blank.parse("  #comment\n  ") match {
      case Ok(value) =>
      case e:Err => e.error
    }
    (blank skipThen atom).parse("  #comment\n123123 ") match {
      case Ok(value) => assertResult(Num(123123))(value)
      case e:Err => e.error
    }
  }

  test("testDigits") {
    digits.parse("123") match {
      case Ok(value) => assertResult("123")(value)
      case e:Err => e.error
    }
    digits.parse("0001234  ") match {
      case Ok(value) => assertResult("0001234")(value)
      case e:Err => e.error
    }
    digits.parse(" 0001234  ") match {
      case Ok(value) => assert(false)
      case e:Err =>
    }

  }

  test("testNat") {
    nat.parse("123") match {
      case Ok(value) => assertResult(123)(value)
      case e:Err => e.error
    }
    nat.parse("0001234  ") match {
      case Ok(value) => assertResult(1234)(value)
      case e:Err => e.error
    }
  }

  test("testReal") {
    real.parse("123.123") match {
      case Ok(value) => assertResult(123.123)(value)
      case e:Err => e.error
    }
    real.parse("123.1") match {
      case Ok(value) => assertResult(123.1)(value)
      case e:Err => e.error
    }
    real.parse("123") match {
      case Ok(value) => assertResult(123)(value)
      case e:Err => e.error
    }
    real.parse("000123") match {
      case Ok(value) => assertResult(123)(value)
      case e:Err => e.error
    }
    real.parse("000123.000123") match {
      case Ok(value) => assertResult(123.000123)(value)
      case e:Err => e.error
    }

  }

  test("testName") {
    name.parse("1aBc") match {
      case Ok(value) => assert(false)
      case e:Err =>
    }
    name.parse("!aBc") match {
      case Ok(value) => assertResult("!abc")(value)
      case e:Err => e.error
    }
    (name skipThen blank skipThen name).parse("abc asddas  ") match {
      case Ok(value) => assertResult("asddas")(value)
      case e:Err => e.error
    }

  }

  test("testLeftParen") {
    leftParen.parse("(") match {
      case Ok(value) => assertResult('(')(value)
      case e:Err => e.error
    }
  }

  test("testRightParen") {
    rightParen.parse(")") match {
      case Ok(value) => assertResult(')')(value)
      case e:Err => e.error
    }
  }

  // Program

  test("testNumber") {
    number.parse("123") match {
      case Ok(value) => assertResult(Num(123))(value)
      case e:Err => e.error
    }
  }

  test("testVariable") {
    variable.parse("aBc!") match {
      case Ok(value) => assertResult(Var("abc!"))(value)
      case e:Err => e.error
    }
  }

  test("testCombination") {
    com.parse("(abc 123)") match {
      case Ok(value) => assertResult(Com(Var("abc"), Num(123)))(value)
      case e:Err => e.error
    }
  }

  test("testAtom") {
    atom.parse("123") match {
      case Ok(value) => assertResult(Num(123))(value)
      case e:Err => e.error
    }
    atom.parse("abc") match {
      case Ok(value) => assertResult(Var("abc"))(value)
      case e:Err => e.error
    }
  }

  test("testProgram") {
    program.parse(" (abc 123)") match {
      case Ok(value) => assertResult(List(Com(Var("abc"), Num(123))))(value)
      case e:Err => e.error
    }
    program.parse("abc 123 (abc (abc (abc (abc 123))))  ") match {
      case Ok(value) => assertResult(List(Var("abc"), Num(123.0), Com(Var("abc"), Com(Var("abc"), Com(Var("abc"), Com(Var("abc"), Num(123.0)))))))(value)
      case e:Err => e.error
    }

    val pgm =
      """
        | (define (square x)
        |  (* x x))
        |
        | (a (- 2 1))
        | (b (- 3 1))
        |
        |""".stripMargin

    program.parse(pgm) match {
      case Ok(value) => assertResult(List(Com(Var("define"), Com(Var("square"), Var("x")), Com(Var("*"), Var("x"), Var("x"))), Com(Var("a"), Com(Var("-"), Num(2.0), Num(1.0))), Com(Var("b"), Com(Var("-"), Num(3.0), Num(1.0)))))(value)
      case e:Err =>  e.error
    }
  }

}

