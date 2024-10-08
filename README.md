# ParserCombinator

A simple parser combinator library for Scala3, with error reporting.

## For Example

A Lisp parser:

```scala 3
package com.skillw

import Parser.*
import Expr.*

enum Expr{
  case Var(name: String)
  case Num(value: Double)
  case Com(exprs: Expr*)
}


//noinspection TypeAnnotation,ScalaWeakerAccess
object LispParser{
  def whitespace = pred(_.isWhitespace,"whitespace")
  def comment = char('#') skipThen pred(_ != '\n').many thenSkip char('\n')
  def blank = (comment or whitespace).many

  private def nameGen(without:Char => Boolean): Parser[Char] = pred({
    case '(' | ')' => false
    case c if without(c) => false
    case _ => true
  },"name")

  private def nameHead = nameGen(c => c.isWhitespace || c.isDigit)
  private def namePart = nameGen(_.isWhitespace)

  def name = (nameHead and namePart.many) ((head,part) => (head :: part).mkString.toLowerCase)

  def nat = digit.some.map(_.mkString.toInt)
  def digits = digit.some.map(_.mkString)

  def real = nat.ifExpect (char('.')) (digits)((fixed,decimal) => fixed + s"0.$decimal".toDouble)(ok(0)) ((fixed,_) => fixed.toDouble)

  def leftParen = char('(')
  def rightParen = blank skipThen char(')')

  // Program

  def variable = name.map(Var.apply)
  def number = real.map(Num.apply)

  def atom:Parser[Expr]  = variable or number or Parser(s => com.run(s))

  def com = (leftParen skipThen (blank skipThen atom).many thenSkip rightParen ).map(Com.apply)

  def program = atom.all(blank)
}
```

If parse failed, it will show a `ParseError`:

```scala 3
  val input =
    """
      | (define (square x)
      |  (* x x)
      |
      | (a (- 2 1))
      | (b (- 3 1))
      |
      |""".stripMargin
  LispParser.program.parse(input) match {
    case Ok(value) => println(value)
    case e:Err => e.error
  }
```

Such as:

```
Exception in thread "main" java.lang.Exception: 
  Error occurred : Expected ) but got end of input at line 7 , 0:

       (b (- 3 1))
      
      ^
      

    
	at com.skillw.Err.error(parser_combinator.scala:41)
	at com.skillw.main$package$.main(main.scala:18)
	at com.skillw.main.main(main.scala:5)
```