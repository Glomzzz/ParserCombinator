package com.skillw



@main
def main(): Unit = {
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
}