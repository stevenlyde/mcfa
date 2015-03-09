package org.ucombinator

object RnRSParserTests {
  def assert (test : => Boolean) {
    if (!test) {
      throw new Exception("Test failed!")
    }
  }
  

  def main (args : Array[String]) {

    val sxp = new SExpParser
    val p = new RnRSParser

    if (args.length > 0) {

      val filename = args(0)

      System.err.println("Testing RnRSParser on " + filename)

      val lines = scala.io.Source.fromFile(filename).mkString("")

      // println(lines)

      val sexps = sxp.parseAll(lines)
      
      val ast = p.parseProgram(sexps)

      println (ast)
    }

    val prog = "(define (f x) x) (define v (f 10))"

    val sprog = sxp.parseAll(prog)

    val ast = p.parseProgram(sprog)

    // println(ast)

    ()
  }
}
