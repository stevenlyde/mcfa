package org.ucombinator

object ANormalizerTests {

  def main (args : Array[String]) {

    val sxp = new SExpParser
    val p = new RnRSParser
    val t = new ANormalizer

    if (args.length > 0) {

      val filename = args(0)

      System.err.println("Testing ANormalizer on " + filename)

      val lines = scala.io.Source.fromFile(filename).mkString("")

      // println(lines)

      val sexps = sxp.parseAll(lines)
      
      val ast = p.parseProgram(sexps)

      val anast = t.normalize(ast.toExp)

      println (anast)
    }
    
  }

}
