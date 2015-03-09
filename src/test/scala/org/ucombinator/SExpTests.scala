package org.ucombinator

object SExpTests {

  def assert (test : => Boolean) {
    if (!test) {
      throw new Exception("Test failed!")
    }
  }

  def main (args : Array[String]) {

    import CommonSSymbols._ ;

    val p = new SExpParser

    if (args.length > 0) {

      val filename = args(0)

      // println("Testing s-expression parser on " + filename)

      val lines = scala.io.Source.fromFile(filename).mkString("")

      println(lines)

      val sexps = p.parseAll(lines)
      
      println(sexps) 
    }


    assert (p.parse(";; Math routines \n (foo)").toString
            ==
            "(foo)") ;

    assert (p.parse("3").toString
            ==
            "3") ;

    assert (p.parse("()").toString
            ==
            "()") ;

    assert (p.parse("foo").toString
            ==
            "foo") ;

    assert (p.parse("(3)").toString
            == 
            "(3)") ;

    assert (p.parse("( foo bar\n\n\t baz)").toString
            == 
            "(foo bar baz)") ;

    assert (p.parse("foo ;bar").toString
            == 
            "foo") ;

    assert (p.parse("(foo ;bar\n\n baz)").toString
            == 
            "(foo baz)") ;

    assert (p.parse("(foo )").toString
            == 
            "(foo)") ;

    assert (p.parse("(lambda)") match {
      case SList(SLet) => false
      case SList(SLambda) => true
    })

    
    //println("parseAll test:")
    //println(p.parseAll(";; Math routines \n (foo) (bar)").toString)

    assert (p.parseAll(";; Math routines \n ; test \n (foo) (bar) ; baz\n").toString
            ==
            "List((foo), (bar))") ;
            
  }

}
