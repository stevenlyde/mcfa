package org.ucombinator

/* RnRS primitives */
object RnRSPrimitives {

  def list = List("*","-","+","/",
                  "quotient","gcd","modulo","log",
                  "ceiling",
                  "<","=",">","<=",">=",
                  "odd?","even?","char?","symbol?","list?","pair?","null?","integer?","number?","boolean?","procedure?","string?",
                  "char-alphabetic?","char-numeric?","string<?",
                  "eq?","equal?","eqv?","char=?",
                  "string-ref","string-length", "string-append","number->string","list->string","symbol->string",
                  "string->symbol",
                  "char->integer",
                  "not",
                  "length",
                  "cons","car","cdr",
                  "newline","display",
                  "random",
                  "error")

}
