import ParserLib._

object HexAssembly {

  def main(a:Array[String]) {
    println( expr("2"))
    println( expr("2+((5-1))"))
    println( expr("2+(5-1)boo"))
    println(program("hello"))
    println(program("hello.hello.bye.hello"))
  }
  
  type Op = (Int, Int)=>Int
  val op: Parser[Op] = (string2Obj[Op]("+",_+_)) <|> (string2Obj[Op]("-",_-_))
  val num: Parser[Int] = for (is <- digit <::> many(digit)) yield is.fold(0)((a,b)=>a*10 + b)
  val value: Parser[Int] = (num <|> (char('(') >> expr << char(')')))
  val expr: Parser[Int] = (for( a <- value; b <- op; c <- value) yield b(a,c)) <|> value 
  
  val statement: Parser[String] = string2Obj("hello",":-)") <|> string2Obj("bye",":-(")
  val statements: Parser[List[String]] = statement <::> many(char('.') >> statement)
  val program: Parser[List[String]] = statements << end
}
