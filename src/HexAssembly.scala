import ParserLib._

object HexAssembly {

  def main(a:Array[String]) {
    println( expr("2"))
    println( expr("2+((5-1))"))
    println( expr("2+(5-1)boo"))
    println(program("hello"))
    println(program("hello.hello.bye.hello"))
  }
  
  val op = (string("+") >> produce[(Int, Int)=>Int](_+_)) <|> (string("-") >> produce[(Int, Int)=>Int](_-_))
  val num: Parser[Int] = (digit <::> many(digit)) >>= ((is)=> produce(is.fold(0)((a,b)=>a*10 + b)))
  val value: Parser[Int] = (num <|> (char('(') >> expr << char(')')))
  val expr: Parser[Int] = (value >>= ((a)=> op >>= ((b)=> value >>= ((c)=> produce(b(a,c)))))) <|> value
  
  val statement: Parser[String] = string2Obj("hello",":-)") <|> string2Obj("bye",":-(")
  val statements: Parser[List[String]] = statement <::> many(char('.') >> statement)
  val program: Parser[List[String]] = statements << end
}