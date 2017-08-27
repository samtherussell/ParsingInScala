/*
import ParserLib._

object Parser {
  def main(args: Array[String]) {
    {
      val p = char('h') >>= ( (c: Char) => char('i') >>= ( (d: Char) => produce("hi")) )
      val result = p("hi bob")
      println(result(0))
    }
    {
      val p = string("hell")
      val result = p("hello my name is sam")
      println(result(0))
    }
    {
      val p = char('h') <|> char('a')
      val result = p("hi")
      println(result.mkString(","))
    }
    {
      val p = any( List(char('h'), char('a'), char('l')) )
      val result = p("lol")
      println(result.mkString(","))
    }
    {
      val p = digit
      val result = p("0h")
      println(result.mkString(","))
    }
    {
      val p = many(char('a'))
      val result = p("aaa")
      println(result.mkString(","))
    }/*
    {
      val p = many(char('a')) >> char('b')
      val result = p("aaaaaaaaab")
      println(result.mkString(","))
    }
    {
      val p = char('c') <::> many(char('a') <|> char('b')) << end
      val result = p("cabba")
      println(result.mkString(","))
    }*/
  }
}



object ParserLib {

  trait Parser[A] extends Function[String, List[(A, String)]] {
    
    def >>=[B](second: A=>Parser[B]): Parser[B] = {
      lazy val secondl = second
      (string: String) => {
        apply(string).flatMap(
          (result) => {
            val (value, leftover) = result
            secondl(value)(leftover)
          }
        ) 
      }
    }
    
    def >>[B](second: Parser[B]): Parser[B] =
      this >>= ((a)=> second )
    
    def <<[B](second: Parser[B]): Parser[A] =
      this >>= ((a)=> second >>= ((b)=> produce(a)))
    
    def <|>(second: Parser[A]): Parser[A] = {
      lazy val secondl = second
      (string: String) => {
        val ret1 = apply(string)
        ret1 match {
          case Nil => secondl(string)
          case xs => xs
        }
      }
    }
    
    def <::>(second: Parser[List[A]]): Parser[List[A]] = {
      lazy val secondl = {println("here"); second}
      this >>= ((a: A) => {/*println("hi");*/ secondl >>= ((as: List[A]) => produce(a :: as))})
    }
  }
  
  def concat(ps: List[Parser[String]]): Parser[String] = ps match {
    case Nil => fail() 
    case x :: Nil => x
    case x :: xs => x >>= ((a) => concat(xs) >>= ((as)=> produce(a + as)))
  }
  
  def produce[A](a: A): Parser[A] = {
    (string: String) => {
      List((a,string))
    }
  }
  def char(c: Char): Parser[Char] = (string: String) => {
    if (string.length() > 0 && string(0) == c) {
      List((string(0), string.substring(1)))
    } else {
      List()
    }
  }
  
  def string(input: String): Parser[String] =
    if(input.length() == 1) {
      (char(input(0)) >>= ((c: Char) => produce(c.toString())))
    } else {
      (char(input(0)) >>= ( (c: Char) => string(input.substring(1)) >>= ((s: String) => produce(c.toString() + s) )))
    }
  
  def int(i: Int): Parser[Int] = char((i+'0').toChar) >>= ( (c: Char) => produce((c-'0').toInt))
  
  def any[A](input: List[Parser[A]]): Parser[A] = input match {
    case Nil => fail()
    case x :: Nil => x
    case x :: xs => x <|> any(xs)
  }
  
  val digit: Parser[Int] = any( List(0,1,2,3,4,5,6,7,8,9).map(int) )
  
  def many[A](p: Parser[A]): Parser[List[A]] = {
    lazy val p1: Parser[List[A]] = {println("many"); many1(p)}
    produce(List[A]()) <|> p1
  }
  
  def many1[A](p: Parser[A]): Parser[List[A]] = {
    lazy val p1 = {println("many1"); many(p)}
    p <::> p1
  }
    
  def fail[A](): Parser[A] = (string: String) => List()
  
  def end(): Parser[String] = (string: String) => if(string.length() == 0) List(("","")) else List()
}*/