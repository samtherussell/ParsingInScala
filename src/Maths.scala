

object Maths {
  def fact(n: Int) : Int = if (n==0) { 1 } else {n * fact(n-1)} 
  def fact1(n: Int, acc: Int) : Int = if (n==0) { acc } else {fact1(n-1, n * acc)}
}

object Test {
  
  def memo(f: Int=>Int) : Int=>Int = {
 
    var cache = Map[Int,Int]()
    def apply(i:Int) = if (cache contains i) {
                           println("cache hit for "+i)
                           cache(i)
                       } else {
                           val ret = f(i)
                           cache += (i -> ret)
                           ret
                       }
    apply
  }
 
  val fib: Int=>Int = memo( (n: Int) => {
    n match {
      case 0 => 0
      case 1 => 1
      case n => fib(n-1) + fib(n-2)
    }
  })
 
  def main(a: Array[String]) {
    lazy val x = { println ("foo") ; 10 }
    println ("bar")
    println (x)
    println (x)
  }
  
}