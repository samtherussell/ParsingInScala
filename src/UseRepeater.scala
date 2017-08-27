
object UseRepeater {
  
  implicit class Repeater(var x: Int) {
    def times [A](f: =>A) {
      while (x>0) {
        f
        x -= 1
      }
    }
  }
  
  def main(a: Array[String]) {
    10 times println("hi")
  }
}