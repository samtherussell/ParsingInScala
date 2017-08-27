

class Point(xc: Int, yc:Int) {
  var x: Int = xc
  var y: Int = yc
  
  def move(dx: Int, dy: Int) {
    x += dx
    y += dy
  }
}

class Location(xc: Int, yc: Int, nameIn: String) extends Point(xc, yc+10) {
  var name=nameIn
}

object Demo {
  def main(args: Array[String]) {
    var point = new Point(0,0)
    point.move(1, -4)
    println(point.x)
    
    var loc = new Location(1,4,"beach")
    println(loc.y)
  }
}