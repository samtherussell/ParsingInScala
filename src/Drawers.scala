trait Drawable {
    def draw() { }
}
 
trait Cowboy extends Drawable {
    override def draw() { println("Bang!") }
}
 
trait Artist extends Drawable {
    override def draw() { println("A pretty painting") }
}

class CowboyArtist extends Cowboy with Artist
 
trait Pupil {
  def doHomework() {
    println("actually, I'm just gonna go to sleep")
  }
}

object Drawers{
  def main(args: Array[String]) {
    var tmp = new CowboyArtist()
    tmp.draw()
    
    var tmp1 = new Object() with Pupil
    var tmp2 = new Object()
    println(s"tmp1.isInstanceOf[Object with Pupil] = ${tmp1.isInstanceOf[Object with Pupil]}")
    println(s"tmp2.isInstanceOf[Object with Pupil] = ${tmp2.isInstanceOf[Object with Pupil]}")
  }
}