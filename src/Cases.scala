
object ListTest {
  trait Lister
  object Nil extends Lister
  case class ::[A](head: A, tail: Lister) extends Lister
}

object Cases {
  def main(args: Array[String]) {
    var tmp = ::[Int](1,Nil)
  }
}