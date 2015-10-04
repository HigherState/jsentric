package jsentric


case class Unnamed(var name:String = "")

trait Setter extends App {


}

object Test extends Setter {

  val me = Unnamed("")
  val you = Unnamed("")


}

object Runner {
  def main(args:Array[String]): Unit = {
    println(Test.me)
  }
}
