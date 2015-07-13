import scalaz.{NonEmptyList, \/}
import argonaut._
import Argonaut._

package object jsentric {

  type Segments = Vector[Either[String, Int]]
  type JValid = \/[NonEmptyList[(String, Path)], Json]

  type JNumeric = Long with Int with Float with Double with Option[Long] with Option[Int] with Option[Float] with Option[Double]
  type JLength = String with Seq[Nothing]
  type JOptionable[T] = T with Option[T]

  implicit def stringToPath(s:String) = Path(s)
}

