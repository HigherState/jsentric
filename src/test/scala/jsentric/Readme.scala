package jsentric

class Readme {
  import jsentric._
  import Jsentric._

  object Order extends Contract {
    val firstName = \[String]("firstName", nonEmptyOrWhiteSpace)
    val lastName = \[String]("lastName", nonEmptyOrWhiteSpace)
    val orderId = \?[Int]("orderId", reserved)

    val email = new \\("email") {
      val friendlyName = \?[String]("friendlyName")
      val address = \[String]("address")
    }
    val status = \?[String]("status", in("pending", "processing", "sent") && reserved)
    val notes = \?[String]("notes", internal)

    import JComposite._
    lazy val fullName = firstName @: lastName
  }



  import argonaut._

  val newOrder = Order.$create{o =>
    o.firstName.$set("John") ~
      o.lastName.$set("Smith") ~
      o.email.address.$set("johnSmith@test.com")
  }

  val validated = Order.$validate(newOrder)

  newOrder match {
    case Order.email.address(email) && Order.email.friendlyName(Some(name)) =>
      println(s"$email <$name>")
    case Order.email.address(email) && Order.fullName(firstName, lastName) =>
      println(s"$email <$firstName $lastName>")
  }

  val processing =
    Order{o =>
      o.orderId.$set(123) ~
        o.status.$set("pending") ~
        o.notes.$modify(maybe => Some(maybe.foldLeft("Order now pending.")(_ + _)))
    }(newOrder)

  val sendToClient = Order.$sanitize(processing)

  val relatedOrdersQuery = Order.lastName.$eq("Smith") && Order.status.$in("processing", "sent")

  import scalaz.\/
  val dynamic = Order.$dynamic[\/[String, Int]]("age")

  sendToClient match {
    case dynamic(Some(\/-(ageInt))) =>
      println(age)
    case _ =>
  }
}
