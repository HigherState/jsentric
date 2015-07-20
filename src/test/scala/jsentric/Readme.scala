package jsentric

class Readme {
  import jsentric._
  import Jsentric._

  /*define a contract,
    /  /?  /! expected, optional, default properties
    /: /:?  /:!  expected, optional, default array properties
    // //? expected, option object properties
   */
  object Order extends Contract {
    val firstName = \[String]("firstName", nonEmptyOrWhiteSpace)
    val lastName = \[String]("lastName", nonEmptyOrWhiteSpace)
    val orderId = \?[Int]("orderId", reserved && immutable)

    val email = new \\("email") {
      val friendlyName = \?[String]("friendlyName")
      val address = \[String]("address")
    }
    val status = \?[String]("status", in("pending", "processing", "sent") && reserved)
    val notes = \?[String]("notes", internal)

    val orderLines = \:[(String, Int)]("orderLines", forall(custom[(String, Int)](ol => ol._2 >= 0, "Cannot order negative items")))

    import Composite._
    //Combine properties to make a composite pattern matcher
    lazy val fullName = firstName @: lastName
  }

  import argonaut._

  //Create a new Json object
  val newOrder = Order.$create{o =>
    o.firstName.$set("John") ~
    o.lastName.$set("Smith") ~
    o.email.address.$set("johnSmith@test.com") ~
    o.orderLines.$append("Socks" -> 3)
  }

  //validate a new json object
  val validated = Order.$validate(newOrder)

  //pattern match property values
  newOrder match {
    case Order.email.address(email) && Order.email.friendlyName(Some(name)) =>
      println(s"$email <$name>")
    case Order.email.address(email) && Order.fullName(firstName, lastName) =>
      println(s"$email <$firstName $lastName>")
  }

  //make changes to the json object.
  val pending =
    Order{o =>
      o.orderId.$set(123) ~
      o.status.$set("pending") ~
      o.notes.$modify(maybe => Some(maybe.foldLeft("Order now pending.")(_ + _)))
    }(newOrder)

  //strip out any properties marked internal
  val sendToClient = Order.$sanitize(pending)

  //generate query json
  val relatedOrdersQuery = Order.orderId.$gt(56) && Order.status.$in("processing", "sent")

  import scalaz.{\/, \/-}
  //create a dynamic property
  val dynamic = Order.$dynamic[\/[String, Int]]("age")

  sendToClient match {
    case dynamic(Some(\/-(ageInt))) =>
      println(ageInt)
    case _ =>
  }

  val statusDelta = Order.$create(_.status.$set("processing"))
  //validate against current state
  Order.$validate(statusDelta, pending)
  val processing = pending.delta(statusDelta)

  //Define subcontract for reusable or recursive structures
  trait UserTimestamp extends SubContract {
    val account = \[String]("account")
    val timestamp = \[Long]("timestamp")
  }
  object Element extends Contract {
    val created = new \\("created", immutable) with UserTimestamp
    val modified = new \\("modified") with UserTimestamp
  }
}
