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
    val firstName = \[String](nonEmptyOrWhiteSpace)
    val lastName = \[String](nonEmptyOrWhiteSpace)
    val orderId = \?[Int](reserved && immutable)

    val email = new \\ {
      val friendlyName = \?[String]
      val address = \[String]
    }
    val status = \?[String](in("pending", "processing", "sent") && reserved)
    val notes = \?[String](internal)

    val orderLines = \:[(String, Int)](forall(custom[(String, Int)](ol => ol._2 >= 0, "Cannot order negative items")))

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
  //experimental convert to postgres jsonb clause
  val postgresQuery = QueryJsonb("data", relatedOrdersQuery)

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
  //apply delta to current state
  val processing = pending.delta(statusDelta)

  //Define subcontract for reusable or recursive structures
  trait UserTimestamp {
    val account = \[String]
    val timestamp = \[Long]
  }
  object Element extends Contract {
    val created = new \\(immutable) with UserTimestamp
    val modified = new \\ with UserTimestamp
  }

  //try to force a match even if wrong type
  import OptimisticCodecs._
  Json("orderId" := "23628") match {
    case Order.orderId(Some(id)) => id
  }

}
