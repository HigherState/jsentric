# jsentric
Json contract patterns, validation, lenses and query

jsentric is built upon [argonaut][] and is designed to facilitate the use of the basic json datatypes in cases where we have partially dynamic data or are regularly moving through bounded context and may not wish to constantly serialize/deserialize.

jsentric works by describing a singleton contract which represents data we might wish to extract from the json data structure.  By doing so, we get easy validation, lenses and even a type safe mongo db query generator.

```scala
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

```

*Auto generation of schema information is still a work in progress
*mongo query is not a full feature set.

[argonaut]: http://argonaut.io/
