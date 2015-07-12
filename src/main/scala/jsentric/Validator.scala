package jsentric

import argonaut._
import Argonaut._

trait Validator[+T] {

  def validate(value:Option[Json], currentState:Option[Json], path:Path):Seq[(String, Path)]

  def &&[S >: T] (v:Validator[S]):Validator[S] = AndValidator(this, v)

  def ||[S >: T] (v:Validator[S]):Validator[S] = OrValidator(this, v)

  def schema:Json
}

case class AndValidator[T, A >: T, B >: T](left:Validator[A], right:Validator[B]) extends Validator[T] {
  def validate(value:Option[Json], currentState:Option[Json], path:Path):Seq[(String, Path)] =
    left.validate(value, currentState, path:Path) ++ right.validate(value, currentState, path:Path)

  def schema =
    left.schema deepmerge right.schema
}

case class OrValidator[T, A >: T, B >: T](left:Validator[A], right:Validator[B]) extends Validator[T] {
  def validate(value:Option[Json], currentState:Option[Json], path:Path):Seq[(String, Path)] ={
    left.validate(value, currentState, path:Path) match {
      case Seq() => Seq()
      case list => right.validate(value, currentState, path:Path) match {
        case Seq() => Seq()
        case list2 if list2.size < list.size => list2
        case _ => list
      }
    }
  }
  def schema =
    ("or" -> (left.schema -->>: right.schema -->>: jEmptyArray)) ->: jEmptyObject
}

case object EmptyValidator extends Validator[Nothing] {
  def validate(value: Option[Json], currentState: Option[Json], path: Path): Seq[(String, Path)] =
    Nil

  def schema =
    jEmptyObject
}

case object Internal extends SimpleValidator[Option[Nothing]]  {
  def maybeValid(path:Path) = {
    case (Some(_), _) =>
      "Value is reserved and cannot be provided." -> path
  }
  def schema  =
    Json("reserved" -> jTrue)
}

trait SimpleValidator[+T] extends Validator[T] {
  def maybeValid(path:Path):PartialFunction[(Option[Json],Option[Json]), (String, Path)]

  def validate(value: Option[Json], currentState: Option[Json], path:Path): Seq[(String, Path)] =
    maybeValid(path).lift(value -> currentState).toSeq
}

trait JsonValidators {
  import Path._

  val immutable = new SimpleValidator[Nothing] {
    def maybeValid(path:Path) = {
      case (Some(a), Some(b)) if a != b =>
        "value is immutable and cannot be changed." -> path
    }
    def schema =
      Json("immutable" -> jTrue)
  }

  val notNull = new SimpleValidator[Option[Nothing]] {
    def maybeValid(path: Path) = {
      case (Some(j), _) if j.isNull =>
        "Value cannot be null." -> path
    }

    def schema = Json("notNull" -> jTrue)
  }

  val reserved = new SimpleValidator[Option[Nothing]]  {
    def maybeValid(path:Path) = {
      case (Some(_), _) =>
        "Value is reserved and cannot be provided." -> path
    }
    def schema = Json("reserved" -> jTrue)
  }

  val internal = Internal

  sealed trait BoundedValidator extends SimpleValidator[JNumeric] {
    def doubleFail(n: Double): Boolean

    def message(n: Number): String

    def maybeValid(path: Path) = {
      case (Some(n), _) if n.isNumber && doubleFail(n.number.get) =>
        message(n.number.get) -> path
    }
  }

  def >[T](value: Long) = new BoundedValidator {
    def doubleFail(n: Double): Boolean = n <= value

    def message(n: Number): String = s"Value $n is not greater than $value"

    def schema = Json("greaterThan" := value)
  }

  def >[T](value: Double):Validator[JNumeric] = new BoundedValidator {
    def doubleFail(n: Double): Boolean = n <= value

    def message(n: Number): String = s"Value $n is not greater than $value"

    def schema = Json("greaterThan" := value)
  }

  def >=[T](value: Long):Validator[JNumeric] = new BoundedValidator {
    def doubleFail(n: Double): Boolean = n < value

    def message(n: Number): String = s"Value $n is not greater than or equal to $value"

    def schema = Json("greaterThanEquals" := value)
  }

  def >=[T](value: Double) = new BoundedValidator {
    def doubleFail(n: Double): Boolean = n < value

    def message(n: Number): String = s"Value $n is not greater than or equal to $value"

    def schema = Json("greaterThanEquals" := value)
  }

  def <[T](value: Long) = new BoundedValidator {
    def doubleFail(n: Double): Boolean = n >= value

    def message(n: Number): String = s"Value $n is not less than $value"

    def schema = Json("lessThan" := value)
  }

  def <[T](value: Double) = new BoundedValidator {
    def doubleFail(n: Double): Boolean = n <= value

    def message(n: Number): String = s"Value $n is not less than $value"

    def schema = Json("lessThan" := value)
  }

  def <=[T](value: Long) = new BoundedValidator {
    def doubleFail(n: Double): Boolean = n > value

    def message(n: Number): String = s"Value $n is not less than or equal to $value"

    def schema = Json("lessThanEquals" := value)
  }

  def <=[T](value: Double) = new BoundedValidator {
    def doubleFail(n: Double): Boolean = n > value

    def message(n: Number): String = s"Value $n is not greater than or equal to $value"

    def schema = Json("lessThanEquals" := value)
  }

  def in[T](values:T*)(implicit codec: CodecJson[T]) = new SimpleValidator[JOptionable[T]] {
    def schema = Json("is in" := values.map(codec.apply).toList)

    def maybeValid(path: Path): PartialFunction[(Option[Json], Option[Json]), (String, Path)] = {
      case (Some(j), _) if !codec.decodeJson(j).toOption.exists(values.contains) =>
        "Value outside of allowed values." -> path
    }
  }

  def nin[T](values:T*)(implicit codec: CodecJson[T]) = new SimpleValidator[JOptionable[T]] {
    def schema = Json("notIn" := values.map(codec.apply).toList)

    def maybeValid(path: Path): PartialFunction[(Option[Json], Option[Json]), (String, Path)] = {
      case (Some(j), _) if codec.decodeJson(j).toOption.exists(values.contains) =>
        "Value not allowed." -> path
    }
  }

  def inCaseInsensitive(values:String*) = new SimpleValidator[JOptionable[String]] {
    def schema = Json("is in" := values.map(jString).toList, "caseInsensitive" -> jTrue)

    def maybeValid(path: Path): PartialFunction[(Option[Json], Option[Json]), (String, Path)] = {
      case (Some(JString(s)), _) if !values.exists(_.equalsIgnoreCase(s)) =>
        "Value outside of allowed values." -> path
    }
  }

  def ninCaseInsensitive(values:String*) = new SimpleValidator[JOptionable[String]] {
    def schema = Json("not in" := values.map(jString).toList, "caseInsensitive" -> jTrue)

    def maybeValid(path: Path): PartialFunction[(Option[Json], Option[Json]), (String, Path)] = {
      case (Some(JString(s)), _) if values.exists(_.equalsIgnoreCase(s)) =>
        "Value outside of allowed values." -> path
    }
  }

  def minLength(value: Int) = new SimpleValidator[JOptionable[JLength]] {
    def maybeValid(path: Path) = {
      case (Some(JArray(seq)), _) if seq.length < value =>
        s"Array must have length of at least $value" -> path
      case (Some(JString(s)), _) if s.length < value =>
        s"String must have length of at least $value" -> path
    }

    def schema = Json("minLength" := value)
  }

  def maxLength(value: Int) = new SimpleValidator[JOptionable[JLength]] {
    def maybeValid(path: Path) = {
      case (Some(JArray(seq)), _) if seq.length > value =>
        s"Array must have length of no greater than $value" -> path
      case (Some(JString(s)), _) if s.length > value =>
        s"String must have length of no greater than $value" -> path
    }

    def schema = Json("maxLength" := value.j)
  }

  val nonEmpty = new SimpleValidator[JOptionable[JLength]] {
    def maybeValid(path: Path) = {
      case (Some(JArray(seq)), _) if seq.isEmpty =>
        s"Array must not be empty" -> path
      case (Some(JString(s)), _) if s.isEmpty =>
        s"String must not be empty" -> path
    }

    def schema = Json("nonEmpty" -> jTrue)
  }

  val nonEmptyOrWhiteSpace:Validator[String] = new SimpleValidator[JLength] {
    def maybeValid(path: Path) = {
      case (Some(JString(text)), _) if text.trim().isEmpty =>
        s"Text must not be all empty or whitespace" -> path
    }

    def schema = Json("nonEmptyOrWhitespace" -> jTrue)
  }

  def forall[T](validator: Validator[T]) = new Validator[JOptionable[Seq[Nothing]]] {
    def validate(value: Option[Json], currentState: Option[Json], pathContext: Path): Seq[(String, Path)] =
      value collect {
        case JArray(seq) =>
          for {
            (e, i) <- seq.zipWithIndex
            v <- validator.validate(Some(e), None, pathContext \ i)
          } yield v
      } getOrElse Seq.empty

    def schema = Json("items" -> validator.schema)
  }

  //TODO Forall doesnt validate agains current state, bit of an odd one..
  def forall(contract: BaseContract) = new Validator[JOptionable[Seq[Nothing]]] {
    def validate(value: Option[Json], currentState: Option[Json], pathContext: Path): Seq[(String, Path)] =
      value collect {
        case JArray(seq) =>
          for {
            (e, i) <- seq.zipWithIndex
            v <- new BaseContractValidation(contract).$validate(e, None, pathContext \ i)
          } yield v
      } getOrElse Seq.empty

    def schema = jEmptyObject //("items" -> contract.schema)
  }

  def values(contract:BaseContract) = new Validator[JOptionable[Map[String,Nothing]]] {
    def validate(value: Option[Json], currentState: Option[Json], path: Path): Seq[(String, Path)] =
      value collect {
        case JObject(map) =>
          map.toMap.flatMap{ kv =>
            val current = currentState \ kv._1
            new BaseContractValidation(contract).$validate(kv._2, current, path \ kv._1)
          }.toSeq
      } getOrElse Seq.empty[(String, Path)]

    def schema = jEmptyObject
  }
}

object DefaultValidators extends JsonValidators
