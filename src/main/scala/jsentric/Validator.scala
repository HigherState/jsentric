package jsentric

import argonaut._
import Argonaut._
import scalaz.Scalaz._

trait Validator[+T] {

  def validate(value:Option[Json], currentState:Option[Json], path:Path):Seq[(String, Path)]

  def &&[S >: T] (v:Validator[S]):Validator[S] = AndValidator(this, v)

  def ||[S >: T] (v:Validator[S]):Validator[S] = OrValidator(this, v)
}

case class AndValidator[T, A >: T, B >: T](left:Validator[A], right:Validator[B]) extends Validator[T] {
  def validate(value:Option[Json], currentState:Option[Json], path:Path):Seq[(String, Path)] =
    left.validate(value, currentState, path:Path) ++ right.validate(value, currentState, path:Path)
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
}

case object EmptyValidator extends Validator[Nothing] {
  def validate(value: Option[Json], currentState: Option[Json], path: Path): Seq[(String, Path)] =
    Nil
}

case object Internal extends SimpleValidator[Option[Nothing]]  {
  def maybeValid(path:Path) = {
    case (Some(_), _) =>
      "Value is reserved and cannot be provided." -> path
  }
}

trait SimpleValidator[+T] extends Validator[T] {
  def maybeValid(path:Path):PartialFunction[(Option[Json],Option[Json]), (String, Path)]

  def validate(value: Option[Json], currentState: Option[Json], path:Path): Seq[(String, Path)] =
    maybeValid(path).lift(value -> currentState).toSeq
}

trait Validators {
  import Path._

  val immutable = new SimpleValidator[Nothing] {
    def maybeValid(path:Path) = {
      case (Some(a), Some(b)) if a != b =>
        "value is immutable and cannot be changed." -> path
    }
  }

  val notNull = new SimpleValidator[Option[Nothing]] {
    def maybeValid(path: Path) = {
      case (Some(j), _) if j.isNull =>
        "Value cannot be null." -> path
    }
  }

  val reserved = new SimpleValidator[Option[Nothing]]  {
    def maybeValid(path:Path) = {
      case (Some(_), _) =>
        "Value is reserved and cannot be provided." -> path
    }
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
  }

  def >[T](value: Double):Validator[JNumeric] = new BoundedValidator {
    def doubleFail(n: Double): Boolean = n <= value

    def message(n: Number): String = s"Value $n is not greater than $value"
  }

  def >=[T](value: Long):Validator[JNumeric] = new BoundedValidator {
    def doubleFail(n: Double): Boolean = n < value

    def message(n: Number): String = s"Value $n is not greater than or equal to $value"
  }

  def >=[T](value: Double) = new BoundedValidator {
    def doubleFail(n: Double): Boolean = n < value

    def message(n: Number): String = s"Value $n is not greater than or equal to $value"
  }

  def <[T](value: Long) = new BoundedValidator {
    def doubleFail(n: Double): Boolean = n >= value

    def message(n: Number): String = s"Value $n is not less than $value"
  }

  def <[T](value: Double) = new BoundedValidator {
    def doubleFail(n: Double): Boolean = n <= value

    def message(n: Number): String = s"Value $n is not less than $value"
  }

  def <=[T](value: Long) = new BoundedValidator {
    def doubleFail(n: Double): Boolean = n > value

    def message(n: Number): String = s"Value $n is not less than or equal to $value"
  }

  def <=[T](value: Double) = new BoundedValidator {
    def doubleFail(n: Double): Boolean = n > value

    def message(n: Number): String = s"Value $n is not greater than or equal to $value"
  }

  def in[T](values:T*)(implicit codec: CodecJson[T]) = new SimpleValidator[JOptionable[T]] {
    def maybeValid(path: Path): PartialFunction[(Option[Json], Option[Json]), (String, Path)] = {
      case (Some(j), _) if !codec.decodeJson(j).toOption.exists(values.contains) =>
        "Value outside of allowed values." -> path
    }
  }

  def nin[T](values:T*)(implicit codec: CodecJson[T]) = new SimpleValidator[JOptionable[T]] {
    def maybeValid(path: Path): PartialFunction[(Option[Json], Option[Json]), (String, Path)] = {
      case (Some(j), _) if codec.decodeJson(j).toOption.exists(values.contains) =>
        "Value not allowed." -> path
    }
  }

  def inCaseInsensitive(values:String*) = new SimpleValidator[JOptionable[String]] {
    def maybeValid(path: Path): PartialFunction[(Option[Json], Option[Json]), (String, Path)] = {
      case (Some(JString(s)), _) if !values.exists(_.equalsIgnoreCase(s)) =>
        "Value outside of allowed values." -> path
    }
  }

  def ninCaseInsensitive(values:String*) = new SimpleValidator[JOptionable[String]] {
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
  }

  def maxLength(value: Int) = new SimpleValidator[JOptionable[JLength]] {
    def maybeValid(path: Path) = {
      case (Some(JArray(seq)), _) if seq.length > value =>
        s"Array must have length of no greater than $value" -> path
      case (Some(JString(s)), _) if s.length > value =>
        s"String must have length of no greater than $value" -> path
    }
  }

  val nonEmpty = new SimpleValidator[JOptionable[JLength]] {
    def maybeValid(path: Path) = {
      case (Some(JArray(seq)), _) if seq.isEmpty =>
        s"Array must not be empty" -> path
      case (Some(JString(s)), _) if s.isEmpty =>
        s"String must not be empty" -> path
    }
  }

  val nonEmptyOrWhiteSpace:Validator[String] = new SimpleValidator[JLength] {
    def maybeValid(path: Path) = {
      case (Some(JString(text)), _) if text.trim().isEmpty =>
        s"Text must not be all empty or whitespace" -> path
    }
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
  }

  def custom[T](t: T => Boolean, message:String)(implicit codec:CodecJson[T]) = new Validator[JOptionable[T]] {
    def validate(value: Option[Json], currentState: Option[Json], path: Path): Seq[(String, Path)] =
      value.flatMap(j => codec.decodeJson(j).toOption)
        .filterNot(t)
        .fold(Seq.empty[(String, Path)]){_ => Seq(message -> path)}

  }

  def customCompare[T](deltaCurrent:(T, T) => Boolean, message:String)(implicit codec:CodecJson[T]) = new Validator[JOptionable[T]] {
    def validate(value: Option[Json], currentState: Option[Json], path: Path): Seq[(String, Path)] =
      (for {
        d <- value
        c <- currentState
        dt <- codec.decodeJson(d).toOption
        ct <- codec.decodeJson(c).toOption
        f <- (!deltaCurrent(dt, ct)).option(Seq(message -> path))
      } yield f).getOrElse(Nil)
  }
}

object Validators extends Validators
