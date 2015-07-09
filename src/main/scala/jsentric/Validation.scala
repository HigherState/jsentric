package jsentric

import argonaut._
import scalaz._

trait Validation extends Functions {
  implicit def valueContractValidation[T](contract:ValueContract[T]) =
    new ValueContractValidation[T](contract)

  implicit def baseContractValidation(contract:BaseContract) =
    new BaseContractValidation(contract)

  implicit def propertyValidation[T](prop:Property[T]) =
    new PropertyValidation(prop)

}

object Validation extends Validation {
  def seqToJValid(seq:Seq[(String, Path)], json:Json) =
    seq match {
      case Nil => \/-(json)
      case failure +: failures => -\/(NonEmptyList(failure, failures:_*))
    }
}

class ValueContractValidation[T](val contract:ValueContract[T]) extends AnyVal {
  import Validation._
  def $validate(newContent:Json):JValid =
    seqToJValid($validate(newContent, None, Path.empty), newContent)

  def $validate(deltaContent:Json, currentState:Json):JValid =
    seqToJValid($validate(deltaContent, Some(currentState), Path.empty), deltaContent)
  //TODO better approach here
  def $validate(deltaContent:Json, currentState:Option[Json]):JValid =
    seqToJValid($validate(deltaContent, currentState, Path.empty), deltaContent)

  def $validate(value: Json, currentState: Option[Json], path:Path): Seq[(String, Path)] =
    contract.validator.validate(Some(value), currentState, path)

  def $sanitize(json:Json):Json = ???
}

class BaseContractValidation(val contract:BaseContract) extends AnyVal with Functions {
  import Validation._
  def $validate(newContent:Json):JValid =
    seqToJValid($validate(newContent, None, Path.empty), newContent)

  def $validate(deltaContent:Json, currentState:Json):JValid =
    seqToJValid($validate(deltaContent, Some(currentState), Path.empty), deltaContent)

  def $validate(deltaContent:Json, currentState:Option[Json]):JValid =
    seqToJValid($validate(deltaContent, currentState, Path.empty), deltaContent)
  //TODO better approach here
  def $validate(value: Json, currentState: Option[Json], path:Path): Seq[(String, Path)] =
    ValidationPropertyCache.getProperties(contract).flatMap{p =>
      val v = getValue(value, p.relativePath.segments)
      val c = currentState.flatMap(getValue(_, p.relativePath.segments))
      new PropertyValidation(p).$validate(v, c, path ++ p.relativePath)
    }

  def $sanitize(json:Json):Json = {
    ValidationPropertyCache.getInternal(contract).foldLeft(json){ (j, p) =>
      dropValue(j, p.absolutePath.segments)
    }
  }
}

class PropertyValidation[T](val prop:Property[T]) extends AnyVal {
  def $validate(value: Option[Json], currentState: Option[Json], path:Path): Seq[(String, Path)] =
    ((value, currentState, prop) match {
      case (None, None, p: Expected[_]) =>
        Seq("Value required." -> path)
      case (Some(v), c, _) if prop.pattern.unapply(v).isEmpty =>
        Seq(s"Unexpected type '${v.getClass.getSimpleName}'." -> path)
      case (Some(v), c, b:BaseContract) =>
        new BaseContractValidation(b).$validate(v, c, path)
      case _ =>
        Seq.empty
    }) ++ prop.validator.validate(value, currentState, path)
}

object ValidationPropertyCache {
  private var properties:Map[Class[_], Seq[Property[_]]] = Map.empty
  private var internal:Map[Class[_], Seq[Property[_]]] = Map.empty
  def getProperties(contract:BaseContract):Seq[Property[_]] =
    properties.get(contract.getClass) match {
      case Some(p) =>
        p
      case None =>
        val vp =
          contract.getClass.getMethods
            .filter(m => m.getParameterTypes.isEmpty && classOf[Property[_]].isAssignableFrom(m.getReturnType))
            .map(_.invoke(contract).asInstanceOf[Property[_]])
            .toSeq
        this.synchronized {
          properties = properties + (contract.getClass -> vp)
        }
        vp
    }

  def getInternal(contract:BaseContract):Seq[Property[_]] = {
    internal.get(contract.getClass) match {
      case Some(p) =>
        p
      case None =>
        val ip = getProperties(contract).collect {
          case p if isInternal(p.validator) => Seq(p)
          case p: BaseContract =>
            getInternal(p)
        }.flatten
        this.synchronized {
          internal = internal + (contract.getClass -> ip)
        }
        ip
    }
  }

  private val isInternal:Function[Validator[_], Boolean] = {
    case AndValidator(l,r) =>
      isInternal(l) || isInternal(r)
    case OrValidator(l, r) => // bit odd through excpetion for now
      throw new Exception("Internal json validator shouldn't be in an 'or' conditional")
    case v => v == Internal
  }
}

