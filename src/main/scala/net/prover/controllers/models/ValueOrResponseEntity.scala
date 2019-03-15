package net.prover.controllers.models

import org.springframework.http.{HttpStatus, ResponseEntity}

sealed trait ValueOrResponseEntity[+T] {
  def map[S](f: T => S): ValueOrResponseEntity[S]
  def flatMap[S](f: T => ValueOrResponseEntity[S]): ValueOrResponseEntity[S]
  def withFilter(f: T => Boolean): ValueOrResponseEntity[T] = this
  def toResponseEntity: ResponseEntity[_]
}

object ValueOrResponseEntity {
  case class Value[+T](t: T) extends ValueOrResponseEntity[T] {
    override def map[S](f: T => S): ValueOrResponseEntity[S] = Value(f(t))
    override def flatMap[S](f: T => ValueOrResponseEntity[S]): ValueOrResponseEntity[S] = f(t)
    override def toResponseEntity: ResponseEntity[_] = new ResponseEntity[T](t, HttpStatus.OK)
  }
  case class Entity(responseEntity: ResponseEntity[Any]) extends ValueOrResponseEntity[Nothing] {
    override def map[S](f: Nothing => S): ValueOrResponseEntity[S] = this
    override def flatMap[S](f: Nothing => ValueOrResponseEntity[S]): ValueOrResponseEntity[S] = this
    override def toResponseEntity: ResponseEntity[_] = responseEntity
  }
}

object ValueOrResponseEntityConverters {
  implicit class OptionOps[T](option: Option[T]) {
    def orResponseEntity(responseEntity: => ResponseEntity[Any]): ValueOrResponseEntity[T] = option match {
      case Some(t) => ValueOrResponseEntity.Value(t)
      case None => ValueOrResponseEntity.Entity(responseEntity)
    }
    def orResponseEntity(value: Any, httpStatus: HttpStatus): ValueOrResponseEntity[T] = orResponseEntity(new ResponseEntity(value, httpStatus))
    def orHttpStatus(httpStatus: HttpStatus): ValueOrResponseEntity[T] = orResponseEntity(new ResponseEntity(httpStatus))
    def orNotFound: ValueOrResponseEntity[T] = orHttpStatus(HttpStatus.NOT_FOUND)
    def orBadRequest(message: String): ValueOrResponseEntity[T] = orResponseEntity(message, HttpStatus.BAD_REQUEST)
  }
  implicit class SeqOps[T](seq: Seq[T]) {
    def mapToMapOfResponseEntities[S](f: T => ValueOrResponseEntity[S]): ValueOrResponseEntity[Map[T, S]] = {
      seq.foldLeft[ValueOrResponseEntity[Map[T, S]]](ValueOrResponseEntity.Value(Map.empty[T, S])) { (mapEntitySoFar, nextKey) =>
        for {
          mapSoFar <- mapEntitySoFar
          nextValue <- f(nextKey)
        } yield mapSoFar + (nextKey -> nextValue)
      }
    }
  }
}