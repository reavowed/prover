package net.prover.controllers.models

import org.springframework.core.convert.converter.Converter
import org.springframework.stereotype.Component

case class PathData(indexes: Seq[Int]) {
 override def toString: String = indexes.mkString(".")
}

object PathData {
  implicit def toIndexes(pathData: PathData): Seq[Int] = pathData.indexes
}

@Component
class StepReferenceConverter extends Converter[String, PathData] {
  override def convert(source: String): PathData = {
    PathData(source.split('.').map(_.toInt))
  }
}
