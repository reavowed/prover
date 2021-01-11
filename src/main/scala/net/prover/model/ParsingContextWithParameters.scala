package net.prover.model

import net.prover._
import net.prover.core.expressions.Parameter

import scala.util.Try

trait ParsingContextWithParameters[T] {
  def parameterLists: Seq[Seq[(String, Int)]]
  def addInnerParameters(parameters: Seq[(String, Int)]): T

  object RecognisedParameter {
    private val literalPattern = "(\\$+)(\\d+)".r
    def unapply(string: String): Option[Parameter] = {
      parameterLists.reverse.zipWithIndex.mapFind {
        case (parameterList, level) =>
          parameterList.find(_._1 == string).map(_._2).map(index => Parameter(index, level))
      } orElse (string match {
        case literalPattern(dollars, indexString) =>
          val level = dollars.length - 1
          for {
            index <- Try(indexString.toInt).toOption
            if level < parameterLists.length && index < parameterLists(parameterLists.length - level - 1).length
          } yield Parameter(index, level)
        case _ =>
          None
      })
    }
  }
}
