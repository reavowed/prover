package net.prover.model

import net.prover.model.expressions.FunctionParameter

import scala.util.Try

trait ParsingContextWithParameters {
  def parameterLists: Seq[Seq[(String, Int)]]

  object RecognisedParameter {
    private val literalPattern = "(\\$+)(\\d+)".r
    def unapply(string: String): Option[FunctionParameter] = {
      parameterLists.reverse.zipWithIndex.mapFind {
        case (parameterList, level) =>
          parameterList.find(_._1 == string).map(_._2).map(index => FunctionParameter(index, level))
      } orElse (string match {
        case literalPattern(dollars, indexString) =>
          val level = dollars.length - 1
          for {
            index <- Try(indexString.toInt).toOption
          } yield FunctionParameter(index, level)
        case _ =>
          None
      })
    }
  }
}
