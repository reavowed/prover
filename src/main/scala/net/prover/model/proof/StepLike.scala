package net.prover.model.proof

import net.prover.model.expressions.Statement
import net.prover.parsing.{KnownWordParser, Parser}

trait StepLike {
  def statement: Statement
  def length: Int
  def serializedLines: Seq[String]
}

object StepLike {
  trait Wrapper extends StepLike {
    def substeps: Seq[StepLike]
    def statement: Statement = substeps.last.statement
    def length: Int = substeps.map(_.length).sum + 1
    def serializedLines: Seq[String] = substeps.flatMap(_.serializedLines)
  }

  def listParser[T](
    getParser: StepContext => KnownWordParser[T],
    addToContext: (StepContext, T) => StepContext)(
    implicit stepContext: StepContext
  ): Parser[(Seq[T], StepContext)] = {
    Parser.mapFoldWhileDefined[T, StepContext](stepContext) { (_, stepContext) =>
      getParser(stepContext).optional
        .mapMap(step => step -> addToContext(stepContext, step))
    }
  }
}
