package net.prover.model.proof

import net.prover.model.expressions.Statement
import net.prover.parsing.Parser

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
    getParser: StepContext => Parser[Option[T]],
    addToContext: (StepContext, T) => StepContext)(
    implicit stepContext: StepContext
  ): Parser[(Seq[T], StepContext)] = {
    Parser.mapFoldWhileDefined[T, StepContext](stepContext) { (_, stepContext) =>
      getParser(stepContext)
        .mapMap(step => step -> addToContext(stepContext, step))
    }
  }
}
