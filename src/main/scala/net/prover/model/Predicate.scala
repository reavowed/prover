package net.prover.model

case class Predicate(symbol: String, arity: Int) extends ChapterEntry {
  val `type` = "predicate"
  val defaultStatement: PredicateStatement = apply((1 to arity).map(TermVariable): _*)

  def parseStatement(line: PartialLine, context: Context): (Statement, PartialLine) = {
    (1 to arity).mapFold(line) { case (_, lineSoFar) =>
        Term.parse(lineSoFar, context)
    }.mapLeft(apply(_: _*))
  }

  def apply(terms: Term*): PredicateStatement = {
    PredicateStatement(terms, this)
  }
}

object Predicate extends SingleLineChapterEntryParser[Predicate] {
  override val name: String = "predicate"
  override def parse(line: PartialLine, context: Context): Predicate = {
    line.splitWords match {
      case Seq(symbol, IntParser(arity)) =>
        Predicate(symbol, arity)
      case _ =>
        throw ParseException.withMessage("Could not parse predicate definition", line.fullLine)
    }
  }
  override def addToContext(predicate: Predicate, context: Context): Context = {
    context.copy(predicates = context.predicates :+ predicate)
  }
}
