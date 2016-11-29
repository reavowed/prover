package net.prover.model

case class Definition(connective: Connective, definingStatement: Statement) extends ChapterEntry with DirectStepParser {
  val `type` = "definition"
  val definedStatement: Statement = ConnectiveStatement(
    (1 to connective.arity).map(Atom),
    connective)

  override val name: String = "definition-" + connective.name

  override def readStep(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): (Step, PartialLine) = {
    val (reference, lineAfterReference) = line.splitFirstWord
    val referredStatement = theoremBuilder.resolveReference(reference)
    val replacedStatement = applyToStatement(referredStatement)
    val step = Step(replacedStatement)
    (step, lineAfterReference)
  }

  def applyToStatement(statement: Statement): Statement = {
    definedStatement.attemptMatch(statement).map(definingStatement.replace)
      .orElse(definingStatement.attemptMatch(statement).map(definedStatement.replace))
      .getOrElse(throw new Exception(s"Could not apply definition to statement '$statement'"))
  }
}

object Definition extends SingleLineChapterEntryParser[Definition] {
  override val name: String = "definition"
  override def parse(line: PartialLine, context: Context): Definition = {
    val (connectiveName, lineAfterConnectiveName) = line.splitFirstWord
    val connective = context.connectives.find(_.name == connectiveName)
      .getOrElse(throw ParseException.withMessage(s"Unrecognised connective '$connectiveName'", line.fullLine))
    val (definingStatement, _) = Statement.parse(lineAfterConnectiveName, context)
    Definition(connective, definingStatement)
  }
  override def addToContext(definition: Definition, context: Context): Context = {
    context.copy(definitions = context.definitions :+ definition)
  }
}
