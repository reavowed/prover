package net.prover.model

case class TemplateParsingContext(
    entryContext: EntryContext,
    parameterLists: Seq[Seq[(String, Int)]])
  extends ParsingContextWithParameters[TemplateParsingContext]
{
  override def addInnerParameters(parameters: Seq[(String, Int)]): TemplateParsingContext = {
    copy(parameterLists = parameterLists :+ parameters)
  }
}
