package net.prover.model

case class TemplateParsingContext(
    entryContext: EntryContext,
    parameterLists: Seq[Seq[(String, Int)]])
  extends ParsingContextWithParameters
{
  def addInnerParameters(parameters: Seq[(String, Int)]): TemplateParsingContext = {
    if (parameters.isEmpty)
      this
    else
      copy(parameterLists = parameterLists :+ parameters)
  }
}
