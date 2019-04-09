package net.prover.model

case class TemplateParsingContext(
    entryContext: EntryContext,
    parameterLists: Seq[Seq[(String, Int)]])
  extends ParsingContextWithParameters
