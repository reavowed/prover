package net.prover.model

case class Context(
    statementDefinitions: Seq[StatementDefinition],
    termDefinitions: Seq[TermDefinition],
    inferences: Seq[Inference],
    inferenceTransforms: Seq[InferenceTransform],
    variables: Variables,
    theoremCache: Map[String, Theorem]) {

  def combine(others: Seq[Context]): Context = {
    Context(
      others.flatMap(_.statementDefinitions) ++ statementDefinitions,
      others.flatMap(_.termDefinitions) ++ termDefinitions,
      others.flatMap(_.inferences) ++ inferences,
      others.flatMap(_.inferenceTransforms) ++ inferenceTransforms,
      variables,
      theoremCache)
  }

  def addStatementDefinition(statementDefinition: StatementDefinition): Context = {
    copy(
      statementDefinitions = statementDefinitions :+ statementDefinition,
      inferences = inferences ++
        statementDefinition.forwardInference.toSeq ++
        statementDefinition.reverseInference.toSeq)
  }

  def addTermDefinition(termDefinition: TermDefinition) = {
    copy(
      termDefinitions = termDefinitions :+ termDefinition,
      inferences = inferences :+ termDefinition.inference)
  }

  def addInferenceTransform(inferenceTransform: InferenceTransform) = {
    copy(inferenceTransforms = inferenceTransforms :+ inferenceTransform)
  }

  def withTheoremCache(theoremCache: Map[String, Theorem]): Context = copy(theoremCache = theoremCache)

  def nextInferenceKey(name: String): String = {
    inferences.count(_.name == name) match {
      case 0 =>
        name.formatAsKey
      case n =>
        (name + " " + (n+1)).formatAsKey
    }
  }
}

object Context {
  val empty = Context(Nil, Nil, Nil, Nil, Variables.empty, Map.empty)
}
