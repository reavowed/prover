package net.prover.model

case class Context(
    statementDefinitions: Seq[StatementDefinition],
    termSpecifications: Seq[TermSpecification],
    inferences: Seq[Inference],
    inferenceTransforms: Seq[InferenceTransform],
    variables: Variables) {

  def combine(others: Seq[Context]): Context = {
    Context(
      others.flatMap(_.statementDefinitions) ++ statementDefinitions,
      others.flatMap(_.termSpecifications) ++ termSpecifications,
      others.flatMap(_.inferences) ++ inferences,
      others.flatMap(_.inferenceTransforms) ++ inferenceTransforms,
      variables
    )
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
      termSpecifications = termSpecifications :+ termDefinition.specification,
      inferences = inferences :+ termDefinition.inference)
  }

  def addInferenceTransform(inferenceTransform: InferenceTransform) = {
    copy(inferenceTransforms = inferenceTransforms :+ inferenceTransform)
  }
}

object Context {
  val empty = Context(Nil, Nil, Nil, Nil, Variables.empty)
}
