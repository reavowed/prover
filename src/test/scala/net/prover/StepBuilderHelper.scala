package net.prover

import net.prover.books.reading.ProofFileReader
import net.prover.model.TestDefinitions._
import net.prover.model.entries.Theorem
import net.prover.model.expressions.{FunctionParameter, Statement}
import net.prover.model.proof.{Premise, Step, StepContext, SubstitutionContext}
import net.prover.model.{AvailableEntries, Substitutions, VariableDefinitions}
import net.prover.theorems.RecalculateReferences
import org.specs2.matcher.Matcher
import org.specs2.mock.mockito.MockitoStubs
import org.specs2.mutable.SpecificationLike

trait StepBuilderHelper extends SpecificationLike with MockitoStubs with ContextHelper with StepHelpers {

  def createTheorem(premises: Seq[Statement], steps: Seq[Step])(implicit variableDefinitions: VariableDefinitions): Theorem = {
    createTheorem(premises, steps.last.provenStatement.get, steps)
  }
  def createTheorem(premises: Seq[Statement], conclusion: Statement, steps: Seq[Step])(implicit variableDefinitions: VariableDefinitions): Theorem = {
    Theorem(
      "Test Theorem",
      variableDefinitions,
      premises,
      conclusion,
      Seq(Theorem.Proof(steps)))
  }
  def createTheorem(premises: Seq[Statement], stepsConstructor: SubstitutionContext => Seq[Step])(implicit variableDefinitions: VariableDefinitions): Theorem = {
    createTheorem(premises, stepsConstructor(SubstitutionContext.outsideProof))
  }
  def createTheorem(premises: Seq[Statement], conclusion: Statement, stepsConstructor: SubstitutionContext => Seq[Step])(implicit variableDefinitions: VariableDefinitions): Theorem = {
    createTheorem(premises, conclusion, stepsConstructor(SubstitutionContext.outsideProof))
  }

  def createSteps(
    stepsConstructor: SubstitutionContext => Seq[Step],
    boundVariables: Seq[String] = Nil)(
    implicit availableEntries: AvailableEntries,
    variableDefinitions: VariableDefinitions
  ): Seq[Step] = {
    val steps = stepsConstructor(SubstitutionContext.withExtraParameters(boundVariables.length)(SubstitutionContext.outsideProof))
    implicit val outerStepContext = createOuterStepContext(boundVariables)
    recalculateReferences(steps)
  }

  def recalculateReferences(steps: Seq[Step])(implicit outerStepContext: StepContext, availableEntries: AvailableEntries): Seq[Step] = {
    RecalculateReferences(createStepsWithContext(steps))._1
  }

  def beValidTheorem(implicit availableEntries: AvailableEntries): Matcher[Theorem] = (theorem: Theorem) => {
    val recalculatedTheorem = RecalculateReferences(createTheoremWithContext(theorem))._1
    val serializedTheorem = recalculatedTheorem.serializedLines.mkString("\n").stripPrefix("theorem ")
    val serializedProofs = recalculatedTheorem.proofs.map(_.serialized)
    val proofFileReader = mock[ProofFileReader]
    proofFileReader.getSerializedProofs(recalculatedTheorem.title) returns serializedProofs
    val parsedTheorem = Theorem.parser(availableEntries, proofFileReader).parseFromString(serializedTheorem, "Theorem")
    parsedTheorem must beTypedEqualTo(theorem)
    parsedTheorem.isComplete(createTheoremWithContext(parsedTheorem)) must beTrue
  }

  def beStepsThatMakeValidTheorem(premises: Seq[Statement], conclusion: Statement)(implicit availableEntries: AvailableEntries, variableDefinitions: VariableDefinitions): Matcher[Seq[Step]] = {
    beValidTheorem(availableEntries) ^^ { (steps: Seq[Step]) => createTheorem(premises, conclusion, steps) }
  }

  def beStepThatMakesValidTheorem(premises: Seq[Statement], conclusion: Statement, depth: Int = 0)(implicit availableEntries: AvailableEntries, variableDefinitions: VariableDefinitions): Matcher[Step] = {
    beStepsThatMakeValidTheorem(premises, conclusion, depth) ^^ { step: Step => Seq(step) }
  }

  def beStepsThatMakeValidTheorem(premises: Seq[Statement], conclusion: Statement, depth: Int)(implicit availableEntries: AvailableEntries, variableDefinitions: VariableDefinitions): Matcher[Seq[Step]] = {
    if (depth == 0)
      beStepsThatMakeValidTheorem(premises, conclusion)
    else {
      def generalizeOnce(statement: Statement, i: Int): Statement = ForAll(s"x_$i")(statement)

      def generalizeToDepth(statement: Statement, parameterDepth: Int): Statement = (0 until parameterDepth).foldLeft(statement)(generalizeOnce)

      def specificationStep(statement: Statement, parameterDepth: Int) = {
        Step.Assertion(
          statement,
          specification.summary,
          Seq(Premise.Pending(generalizeOnce(statement, parameterDepth).insertExternalParameters(1))),
          Substitutions(Seq(statement.specify(Seq(FunctionParameter(0, depth - parameterDepth)), 0, 0).get), Seq($)))
      }

      beStepsThatMakeValidTheorem(premises.map(generalizeToDepth(_, depth)), generalizeToDepth(conclusion, depth)) ^^ { steps: Seq[Step] =>
        (0 until depth).foldLeft(steps) { case (steps, i) => Seq(Step.Generalization(s"x_$i", premises.map(p => specificationStep(generalizeToDepth(p, i), i)) ++ steps, GeneralizationDefinition)) }
      }
    }
  }

}
