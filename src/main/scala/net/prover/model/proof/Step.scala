package net.prover.model.proof

import com.fasterxml.jackson.annotation.{JsonIgnore, JsonIgnoreProperties}
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.model._
import net.prover.model.definitions.{DeductionDefinition, ExpressionDefinition, GeneralizationDefinition}
import net.prover.model.expressions.Statement

import scala.util.Try

@JsonIgnoreProperties(Array("substitutions", "isComplete"))
sealed trait Step {
  def `type`: String
  def provenStatement: Option[Statement]
  def referencedLines: Set[PreviousLineReference] = recursivePremises.flatMap(_.referencedLines).toSet
  def recursivePremises: Seq[Premise]
  def length: Int
  def serializedLines: Seq[String]
  override def toString: String = serializedLines.mkString("\n")
}

object Step {
  sealed trait WithoutSubsteps extends Step
  sealed trait WithSubsteps extends Step {
    def substeps: Seq[Step]
    def specifyStepContext(outerContext: StepContext): StepContext = outerContext
    def replaceSubsteps(newSubsteps: Seq[Step], stepContext: StepContext): Step
  }
  sealed trait WithVariable extends Step.WithSubsteps {
    def variableName: String
    def replaceVariableName(newVariableName: String): Step
    override def specifyStepContext(outerContext: StepContext): StepContext = {
      super.specifyStepContext(outerContext).addBoundVariable(variableName)
    }
  }
  sealed trait WithoutVariable extends Step.WithSubsteps {
    override def specifyStepContext(outerContext: StepContext): StepContext = outerContext
  }
  sealed trait WithAssumption extends Step.WithSubsteps {
    def assumption: Statement
    override def specifyStepContext(outerContext: StepContext): StepContext = {
      super.specifyStepContext(outerContext).addAssumption(assumption)
    }
  }

  sealed trait WithTopLevelStatement extends Step {
    def updateStatement(f: Statement => Try[Statement]): Try[Step]
  }

  case class Deduction(
      assumption: Statement,
      substeps: Seq[Step],
      deductionDefinition: DeductionDefinition)
    extends Step.WithSubsteps with WithoutVariable with WithTopLevelStatement with WithAssumption
  {
    val `type` = "deduction"
    override def provenStatement: Option[Statement] = {
      substeps.lastOption.flatMap(_.provenStatement).map(s => deductionDefinition(assumption, s))
    }
    override def replaceSubsteps(newSubsteps: Seq[Step], stepContext: StepContext): Deduction = copy(substeps = newSubsteps)
    override def updateStatement(f: Statement => Try[Statement]): Try[Step] = f(assumption).map(a => copy(assumption = a))
    override def recursivePremises: Seq[Premise] = substeps.flatMap(_.recursivePremises)
    override def length: Int = substeps.map(_.length).sum
    override def serializedLines: Seq[String] = Seq(s"assume ${assumption.serialized} {") ++
      substeps.flatMap(_.serializedLines).indent ++
      Seq("}")
  }
  object Deduction {
    def parser(implicit availableEntries: AvailableEntries, stepContext: StepContext): Parser[Deduction] = {
      val deductionDefinition = availableEntries.deductionDefinitionOption
        .getOrElse(throw new Exception("Cannot prove a deduction without an appropriate statement definition"))
      for {
        assumption <- Statement.parser
        substeps <- listParser(availableEntries, stepContext.addAssumption(assumption)).inBraces
      } yield Deduction(assumption, substeps, deductionDefinition)
    }
  }

  case class Naming(
      variableName: String,
      assumption: Statement,
      statement: Statement,
      substeps: Seq[Step],
      inference: Inference.Summary,
      premises: Seq[Premise],
      substitutions: Substitutions,
      @JsonIgnore generalizationDefinition: GeneralizationDefinition,
      @JsonIgnore deductionDefinition: DeductionDefinition)
    extends Step.WithSubsteps with WithTopLevelStatement with WithVariable with WithAssumption
  {
    val `type` = "naming"
    override def provenStatement: Option[Statement] = Some(statement)
    override def replaceVariableName(newVariableName: String): Step = copy(variableName = newVariableName)
    override def replaceSubsteps(newSubsteps: Seq[Step], stepContext: StepContext): Step = {
      val newInternalConclusion = newSubsteps.flatMap(_.provenStatement).lastOption.getOrElse(throw new Exception("No conclusion for naming step"))
      val newConclusion = newInternalConclusion.removeExternalParameters(1).getOrElse(throw new Exception("Naming step conclusion could not be extracted"))
      val newInternalPremise = generalizationDefinition(variableName, deductionDefinition(assumption, newInternalConclusion))
      val newSubstitutions = inference.premises.zip(premises.map(_.statement) :+ newInternalPremise)
        .foldLeft(Substitutions.Possible.empty) { case (substitutions, (inferencePremise, premise)) => inferencePremise.calculateSubstitutions(premise, substitutions)(stepContext).getOrElse(throw new Exception("Could not calculate substitutions from naming premise"))}
        .confirmTotality(inference.variableDefinitions)
        .getOrElse(throw new Exception("Naming substitutions were not total"))
      Naming(
        variableName,
        assumption,
        newConclusion,
        newSubsteps,
        inference,
        premises,
        newSubstitutions,
        generalizationDefinition,
        deductionDefinition)
    }
    override def updateStatement(f: Statement => Try[Statement]): Try[Step] = f(assumption).map(a => copy(assumption = a))
    override def recursivePremises: Seq[Premise] = premises ++ substeps.flatMap(_.recursivePremises)
    @JsonSerialize
    def referencedLinesForExtraction: Set[PreviousLineReference] = premises.flatMap(_.referencedLines).toSet
    override def length: Int = substeps.map(_.length).sum + 1
    override def serializedLines: Seq[String] = Seq(s"let $variableName ${assumption.serialized} ${inference.id} ${substitutions.serialize} {") ++
      substeps.flatMap(_.serializedLines).indent ++
      Seq(Seq("}", Premise.serialize(premises)).filter(_.nonEmpty).mkString(" "))
  }
  object Naming {
    def parser(implicit availableEntries: AvailableEntries, stepContext: StepContext): Parser[Naming] = {
      for {
        variableName <- Parser.singleWord
        assumption <- Statement.parser(ExpressionParsingContext.atStep(stepContext.addBoundVariable(variableName)))
        innerStepContext = stepContext.addBoundVariable(variableName).addAssumption(assumption)
        inference <- Inference.parser
        substitutions <- inference.substitutionsParser
        substeps <- listParser(availableEntries, innerStepContext).inBraces
        internalConclusion = substeps.flatMap(_.provenStatement).lastOption.getOrElse(throw new Exception("No conclusion for naming step"))
        extractedConclusion = internalConclusion.removeExternalParameters(1).getOrElse(throw new Exception("Naming step conclusion could not be extracted"))
        premiseStatements = inference.substitutePremisesAndValidateConclusion(extractedConclusion, substitutions).getOrElse(throw new Exception("Could not apply substitutions"))
        premises <- premiseStatements.init.map(Premise.parser).traverse
        generalizationDefinition = availableEntries.generalizationDefinitionOption.getOrElse(throw new Exception("Naming step requires a generalization statement"))
        deductionDefinition = availableEntries.deductionDefinitionOption.getOrElse(throw new Exception("Naming step requires a deduction statement"))
        internalPremise = generalizationDefinition(variableName, deductionDefinition(assumption, internalConclusion))
        _ = if (internalPremise != premiseStatements.last) throw new Exception("Invalid naming premise")
      } yield {
        Naming(variableName, assumption, extractedConclusion, substeps, inference, premises, substitutions, generalizationDefinition, deductionDefinition)
      }
    }
  }

  case class Generalization(
      variableName: String,
      substeps: Seq[Step],
      generalizationDefinition: GeneralizationDefinition)
    extends Step.WithSubsteps with WithVariable
  {
    val `type` = "generalization"
    override def provenStatement: Option[Statement] = {
      substeps.lastOption.flatMap(_.provenStatement).map(s => generalizationDefinition(variableName, s))
    }
    override def replaceSubsteps(newSubsteps: Seq[Step], stepContext: StepContext): Step = copy(substeps = newSubsteps)
    override def replaceVariableName(newVariableName: String): Step = copy(variableName = newVariableName)
    override def recursivePremises: Seq[Premise] = substeps.flatMap(_.recursivePremises)
    override def length: Int = substeps.map(_.length).sum
    override def serializedLines: Seq[String] = Seq(s"take $variableName {") ++
      substeps.flatMap(_.serializedLines).indent ++
      Seq("}")
  }
  object Generalization {
    def parser(implicit availableEntries: AvailableEntries, stepContext: StepContext): Parser[Generalization] = {
      val generalizationDefinition = availableEntries.generalizationDefinitionOption
        .getOrElse(throw new Exception("Generalization step could not find generalization statement"))
      for {
        variableName <- Parser.singleWord
        substeps <- listParser(availableEntries, stepContext.addBoundVariable(variableName)).inBraces
      } yield Generalization(variableName, substeps, generalizationDefinition)
    }
  }

  case class Target(statement: Statement) extends Step.WithoutSubsteps with Step.WithTopLevelStatement {
    val `type` = "target"
    override def provenStatement: Option[Statement] = Some(statement)
    override def updateStatement(f: Statement => Try[Statement]): Try[Step] = f(statement).map(a => copy(statement = a))
    override def recursivePremises: Seq[Premise] = Nil
    override def length = 1
    def serializedLines: Seq[String] = Seq(s"target ${statement.serialized}")
  }
  object Target {
    def parser(implicit availableEntries: AvailableEntries, stepContext: StepContext): Parser[Target] = {
      for {
        statement <- Statement.parser
      } yield Target(statement)
    }
  }

  case class Elided(substeps: Seq[Step], highlightedInference: Option[Inference.Summary], description: Option[String]) extends Step.WithSubsteps with WithoutVariable {
    val `type` = "elided"
    override def provenStatement: Option[Statement] = substeps.flatMap(_.provenStatement).lastOption
    override def replaceSubsteps(newSubsteps: Seq[Step], stepContext: StepContext): Step = copy(substeps = newSubsteps)
    override def recursivePremises: Seq[Premise] = substeps.flatMap(_.recursivePremises)
    override def length: Int = substeps.map(_.length).sum
    override def serializedLines: Seq[String] = Seq(("elided" +: (highlightedInference.map(_.id).toSeq ++ description.map(_.inParens).toSeq) :+ "{").mkString(" ")) ++
      substeps.flatMap(_.serializedLines).indent ++
      Seq("}")
  }
  object Elided {
    def parser(implicit availableEntries: AvailableEntries, stepContext: StepContext): Parser[Elided] = {
      for {
        highlightedInference <- Inference.parser.tryOrNone
        description <- Parser.allInParens.tryOrNone
        substeps <- listParser.inBraces
      } yield Elided(substeps, highlightedInference, description)
    }
    def ifNecessary(substeps: Seq[Step], elider: Seq[Step] => Step): Option[Step] = {
      substeps match {
        case Nil => None
        case Seq(single) => Some(single)
        case _ => Some(elider(substeps))
      }
    }
    def ifNecessary(substeps: Seq[Step], inference: Inference): Option[Step] = {
      ifNecessary(substeps, forInference(inference))
    }
    def ifNecessary(substeps: Seq[Step], description: String): Option[Step] = {
      ifNecessary(substeps, Step.Elided(_, None, Some(description)))
    }
    def ifNecessary(substeps: Seq[Step], inferenceOption: Option[Inference], description: String): Option[Step] = {
      ifNecessary(substeps, inferenceOption.map(forInference).getOrElse(forDescription(description)))
    }

    def forInference(inference: Inference): Seq[Step] => Step.Elided = {
      Step.Elided(_, Some(inference.summary), None)
    }
    def forDescription(description: String): Seq[Step] => Step.Elided = {
      Step.Elided(_, None, Some(description))
    }
  }

  case class Assertion(
      statement: Statement,
      inference: Inference.Summary,
      premises: Seq[Premise],
      substitutions: Substitutions)
    extends Step.WithoutSubsteps with Step.WithTopLevelStatement
  {
    val `type`: String = "assertion"
    override def provenStatement: Option[Statement] = Some(statement)
    override def updateStatement(f: Statement => Try[Statement]): Try[Step] = f(statement).map(a => copy(statement = a))
    @JsonSerialize
    def referencedLinesForAssertion: Set[PreviousLineReference] = premises.flatMap(_.referencedLines).toSet
    override def recursivePremises: Seq[Premise] = premises
    override def length: Int = 1
    override def serializedLines: Seq[String] = {
      Seq(Seq("prove", statement.serialized, inference.id, substitutions.serialize, Premise.serialize(premises)).filter(_.nonEmpty).mkString(" "))
    }
    def pendingPremises: Map[Seq[Int], Premise.Pending] = {
      premises.flatMapWithIndex((p, i) => p.getPendingPremises(Seq(i)).toSeq).toMap
    }
  }
  object Assertion {
    def parser(implicit availableEntries: AvailableEntries, stepContext: StepContext): Parser[Assertion] = {
      for {
        statement <- Statement.parser
        inference <- Inference.parser
        substitutions <- inference.substitutionsParser
        premiseStatements = inference.substitutePremisesAndValidateConclusion(statement, substitutions).getOrElse(throw new Exception("Could not apply substitutions"))
        premises <- premiseStatements.map(Premise.parser).traverse
        _ = inference.validatePremisesAndConclusion(premises.map(_.statement), statement, substitutions)
      } yield {
        Assertion(statement, inference, premises, substitutions)
      }
    }

    def forInference(inference: Inference, substitutions: Substitutions)(implicit substitutionContext: SubstitutionContext): Option[Assertion] = {
      for {
        premises <- inference.substitutePremises(substitutions)
        conclusion <- inference.substituteConclusion(substitutions)
      } yield Assertion(conclusion, inference.summary, premises.map(Premise.Pending), substitutions.restrictTo(inference.variableDefinitions))
    }
  }

  case class SubProof(name: String, substeps: Seq[Step]) extends Step.WithSubsteps with WithoutVariable {
    val `type`: String = "subproof"
    override def replaceSubsteps(newSubsteps: Seq[Step], stepContext: StepContext): Step = copy(substeps = newSubsteps)
    override def provenStatement: Option[Statement] = substeps.flatMap(_.provenStatement).lastOption
    override def recursivePremises: Seq[Premise] = substeps.flatMap(_.recursivePremises)
    override def length: Int = substeps.map(_.length).sum
    override def serializedLines: Seq[String] = Seq(s"subproof ($name) {") ++
      substeps.flatMap(_.serializedLines).indent ++
      Seq("}")
  }
  object SubProof {
    def parser(implicit availableEntries: AvailableEntries, stepContext: StepContext): Parser[SubProof] = {
      for {
        name <- Parser.allInParens
        substeps <- listParser.inBraces
      } yield SubProof(name, substeps)
    }
  }

  case class ExistingStatementExtraction(substeps: Seq[Step]) extends Step.WithSubsteps {
    val `type` = "existingStatementExtraction"
    override def provenStatement: Option[Statement] = {
      substeps.flatMap(_.provenStatement.toSeq).lastOption
    }
    override def replaceSubsteps(newSubsteps: Seq[Step], stepContext: StepContext): Step = {
      copy(substeps = newSubsteps)
    }
    override def recursivePremises: Seq[Premise] = substeps.flatMap(_.recursivePremises)
    override def length: Int = substeps.map(_.length).sum
    override def serializedLines: Seq[String] = Seq(s"existingStatementExtraction {") ++
      substeps.flatMap(_.serializedLines).indent ++
      Seq("}")
  }
  object ExistingStatementExtraction {
    def parser(implicit availableEntries: AvailableEntries, stepContext: StepContext): Parser[ExistingStatementExtraction] = {
      for {
        substeps <- listParser.inBraces
      } yield ExistingStatementExtraction(substeps)
    }
  }

  def parser(implicit availableEntries: AvailableEntries, stepContext: StepContext): Parser[Option[Step]] = {
    Parser.selectOptionalWordParser {
      case "assume" => Deduction.parser
      case "let" => Naming.parser
      case "take" => Generalization.parser
      case "target" => Target.parser
      case "prove" => Assertion.parser
      case "elided" => Elided.parser
      case "subproof" => SubProof.parser
      case "existingStatementExtraction" => ExistingStatementExtraction.parser
    }
  }
  def listParser(implicit availableEntries: AvailableEntries, stepContext: StepContext): Parser[Seq[Step]] = {
    Parser.foldWhileDefined[Step, StepContext](stepContext) { (_, index, currentStepContext) =>
      val innerStepContext = currentStepContext.atIndex(index)
      parser(availableEntries, innerStepContext)
        .mapMap(step => step -> currentStepContext.addStep(step, innerStepContext.stepReference))
    }.map(_._1)
  }
}
