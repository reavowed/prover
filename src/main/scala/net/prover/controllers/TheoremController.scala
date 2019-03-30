package net.prover.controllers

import net.prover.controllers.models.{QuickDefinition, StepDefinition}
import net.prover.exceptions.BadRequestException
import net.prover.model._
import net.prover.model.entries.{StatementDefinition, TermDefinition, Theorem}
import net.prover.model.expressions.Statement
import net.prover.model.proof.Step.NewAssert
import net.prover.model.proof._
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.core.convert.converter.Converter
import org.springframework.http.ResponseEntity
import org.springframework.stereotype.Component
import org.springframework.web.bind.annotation._

import scala.reflect._
import scala.util.{Failure, Success, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}"))
class TheoremController @Autowired() (bookService: BookService) {
  case class InferenceSuggestion(
    inference: Inference.Summary,
    requiredSubstitutions: Substitutions.Required,
    substitutions: Seq[Substitutions])
  @GetMapping(value = Array("/{stepReference}/suggestInferences"), produces = Array("application/json;charset=UTF-8"))
  def suggestInferences(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepReference: PathData,
    @RequestParam("searchText") searchText: String
  ): ResponseEntity[_] = {
    (for {
      (book, chapter, theorem, step, stepContext) <- findStep[Step.Target](bookKey, chapterKey, theoremKey, stepReference)
    } yield {
      implicit val parsingContext: ParsingContext = getStepParsingContext(book, chapter, theorem, stepContext)
      parsingContext.inferences
        .filter(_.name.toLowerCase.contains(searchText.toLowerCase))
        .reverse
        .mapCollect { inference =>
          val substitutions = inference.conclusion.calculateSubstitutions(step.statement, Substitutions.empty, 0, stepContext.externalDepth)
          if (substitutions.nonEmpty)
            Some(InferenceSuggestion(inference.summary, inference.requiredSubstitutions, substitutions))
          else
            None
        }
        .take(10)
    }).toResponseEntity
  }

  case class PossiblePremiseMatch(statement: Statement, substitutions: Seq[Substitutions])
  @GetMapping(value = Array("/{stepReference}/suggestPremises"), produces = Array("application/json;charset=UTF-8"))
  def suggestPremisesForSubstitutions(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepReference: PathData,
    @RequestParam("inferenceId") inferenceId: String
  ): ResponseEntity[_] = {
    (for {
      (book, chapter, theorem, step, stepContext) <- findStep[Step.Target](bookKey, chapterKey, theoremKey, stepReference)
      parsingContext = getStepParsingContext(book, chapter, theorem, stepContext)
      inference <- findInference(inferenceId)(parsingContext)
    } yield {
      val availablePremises = ProofHelper.getAvailablePremises(stepContext, parsingContext)
      inference.premises.map { premise =>
        availablePremises.mapCollect { availablePremise =>
          val substitutions = premise.statement.calculateSubstitutions(availablePremise.statement, Substitutions.empty, 0, stepContext.externalDepth)
          if (substitutions.nonEmpty) {
            Some(PossiblePremiseMatch(availablePremise.statement, substitutions))
          } else {
            None
          }
        }
      }
    }).toResponseEntity
  }

  @PutMapping(value = Array("/{stepReference}"))
  def createStep(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepReference: PathData,
    @RequestBody definition: StepDefinition
  ): ResponseEntity[_] = {
    modifyStep[Step.Target](bookKey, chapterKey, theoremKey, stepReference) { (book, chapter, theorem, step, stepContext) =>
      implicit val parsingContext: ParsingContext = getStepParsingContext(book, chapter, theorem, stepContext)
      for {
        inference <- findInference(definition.inferenceId)
        substitutions <- definition.parseSubstitutions(inference)
        premiseStatements <- Try(inference.substitutePremisesAndValidateConclusion(substitutions, step.statement, stepContext.externalDepth)).recoverWith { case e => Failure(BadRequestException(e.getMessage))}
      } yield {
        Step.NewAssert(
          step.statement,
          inference,
          premiseStatements.map(createPremise(_, stepContext, parsingContext)),
          substitutions)
      }
    }.toResponseEntity
  }

  @DeleteMapping(value = Array("/{stepReference}"))
  def deleteStep(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepReference: PathData
  ): ResponseEntity[_] = {
    def getReplacementStep(step: Step): Try[Option[Step]] = {
      step match {
        case _: Step.Target =>
          Success(None)
        case _ =>
          Success(step.provenStatement.map(s => Step.Target(s)))
      }
    }
    def deleteFromTheorem(theorem: Theorem, parsingContext: ParsingContext): Option[Try[Theorem]] = {
      stepReference.indexes match {
        case Nil =>
          None
        case init :+ last =>
          theorem.tryModifySteps(init, (steps, _) => {
            steps.lift(last).map { step =>
              getReplacementStep(step).map {
                case Some(replacementStep) =>
                  steps.updated(last, replacementStep)
                case None =>
                  steps.take(last) ++ steps.drop(last + 1)
              }
            }
          })
          .mapMap(_.recalculateReferences(parsingContext))
      }
    }
    modifyTheorem(bookKey, chapterKey, theoremKey) { (book, chapter, theorem) =>
      val parsingContext = getTheoremParsingContext(book, chapter, theorem)
      deleteFromTheorem(theorem, parsingContext).orNotFound(s"Step $stepReference").flatten
    }.toResponseEntity
  }

  @PostMapping(value = Array("/{stepReference}/move"))
  def moveStep(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepReference: PathData,
    @RequestParam("direction") direction: String
  ): ResponseEntity[_] = {
    def moveInTheorem(theorem: Theorem, parsingContext: ParsingContext): Option[Try[Theorem]] = {
      stepReference.indexes match {
        case Nil =>
          None
        case Seq(0) if direction == "up" =>
          Some(Failure(BadRequestException("Cannot move step upwards if it's the first step")))
        case init :+ containerIndex :+ 0 if direction == "up" =>
          theorem.tryModifySteps(init, (steps, _) => {
            steps.lift(containerIndex).flatMap { outerStep =>
              outerStep.extractSubstep(0).map(_.orBadRequest(s"Could not extract step $stepReference from outer step"))
            }.mapMap { case (outerStep, innerStep) =>
              steps.take(containerIndex) ++ Seq(innerStep, outerStep) ++ steps.drop(containerIndex + 1)
            }
          })
        case init :+ last =>
          theorem.tryModifySteps(init, (steps, _) => {
            steps.lift(last).map { step =>
              direction match {
                case "up" =>
                  Success((steps.take(last - 1) :+ step :+ steps(last - 1)) ++ steps.drop(last + 1))
                case "down" =>
                  Success((steps.take(last) ++ steps.lift(last + 1).toSeq :+ step) ++ steps.drop(last + 1))
                case _ =>
                  Failure(BadRequestException(s"Unrecognised direction $direction"))
              }
            }
          }).mapMap(_.recalculateReferences(parsingContext))
      }
    }
    modifyTheorem(bookKey, chapterKey, theoremKey) { (book, chapter, theorem) =>
      val parsingContext = getTheoremParsingContext(book, chapter, theorem)
      moveInTheorem(theorem, parsingContext).orNotFound(s"Step $stepReference").flatten
    }.toResponseEntity
  }

  @PostMapping(value = Array("/{stepReference}/introduceBoundVariable"))
  def introduceBoundVariable(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepReference: PathData,
    @RequestBody variableName: String
  ): ResponseEntity[_] = {
    modifyStep[Step.Target](bookKey, chapterKey, theoremKey, stepReference) { (book, chapter, theorem, step, stepContext) =>
      implicit val parsingContext: ParsingContext = getStepParsingContext(book, chapter, theorem, stepContext)
      for {
        (substatement, scopingStatementDefinition) <- parsingContext.matchScopingStatement(step.statement).orBadRequest("Target statement is not a scoped statement")
      } yield {
        Step.ScopedVariable(
          variableName,
          Seq(Step.Target(substatement)),
          scopingStatementDefinition)
      }
    }.toResponseEntity
  }

  @PostMapping(value = Array("/{stepReference}/introduceDeduction"))
  def introduceDeduction(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepReference: PathData
  ): ResponseEntity[_] = {
    modifyStep[Step.Target](bookKey, chapterKey, theoremKey, stepReference) { (book, chapter, theorem, step, stepContext) =>
      implicit val parsingContext: ParsingContext = getStepParsingContext(book, chapter, theorem, stepContext)
      for {
        (antecedent, consequent, deductionStatementDefinition) <- parsingContext.matchDeductionStatement(step.statement).orBadRequest("Target statement is not a deduction statement")
      } yield {
        Step.Deduction(
          antecedent,
          Seq(Step.Target(consequent)),
          deductionStatementDefinition)
      }
    }.toResponseEntity
  }

  @PostMapping(value = Array("/{stepReference}/elide"))
  def elide(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepReference: PathData
  ): ResponseEntity[_] = {
    modifyStep[Step.Target](bookKey, chapterKey, theoremKey, stepReference) { (book, chapter, theorem, step, stepContext) =>
      Success(Step.Elided(Seq(step), None))
    }.toResponseEntity
  }

  @PutMapping(value = Array(
    "/{stepReference}/premises/{premisePath}/statement/{expressionPath}/boundVariables/{boundVariableIndex}",
    "/{stepReference}/premises/{premisePath}/statement/boundVariables/{boundVariableIndex}"))
  def editBoundVariableName(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepPath: PathData,
    @PathVariable("premisePath") premisePath: PathData,
    @PathVariable(value = "expressionPath", required = false) expressionPath: PathData,
    @PathVariable("boundVariableIndex") boundVariableIndex: Int,
    @RequestBody newBoundVariableName: String
  ): ResponseEntity[_] = {
    import Step.NewAssert._
    def updatePremise(oldPremise: Premise): Try[Premise] = {
      for {
        oldPendingPremise <- oldPremise.asOptionalInstanceOf[Premise.Pending].orBadRequest(s"Premise $premisePath is not pending")
        newStatement <- oldPendingPremise.statement.renameBoundVariable(
          newBoundVariableName,
          boundVariableIndex,
          Option(expressionPath).map(_.indexes).getOrElse(Nil)
        ).orNotFound(s"Bound variable $boundVariableIndex at $expressionPath")
      } yield oldPendingPremise.copy(statement = newStatement)
    }
    modifyStep[Step.NewAssert](bookKey, chapterKey, theoremKey, stepPath) { (_, _, _, oldStep, _) =>
      oldStep.tryUpdatePremiseAtPath(premisePath.indexes, updatePremise).orNotFound(s"Premise $premisePath").flatten
    }.toResponseEntity
  }

  @PostMapping(value = Array("/{stepPath}/premises/{premisePath}/target"))
  def createTarget(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData,
    @PathVariable("premisePath") premisePath: PathData
  ): ResponseEntity[_] = {
    modifyTheorem(bookKey, chapterKey, theoremKey) { (book, chapter, theorem) =>
      for {
        (rawStep, _) <- theorem.findStep(stepPath.indexes).orNotFound(s"Step $stepPath")
        step <- rawStep.asOptionalInstanceOf[Step.NewAssert].orBadRequest(s"Step $stepPath is not editable")
        premise <- step.pendingPremises.get(premisePath.indexes).orNotFound(s"Premise $premisePath")
        newStep = Step.Target(premise.statement)
        newTheorem <- theorem.insertStep(stepPath.indexes, newStep).orBadRequest(s"Failed to insert new step")
          .map(_.recalculateReferences(getTheoremParsingContext(book, chapter, theorem)))
      } yield {
        newTheorem
      }
    }.toResponseEntity
  }

  case class QuickSuggestion(inference: Inference.Summary, target: Statement)
  case class PremiseOption(path: Seq[Int], expansions: Seq[Inference.Summary], quick: Seq[QuickSuggestion])
  @GetMapping(value = Array("/{stepReference}/premiseOptions"))
  def premiseOptions(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepReference: PathData
  ): ResponseEntity[_] = {
    findStep[Step.NewAssert](bookKey, chapterKey, theoremKey, stepReference).map { case (book, chapter, theorem, step, stepContext) =>
      val parsingContext = getStepParsingContext(book, chapter, theorem, stepContext)
      val expansionInferences = parsingContext.inferences.filter(_.rearrangementType == Inference.RearrangementType.Expansion)
      val quickInferences = parsingContext.inferences.filter(i => i.requiredSubstitutions isEquivalentTo i.conclusion.requiredSubstitutions).mapCollect(i => i.premises.single.map(i -> _.statement)).reverse
      step.pendingPremises.map { case (path, p) =>
        val quickSuggestions = quickInferences
          .flatMap { case (inference, premiseStatement) =>
            val possibleTargets = inference.conclusion
              .calculateSubstitutions(p.statement, Substitutions.empty, 0, stepContext.externalDepth)
              .mapCollect(s => premiseStatement.applySubstitutions(s, 0, stepContext.externalDepth))
            possibleTargets.map { target => QuickSuggestion(inference.summary, target) }
          }
        val matchingExpansions = expansionInferences.filter(_.conclusion.calculateSubstitutions(p.statement, Substitutions.empty, 0, stepContext.externalDepth).nonEmpty)
        PremiseOption(path, matchingExpansions.map(_.summary), quickSuggestions)
      }
    }.toResponseEntity
  }

  @PostMapping(value = Array("/{stepPath}/premises/{premisePath}/rearrangement"))
  def createRearrangement(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData,
    @PathVariable("premisePath") premisePath: PathData,
    @RequestBody inferenceId: String
  ): ResponseEntity[_] = {
    import Step.NewAssert._
    def updatePremise(oldPremise: Premise, stepContext: StepContext)(implicit parsingContext: ParsingContext): Try[Premise] = {
      for {
        inference <- findInference(inferenceId)
        substitutions <- inference.conclusion.calculateSubstitutions(oldPremise.statement, Substitutions.empty, 0, stepContext.externalDepth).headOption.orException(BadRequestException("Could not calculate substitutions"))
        substitutedInferencePremises <- Try(inference.substitutePremisesAndValidateConclusion(substitutions, oldPremise.statement, stepContext.externalDepth)).recoverWith { case e => Failure(BadRequestException(e.getMessage))}
        newPremises = substitutedInferencePremises.map(createPremise(_, stepContext, parsingContext))
      } yield Step.NewAssert.Premise.Expansion(oldPremise.statement, inference, newPremises, substitutions)
    }

    modifyStep[Step.NewAssert](bookKey, chapterKey, theoremKey, stepPath) { (book, chapter, theorem, oldStep, stepContext) =>
      implicit val parsingContext: ParsingContext = getStepParsingContext(book, chapter, theorem, stepContext)
      oldStep.tryUpdatePremiseAtPath(premisePath.indexes, updatePremise(_, stepContext)).orNotFound(s"Premise $premisePath not found").flatten
    }.toResponseEntity
  }

  @PostMapping(value = Array("/{stepPath}/premises/{premisePath}/quick"))
  def createQuickPremise(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData,
    @PathVariable("premisePath") premisePath: PathData,
    @RequestBody quickDefinition: QuickDefinition
  ): ResponseEntity[_] = {
    import Step.NewAssert._
    def updatePremise(oldPremise: Premise, stepContext: StepContext)(implicit parsingContext: ParsingContext): Try[Premise] = {
      for {
        inference <- findInference(quickDefinition.inferenceId)
        target <- Try(Statement.parser.parseFromString(quickDefinition.target, "")).toOption.orBadRequest("Invalid target statement")
        singlePremise <- inference.premises.single.orBadRequest("Invalid inference for quick premise")
        substitutions <- inference.conclusion.calculateSubstitutions(oldPremise.statement, Substitutions.empty, 0, stepContext.externalDepth)
          .flatMap(s => singlePremise.statement.calculateSubstitutions(target, s, 0, stepContext.externalDepth))
          .headOption.orBadRequest("Could not calculate substitutions")
        substitutedInferencePremises <- Try(inference.substitutePremisesAndValidateConclusion(substitutions, oldPremise.statement, stepContext.externalDepth)).recoverWith { case e => Failure(BadRequestException(e.getMessage))}
        newPremises = substitutedInferencePremises.map(createPremise(_, stepContext, parsingContext))
      } yield Step.NewAssert.Premise.Expansion(oldPremise.statement, inference, newPremises, substitutions)
    }

    modifyStep[Step.NewAssert](bookKey, chapterKey, theoremKey, stepPath) { (book, chapter, theorem, oldStep, stepContext) =>
      implicit val parsingContext: ParsingContext = getStepParsingContext(book, chapter, theorem, stepContext)
      oldStep.tryUpdatePremiseAtPath(premisePath.indexes, updatePremise(_, stepContext)).orNotFound(s"Premise $premisePath not found").flatten
    }.toResponseEntity
  }

  @DeleteMapping(value = Array("/{stepPath}/premises/{premisePath}"))
  def deletePremise(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData,
    @PathVariable("premisePath") premisePath: PathData
  ): ResponseEntity[_] = {
    modifyStep[Step.NewAssert](bookKey, chapterKey, theoremKey, stepPath) { (book, chapter, theorem, oldStep, stepContext) =>
      implicit val parsingContext: ParsingContext = getStepParsingContext(book, chapter, theorem, stepContext)
      oldStep.tryUpdatePremiseAtPath(premisePath.indexes, p => Success(NewAssert.Premise.Pending(p.statement))).orNotFound(s"Premise $premisePath not found").flatten
    }.toResponseEntity
  }

  private def findTheorem(bookKey: String, chapterKey: String, theoremKey: String): Try[(Book, Chapter, Theorem)] = {
    for {
      book <- bookService.books.find(_.key.value == bookKey).orNotFound(s"Book $bookKey")
      chapter <- book.chapters.find(_.key.value == chapterKey).orNotFound(s"Chapter $chapterKey")
      theorem <- chapter.entries.ofType[Theorem].find(_.key.value == theoremKey).orNotFound(s"Theorem $theoremKey")
    } yield (book, chapter, theorem)
  }

  private def findStep[T <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, stepReference: PathData): Try[(Book, Chapter, Theorem, T, StepContext)] = {
    for {
      (book, chapter, theorem) <- findTheorem(bookKey, chapterKey, theoremKey)
      (rawStep, stepContext) <- theorem.findStep(stepReference.indexes).orNotFound(s"Step $stepReference")
      step <- rawStep.asOptionalInstanceOf[T].orBadRequest(s"Step was not ${classTag[T].runtimeClass.getName}")
    } yield {
      (book, chapter, theorem, step, stepContext)
    }
  }

  private def modifyTheorem(bookKey: String, chapterKey: String, theoremKey: String)(f: (Book, Chapter, Theorem) => Try[Theorem]): Try[Theorem] = {
    bookService.modifyEntry[Theorem, Theorem](bookKey, chapterKey, theoremKey, (_, book, chapter, theorem) =>
      f(book, chapter, theorem)
    ).map(_._4)
  }

  private def modifyStep[TStep <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, stepReference: PathData)(f: (Book, Chapter, Theorem, TStep, StepContext) => Try[Step]): Try[Theorem] = {
    modifyTheorem(bookKey, chapterKey, theoremKey) { (book, chapter, theorem) =>
      theorem.tryModifyStep(stepReference.indexes, (oldStep, stepContext) => {
        for {
          oldTStep <- oldStep.asOptionalInstanceOf[TStep].orBadRequest(s"Step was not ${classTag[TStep].runtimeClass.getSimpleName}")
          newStep <- f(book, chapter, theorem, oldTStep, stepContext)
        } yield newStep
      }).orNotFound(s"Step $stepReference").flatten
    }
  }

  private def getStepParsingContext(book: Book, chapter: Chapter, theorem: Theorem, stepContext: StepContext): ParsingContext = {
    getTheoremParsingContext(book, chapter, theorem).copy(parameterLists = stepContext.boundVariableLists.map(_.zipWithIndex))
  }

  private def findInference(inferenceId: String)(implicit parsingContext: ParsingContext): Try[Inference.Summary] = {
    parsingContext.inferences.find(_.id == inferenceId).map(_.summary).orBadRequest(s"Invalid inference $inferenceId")
  }

  private def createPremise(target: Statement, stepContext: StepContext, parsingContext: ParsingContext): NewAssert.Premise = {
    ProofHelper.findPremise(target, stepContext, parsingContext).getOrElse(NewAssert.Premise.Pending(target))
  }

  case class PathData(indexes: Seq[Int]) {
   override def toString: String = indexes.mkString(".")
  }
  @Component
  class StepReferenceConverter extends Converter[String, PathData] {
    override def convert(source: String): PathData = {
      PathData(source.split('.').map(_.toInt))
    }
  }
}
