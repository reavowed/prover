package net.prover.controllers

import net.prover.controllers.models.PathData
import net.prover.model.{Book, Chapter, Inference, ParsingContext}
import net.prover.model.entries.Theorem
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, ProofHelper, Step, StepContext}

import scala.reflect.{ClassTag, classTag}
import scala.util.Try

trait BookModification {
  def bookService: BookService

  protected def findTheorem(bookKey: String, chapterKey: String, theoremKey: String): Try[(Book, Chapter, Theorem)] = {
    for {
      book <- bookService.books.find(_.key.value == bookKey).orNotFound(s"Book $bookKey")
      chapter <- book.chapters.find(_.key.value == chapterKey).orNotFound(s"Chapter $chapterKey")
      theorem <- chapter.entries.ofType[Theorem].find(_.key.value == theoremKey).orNotFound(s"Theorem $theoremKey")
    } yield (book, chapter, theorem)
  }

  protected def findStep[T <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, stepPath: PathData): Try[(Book, Chapter, Theorem, T, StepContext)] = {
    for {
      (book, chapter, theorem) <- findTheorem(bookKey, chapterKey, theoremKey)
      (rawStep, stepContext) <- theorem.findStep(stepPath.indexes).orNotFound(s"Step $stepPath")
      step <- rawStep.asOptionalInstanceOf[T].orBadRequest(s"Step was not ${classTag[T].runtimeClass.getName}")
    } yield {
      (book, chapter, theorem, step, stepContext)
    }
  }

  protected def modifyTheorem(bookKey: String, chapterKey: String, theoremKey: String)(f: (Theorem, ParsingContext) => Try[Theorem]): Try[Theorem] = {
    bookService.modifyEntry[Theorem, Theorem](bookKey, chapterKey, theoremKey, (_, book, chapter, theorem) => {
      val parsingContext = getTheoremParsingContext(book, chapter, theorem)
      f(theorem, parsingContext).map(_.recalculateReferences(parsingContext))
    }).map(_._4)
  }

  protected def modifyStep[TStep <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, stepPath: PathData)(f: (TStep, StepContext, ParsingContext) => Try[Step]): Try[Theorem] = {
    modifyTheorem(bookKey, chapterKey, theoremKey) { (theorem, parsingContext) =>
      theorem.tryModifyStep(stepPath.indexes, (step, stepContext) => {
        for {
          typedStep <- step.asOptionalInstanceOf[TStep].orBadRequest(s"Step was not ${classTag[TStep].runtimeClass.getSimpleName}")
          newStep <- f(typedStep, stepContext, parsingContext.copy(parameterLists = stepContext.boundVariableLists.map(_.zipWithIndex)))
        } yield newStep
      }).orNotFound(s"Step $stepPath").flatten
    }
  }

  protected def replaceStep[TStep <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, stepPath: PathData)(f: (TStep, StepContext, ParsingContext) => Try[Seq[Step]]): Try[Theorem] = {
    modifyTheorem(bookKey, chapterKey, theoremKey) { (theorem, parsingContext) =>
        (stepPath.indexes match {
          case init :+ last =>
            theorem.tryModifySteps(init, (steps, outerContext) => {
              steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
                for {
                  typedStep <- step.asOptionalInstanceOf[TStep].orBadRequest(s"Step was not ${classTag[TStep].runtimeClass.getSimpleName}")
                  replacementSteps <- f(typedStep, outerContext.addSteps(before), parsingContext.copy(parameterLists = outerContext.boundVariableLists.map(_.zipWithIndex)))
                } yield before ++ replacementSteps ++ after
              }
            })
          case _ =>
            None
        }).orNotFound(s"Step $stepPath").flatten
    }
  }

  protected def getStepParsingContext(book: Book, chapter: Chapter, theorem: Theorem, stepContext: StepContext): ParsingContext = {
    getTheoremParsingContext(book, chapter, theorem).copy(parameterLists = stepContext.boundVariableLists.map(_.zipWithIndex))
  }

  protected def findInference(inferenceId: String)(implicit parsingContext: ParsingContext): Try[Inference.Summary] = {
    parsingContext.inferences.find(_.id == inferenceId).map(_.summary).orBadRequest(s"Invalid inference $inferenceId")
  }

  protected def createPremise(target: Statement, stepContext: StepContext, parsingContext: ParsingContext): Premise = {
    ProofHelper.findPremise(target, stepContext, parsingContext)
  }
}
