package net.prover

import net.prover.books.model.Book
import net.prover.books.reading.ProofFileReader
import net.prover.entries._
import net.prover.model.definitions.Definitions
import net.prover.model.entries.{ChapterEntry, Theorem}
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Step, StepContext, StepProvingContext, StepReference}
import net.prover.model.{AvailableEntries, Chapter, ProvingContext, VariableDefinitions}
import org.mockito.Mockito.when

import scala.reflect.ClassTag

trait ContextHelper extends MockitoHelpers {

  val bookKey = "test-book-key"
  val chapterKey = "test-chapter-key"
  val theoremKey = "test-theorem-key"
  val proofIndex = 3
  val outerStepPath = Seq(3, 1, 4, 1)
  val stepIndex = 5
  val stepPath = outerStepPath :+ stepIndex

  def createBaseStepContext(premises: Seq[Statement])(using variableDefinitions: VariableDefinitions, availableEntries: AvailableEntries): StepContext = {
    StepContext.withPremisesAndVariables(premises, variableDefinitions)
  }

  def createBaseStepContext(premises: Seq[Statement], boundVariables: Seq[String])(using variableDefinitions: VariableDefinitions, availableEntries: AvailableEntries): StepContext = {
    val baseContext = createBaseStepContext(premises)
    boundVariables.foldLeft(baseContext) { case (context, variable) => context.addBoundVariable(variable) }
  }

  def createBaseStepContext(premises: Seq[Statement], depth: Int)(using variableDefinitions: VariableDefinitions, availableEntries: AvailableEntries): StepContext = {
    createBaseStepContext(premises, (0 until depth).map(i => s"x_$i"))
  }

  def createOuterStepContext(
    boundVariables: Seq[String])(
    using variableDefinitions: VariableDefinitions,
    availableEntries: AvailableEntries
  ): StepContext = {
    createBaseStepContext(Nil, boundVariables).copy(parentReference = StepReference(outerStepPath))
  }

  def createTargetStepWithContext(
    statement: Statement)(
    using outerStepContext: StepContext,
    availableEntries: AvailableEntries
  ): TypedStepWithContext[Step.TargetStep] = {
    createStepWithContext(Step.TargetStep(statement))
  }

  def createStepWithContext[T <: Step : ClassTag](
    step: T)(
    using outerStepContext: StepContext,
    availableEntries: AvailableEntries
  ): TypedStepWithContext[T] = {
    createStepsWithContext(Seq(step)).atChild(Nil, step)
  }

  def createStepsWithContext(
    steps: Seq[Step])(
    using outerStepContext: StepContext,
    availableEntries: AvailableEntries
  ): StepsWithContext = {
    val provingContext = availableEntriesToProvingContext
    val proofWithContext = mock[ProofWithContext]
    proofWithContext.provingContext returns provingContext
    proofWithContext.availableEntries returns availableEntries
    proofWithContext.theoremWithContext returns mock[TheoremWithContext]
    proofWithContext.theoremWithContext.theorem returns mock[Theorem]
    proofWithContext.theoremWithContext.theorem.title returns "Theorem"
    proofWithContext.theoremWithContext.chapterWithContext returns createChapterWithContext

    StepsWithContext(
      steps,
      outerStepContext,
      proofWithContext)
  }

  given availableEntriesToProvingContext(using availableEntries: AvailableEntries): ProvingContext = ProvingContext(availableEntries, new Definitions(availableEntries))
  given availableEntriesToStepProvingContext(using availableEntries: AvailableEntries, stepContext: StepContext): StepProvingContext = new StepProvingContext()

  def createAvailableEntries(entries: Seq[ChapterEntry]): AvailableEntries = {
    val entriesWithContext = entries.map(createEntryWithContext(_)(using null))
    val availableEntries = AvailableEntries(entriesWithContext)
    entriesWithContext.foreach(e => {
      e.availableEntries returns availableEntries
      e.provingContext returns availableEntriesToProvingContext(using availableEntries)
    })
    availableEntries
  }

  def createGlobalContext(using availableEntries: AvailableEntries): GlobalContext = {
    val globalContext = mock[GlobalContext]
    globalContext.definitions returns Definitions(availableEntries)
    globalContext
  }

  def createEntryWithContext[T <: ChapterEntry](entry: T)(using availableEntries: AvailableEntries): TypedEntryWithContext[T] = {
    val entryWithContext = mock[TypedEntryWithContext[T]]
    entryWithContext.entry returns entry
    entryWithContext.availableEntries returns availableEntries
    entryWithContext.provingContext returns availableEntriesToProvingContext
    entryWithContext.globalContext returns createGlobalContext
    entryWithContext
  }

  def createTheoremWithContext(theorem: Theorem)(using availableEntries: AvailableEntries): TheoremWithContext = {
    val theoremWithContext = createEntryWithContext(theorem)
    theoremWithContext.theorem returns theorem
    when(theoremWithContext.proofsWithContext).thenCallRealMethod()
    theoremWithContext
  }

  given createChapterWithContext(using availableEntries: AvailableEntries): ChapterWithContext = {
    val chapterWithContext = mock[ChapterWithContext]
    chapterWithContext.chapter returns mock[Chapter]
    chapterWithContext.chapter.title returns "Test Chapter"
    chapterWithContext.chapterKey returns chapterKey
    chapterWithContext.bookWithContext returns mock[BookWithContext]
    chapterWithContext.bookWithContext.book returns mock[Book]
    chapterWithContext.bookWithContext.book.title returns "Test Book"
    chapterWithContext.bookWithContext.bookKey returns bookKey
    chapterWithContext.globalContext returns mock[GlobalContext]
    chapterWithContext.globalContext.definitions returns new Definitions(availableEntries)
    chapterWithContext
  }

  given createEntryParsingContext(using availableEntries: AvailableEntries): EntryParsingContext = {
    EntryParsingContext(
      "Book",
      "Chapter",
      mock[ProofFileReader])(
      using availableEntries)
  }
}
