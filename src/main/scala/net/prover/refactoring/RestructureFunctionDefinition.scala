package net.prover.refactoring

import net.prover.books.keys.ListWithKeys
import net.prover.books.management.{BookStateManager, UpdateBooks}
import net.prover.books.model.Book
import net.prover.entries.*
import net.prover.model.*
import net.prover.model.definitions.{Definitions, ExpressionDefinition, StatementDefinition, TermDefinition}
import net.prover.model.entries.*
import net.prover.model.entries.Theorem.Proof
import net.prover.model.expressions.*
import net.prover.model.proof.{Premise, Step, StepContext, SubstitutionContext}
import net.prover.proving.structure.definitions.{ConjunctionDefinition, DeductionDefinition, GeneralizationDefinition}
import net.prover.theorems.CompoundTheoremUpdater
import scalaz.Id.Id

import scala.collection.mutable
import scala.util.Try

/**
  * One-off refactoring: replaces
  *
  *   type function f (definition: pair set with unique values on its domain)
  *   qualifier from on function (A B) (from A → B)
  *
  * with a single type whose default qualifier is (A B) (from A → B), defined by the right-hand side of
  * "Subset Condition for Function From".
  *
  * Statements are rewritten as follows:
  *  - `∧ function X functionFrom X A B` → `function X A B`
  *  - `functionFrom X A B` on its own → `function X A B`
  *  - bare `function X` / `injectiveFunction X` → `function X A B` / `injectiveFunction X A B`, where A and B come from a
  *    sibling conjunct (`functionFrom`, `surjectiveFunction` or `bijectiveFunction` on the same X), or failing that
  *    from a unique such statement about X anywhere in the same theorem and its proofs, or failing that
  *    `domain X` and `codomain X`.
  *
  * Every proof step is then re-validated by parsing it against the new definitions; any step that fails becomes a
  * target with the same (rewritten) statement.
  */
object RestructureFunctionDefinition {
  case class Report(lines: Seq[String]) {
    override def toString: String = lines.mkString("\n")
  }

  def apply()(implicit bookStateManager: BookStateManager): Report = {
    var report: Report = null
    UpdateBooks[Id](globalContext => {
      val (books, r) = run(globalContext)
      report = r
      books
    })
    report
  }

  def run(oldGlobalContext: GlobalContext): (Seq[Book], Report) = {
    val rework = new Rework(oldGlobalContext)
    val newBooks = mutable.ArrayBuffer.empty[Book]
    for (oldBookWithContext <- oldGlobalContext.booksWithContexts) {
      println("- " + oldBookWithContext.book.title)
      val newChapters = mutable.ArrayBuffer.empty[Chapter]
      for (oldChapterWithContext <- oldBookWithContext.chaptersWithContexts) {
        println("  - " + oldChapterWithContext.chapter.title)
        val newEntries = mutable.ArrayBuffer.empty[ChapterEntry]
        for (oldEntryWithContext <- oldChapterWithContext.entriesWithContexts) {
          val chapterSoFar = oldChapterWithContext.chapter.setEntries(newEntries.toList)
          val bookSoFar = oldBookWithContext.book.setChapters((newChapters :+ chapterSoFar).toList)
          val globalContextSoFar = GlobalContext(ListWithKeys((newBooks :+ bookSoFar).toList))
          val bookWithContext = BookWithContext(bookSoFar, oldBookWithContext.bookKey, globalContextSoFar)
          val chapterWithContext = ChapterWithContext(chapterSoFar, oldChapterWithContext.chapterKey, bookWithContext)
          // The old entry, positioned in the new world after all the entries rewritten so far
          val entryInNewWorld = TypedEntryWithContext[ChapterEntry](oldEntryWithContext.entry, oldEntryWithContext.entryKey, chapterWithContext)
          rework.transformEntry(oldEntryWithContext, entryInNewWorld).foreach(newEntries += _)
        }
        newChapters += oldChapterWithContext.chapter.setEntries(newEntries.toList)
      }
      newBooks += oldBookWithContext.book.setChapters(newChapters.toList)
    }
    (newBooks.toSeq, rework.report)
  }

  private type Candidates = Map[Term, (Term, Term)]

  private class Rework(oldGlobalContext: GlobalContext) {
    private val allOldEntries = oldGlobalContext.allEntries

    private def findOld[T <: ChapterEntry](description: String)(f: PartialFunction[ChapterEntry, T]): T = {
      val matches = allOldEntries.map(_.entry).collect(f)
      matches match {
        case Seq(single) => single
        case _ => throw new Exception(s"Expected exactly one $description, found ${matches.length}")
      }
    }

    private val oldFunctionType = findOld("function type") { case t: TypeDefinition if t.symbol == "function" => t }
    private val oldFromQualifier = findOld("from qualifier") { case q: TypeQualifierDefinition if q.parentType == oldFunctionType && q.symbol == "from" => q }
    private val oldInjective = findOld("injective property") { case p: PropertyDefinitionOnType if p.parentType == oldFunctionType && p.symbol == "injective" => p }
    private val oldSurjective = findOld("surjective property") { case p: PropertyDefinitionOnType if p.parentType == oldFunctionType && p.symbol == "surjective" => p }
    private val oldBijective = findOld("bijective property") { case p: PropertyDefinitionOnType if p.parentType == oldFunctionType && p.symbol == "bijective" => p }
    private val oldDomain = findOld("domain term") { case t: TermDefinitionEntry if t.baseSymbol == "domain" && t.disambiguator.isEmpty => t }
    private val oldCodomain = findOld("codomain term") { case t: TermDefinitionEntry if t.baseSymbol == "codomain" && t.disambiguator.isEmpty => t }
    private val oldSubsetCondition = findOld("subset condition theorem") { case t: Theorem if t.name == "Subset Condition for Function From" => t }

    private val oldFunctionDefinition: StatementDefinition = oldFunctionType.statementDefinition
    private val oldFromDefinition: StatementDefinition = oldFromQualifier.statementDefinition
    private val oldInjectiveDefinition: StatementDefinition = oldInjective.statementDefinition
    private val qualifierArgumentDefinitions: Set[StatementDefinition] = Set(oldFromDefinition, oldSurjective.statementDefinition, oldBijective.statementDefinition)
    private val oldConjunctionDefinition: StatementDefinition = allOldEntries
      .find(_.entry == oldFunctionType).get.availableEntries.conjunctionDefinitionOption.get.statementDefinition
    private val specialEntries: Set[ChapterEntry] = Set(oldFunctionType, oldFromQualifier, oldInjective, oldSurjective, oldBijective)

    private val entryMap = mutable.Map.empty[ChapterEntry, ChapterEntry]
    private val definitionMap = mutable.Map.empty[ExpressionDefinition, ExpressionDefinition]
    private val inferenceMap = mutable.Map.empty[String, Inference.Summary]
    private var newFunctionDefinition: StatementDefinition = null

    private val reportLines = mutable.LinkedHashSet.empty[String]
    private var currentLabel: String = ""
    def report: Report = Report(reportLines.toSeq)

    // ---------------------------------------------------------------------------------------------------------------
    // Expression rewriting
    // ---------------------------------------------------------------------------------------------------------------

    private def newStatementDefinition(old: StatementDefinition): StatementDefinition = {
      definitionMap.getOrElse(old, throw new Exception(s"No replacement for statement definition ${old.symbol} ($currentLabel)")).asInstanceOf[StatementDefinition]
    }
    private def newTermDefinition(old: TermDefinition): TermDefinition = {
      definitionMap.getOrElse(old, throw new Exception(s"No replacement for term definition ${old.symbol} ($currentLabel)")).asInstanceOf[TermDefinition]
    }

    private def isClosed(expression: Expression): Boolean = expression match {
      case _: FunctionParameter => false
      case definedExpression: DefinedExpression[?] => definedExpression.components.forall(isClosed)
      case StatementVariable(_, arguments) => arguments.forall(isClosed)
      case TermVariable(_, arguments) => arguments.forall(isClosed)
      case _ => true
    }

    private def isOldConjunction(statement: Statement): Boolean = statement match {
      case DefinedStatement(Seq(_: Statement, _: Statement), `oldConjunctionDefinition`) => true
      case _ => false
    }

    private def directCandidate(statement: Statement): Option[(Term, (Term, Term))] = statement match {
      case DefinedStatement(Seq(x: Term, a: Term, b: Term), definition) if qualifierArgumentDefinitions.contains(definition) =>
        Some(x -> (a, b))
      case _ =>
        None
    }

    private def uniqueCandidates(candidates: Seq[(Term, (Term, Term))]): Candidates = {
      candidates.groupBy(_._1).view.mapValues(_.map(_._2).distinct).collect { case (x, Seq(single)) => x -> single }.toMap
    }

    private def conjunctionLeaves(statement: Statement): Seq[Statement] = statement match {
      case DefinedStatement(Seq(l: Statement, r: Statement), `oldConjunctionDefinition`) => conjunctionLeaves(l) ++ conjunctionLeaves(r)
      case other => Seq(other)
    }

    private def allSubStatements(expression: Expression): Seq[Statement] = {
      val children = expression match {
        case definedExpression: DefinedExpression[?] => definedExpression.components.flatMap(allSubStatements)
        case StatementVariable(_, arguments) => arguments.flatMap(allSubStatements)
        case TermVariable(_, arguments) => arguments.flatMap(allSubStatements)
        case _ => Nil
      }
      expression match {
        case statement: Statement => statement +: children
        case _ => children
      }
    }

    def closedCandidates(expressions: Seq[Expression]): Candidates = {
      uniqueCandidates(
        expressions.flatMap(allSubStatements).flatMap(directCandidate).filter { case (x, (a, b)) => isClosed(x) && isClosed(a) && isClosed(b) })
    }

    private def newFunction(x: Term, a: Term, b: Term): Statement = DefinedStatement(Seq(x, a, b), newFunctionDefinition)(Nil)

    private def qualifierArguments(x: Term, candidates: Candidates, description: String): (Term, Term) = {
      candidates.get(x) match {
        case Some((a, b)) =>
          (term(a, candidates), term(b, candidates))
        case None =>
          val newX = term(x, candidates)
          reportLines += s"    FALLBACK $currentLabel: $description ${newX.serialized} -> domain/codomain"
          (DefinedTerm(Seq(newX), newTermDefinition(oldDomain))(Nil), DefinedTerm(Seq(newX), newTermDefinition(oldCodomain))(Nil))
      }
    }

    def statement(statement: Statement, candidates: Candidates): Statement = statement match {
      case DefinedStatement(Seq(l: Statement, r: Statement), `oldConjunctionDefinition`) =>
        (l, r) match {
          case (DefinedStatement(Seq(x: Term), `oldFunctionDefinition`), DefinedStatement(Seq(x2: Term, a: Term, b: Term), `oldFromDefinition`)) if x == x2 =>
            newFunction(term(x, candidates), term(a, candidates), term(b, candidates))
          case _ =>
            val localCandidates = candidates ++ uniqueCandidates(conjunctionLeaves(statement).flatMap(directCandidate))
            DefinedStatement(
              Seq(this.statement(l, localCandidates), this.statement(r, localCandidates)),
              newStatementDefinition(oldConjunctionDefinition))(Nil)
        }
      case DefinedStatement(Seq(x: Term), `oldFunctionDefinition`) =>
        val (a, b) = qualifierArguments(x, candidates, "function")
        newFunction(term(x, candidates), a, b)
      case DefinedStatement(Seq(x: Term, a: Term, b: Term), `oldFromDefinition`) =>
        newFunction(term(x, candidates), term(a, candidates), term(b, candidates))
      case DefinedStatement(Seq(x: Term), `oldInjectiveDefinition`) =>
        val (a, b) = qualifierArguments(x, candidates, "injectiveFunction")
        DefinedStatement(Seq(term(x, candidates), a, b), newStatementDefinition(oldInjectiveDefinition))(Nil)
      case definedStatement @ DefinedStatement(components, definition) =>
        val innerCandidates = if (definedStatement.boundVariableNames.nonEmpty) candidates.filter { case (x, (a, b)) => isClosed(x) && isClosed(a) && isClosed(b) } else candidates
        DefinedStatement(components.map(expression(_, innerCandidates)), newStatementDefinition(definition))(definedStatement.boundVariableNames)
      case StatementVariable(index, arguments) =>
        StatementVariable(index, arguments.map(term(_, candidates)))
      case other =>
        throw new Exception(s"Unexpected statement type ${other.getClass.getName}")
    }

    def term(term: Term, candidates: Candidates): Term = term match {
      case definedTerm @ DefinedTerm(components, definition) =>
        val innerCandidates = if (definedTerm.boundVariableNames.nonEmpty) candidates.filter { case (x, (a, b)) => isClosed(x) && isClosed(a) && isClosed(b) } else candidates
        DefinedTerm(components.map(expression(_, innerCandidates)), newTermDefinition(definition))(definedTerm.boundVariableNames)
      case TermVariable(index, arguments) =>
        TermVariable(index, arguments.map(this.term(_, candidates)))
      case parameter: FunctionParameter =>
        parameter
      case other =>
        throw new Exception(s"Unexpected term type ${other.getClass.getName}")
    }

    def expression(expression: Expression, candidates: Candidates): Expression = expression match {
      case s: Statement => statement(s, candidates)
      case t: Term => term(t, candidates)
    }

    def substitutions(substitutions: Substitutions, candidates: Candidates): Substitutions = {
      Substitutions(substitutions.statements.map(statement(_, candidates)), substitutions.terms.map(term(_, candidates)))
    }

    def premise(premise: Premise, candidates: Candidates): Premise = premise match {
      case Premise.Pending(s) => Premise.Pending(statement(s, candidates))
      case Premise.Given(s, reference) => Premise.Given(statement(s, candidates), reference)
      case Premise.Simplification(s, inner, inference, simplificationSubstitutions, path) =>
        Premise.Simplification(
          statement(s, candidates),
          this.premise(inner, candidates).asInstanceOf[Premise.SingleLinePremise],
          this.inference(inference),
          substitutions(simplificationSubstitutions, candidates),
          path)
    }

    /** Unmapped inferences (the old definitions of function / function from) are left as-is, and so will fail validation. */
    def inference(inference: Inference.Summary): Inference.Summary = inferenceMap.getOrElse(inference.id, inference)

    // ---------------------------------------------------------------------------------------------------------------
    // Entries
    // ---------------------------------------------------------------------------------------------------------------

    def transformEntry(oldEntryWithContext: EntryWithContext, entryInNewWorld: EntryWithContext): Option[ChapterEntry] = {
      val oldEntry = oldEntryWithContext.entry
      currentLabel = s"${oldEntryWithContext.chapterWithContext.chapter.title} / ${oldEntry.name}"
      lazy val newAvailableEntries = entryInNewWorld.availableEntries
      lazy val newConjunctionDefinition: ConjunctionDefinition = newAvailableEntries.conjunctionDefinitionOption.get

      def newParentTypeConditions(old: ParentTypeConditions): ParentTypeConditions = {
        if (old.parentType == oldFunctionType) {
          if (old.termListAdapter.nonEmpty) throw new Exception(s"Unexpected term list adapter on $currentLabel")
          ParentTypeConditions(
            entryMap(oldFunctionType).asInstanceOf[TypeDefinition],
            None,
            old.requiredParentObjects.map(_.replaceDefinitions(entryMap.toMap, newAvailableEntries)),
            None,
            newConjunctionDefinition)
        } else {
          old.replaceDefinitions(entryMap.toMap, definitionMap.toMap, newAvailableEntries)
        }
      }

      val newEntryOption: Option[ChapterEntry] = oldEntry match {
        case `oldFromQualifier` =>
          None
        case `oldFunctionType` =>
          val definingStatement = oldSubsetCondition.conclusion match {
            case DefinedStatement(Seq(_, rhs: Statement), _) => statement(rhs, Map.empty)
            case other => throw new Exception(s"Unexpected subset condition $other")
          }
          Some(TypeDefinition(
            oldFunctionType.symbol,
            oldFunctionType.mainVariableDefinition,
            Some(oldFromQualifier.qualifier),
            oldFunctionType.explicitName,
            definingStatement))
        case theorem: Theorem =>
          Some(transformTheorem(oldEntryWithContext.asInstanceOf[TheoremWithContext], entryInNewWorld.asInstanceOf[TheoremWithContext]))
        case axiom: Axiom =>
          val candidates = closedCandidates(axiom.premises :+ axiom.conclusion)
          Some(Axiom(axiom.name, axiom.variableDefinitions, axiom.premises.map(statement(_, candidates)), statement(axiom.conclusion, candidates)))
        case termDefinition: TermDefinitionEntry =>
          val candidates = closedCandidates(termDefinition.premises :+ termDefinition.definitionPredicate)
          Some(termDefinition.copy(
            premises = termDefinition.premises.map(statement(_, candidates)),
            definitionPredicate = statement(termDefinition.definitionPredicate, candidates),
            disambiguatorAdders = termDefinition.disambiguatorAdders.map(_.replaceDefinitions(definitionMap.toMap))))
        case statementDefinition: StatementDefinitionEntry =>
          Some(statementDefinition.copy(definingStatement = statementDefinition.definingStatement.map(statement(_, Map.empty))))
        case typeDefinition: TypeDefinition =>
          Some(typeDefinition.copy(definingStatement = statement(typeDefinition.definingStatement, Map.empty)))
        case qualifierDefinition: TypeQualifierDefinition =>
          Some(TypeQualifierDefinition(
            qualifierDefinition.symbol,
            entryMap(qualifierDefinition.parentType).asInstanceOf[TypeDefinition],
            qualifierDefinition.qualifier,
            qualifierDefinition.explicitName,
            statement(qualifierDefinition.definingStatement, Map.empty),
            newConjunctionDefinition))
        case property: PropertyDefinitionOnType =>
          Some(PropertyDefinitionOnType(
            property.symbol,
            newParentTypeConditions(property.parentTypeConditions),
            property.explicitName,
            statement(property.definingStatement, Map.empty)))
        case property: StandalonePropertyDefinition =>
          Some(property.copy(definingStatement = statement(property.definingStatement, Map.empty)))
        case relatedObject: RelatedObjectDefinition =>
          Some(RelatedObjectDefinition(
            relatedObject.symbol,
            relatedObject.mainVariableDefinition,
            newParentTypeConditions(relatedObject.parentTypeConditions),
            relatedObject.explicitName,
            statement(relatedObject.definingStatement, Map.empty)))
        case other =>
          if (other.referencedEntries.exists(specialEntries.contains)) {
            throw new Exception(s"Unhandled entry type ${other.getClass.getSimpleName} referencing function definitions: $currentLabel")
          }
          Some(other.replaceDefinitions(entryMap.toMap, definitionMap.toMap, entryInNewWorld))
      }
      newEntryOption.foreach(recordEntry(oldEntry, _))
      newEntryOption
    }

    private def recordEntry(oldEntry: ChapterEntry, newEntry: ChapterEntry): Unit = {
      entryMap += (oldEntry -> newEntry)
      (AvailableEntries.getStatementDefinitionFromEntry(oldEntry), AvailableEntries.getStatementDefinitionFromEntry(newEntry)) match {
        case (Some(_), Some(newDefinition)) if oldEntry == oldFunctionType =>
          newFunctionDefinition = newDefinition
        case (Some(oldDefinition), Some(newDefinition)) =>
          definitionMap += (oldDefinition -> newDefinition)
        case _ =>
      }
      (oldEntry, newEntry) match {
        case (oldTerm: TermDefinitionEntry, newTerm: TermDefinitionEntry) => definitionMap += (oldTerm -> newTerm)
        case _ =>
      }
      if (oldEntry != oldFunctionType) {
        if (oldEntry.inferences.length == newEntry.inferences.length) {
          oldEntry.inferences.zip(newEntry.inferences).foreach { case (oldInference, newInference) =>
            inferenceMap += (oldInference.id -> Inference.Summary(newInference))
          }
        } else {
          reportLines += s"  WARNING: inference count changed for $currentLabel"
        }
      }
    }

    // ---------------------------------------------------------------------------------------------------------------
    // Theorems and proofs
    // ---------------------------------------------------------------------------------------------------------------

    private def transformTheorem(oldTheoremWithContext: TheoremWithContext, theoremInNewWorld: TheoremWithContext): Theorem = {
      val oldTheorem = oldTheoremWithContext.theorem
      val candidates = collectCandidates(oldTheoremWithContext)
      val newAvailableEntries = theoremInNewWorld.availableEntries
      implicit val newProvingContext: ProvingContext = ProvingContext(newAvailableEntries, Definitions(newAvailableEntries))

      // Traverse the old theorem's steps in the old world - the step contexts only matter for their structure and bound
      // variables, and using the old world avoids recalculating definitions for every theorem
      val rewritten = new ProofRewriter(candidates, newProvingContext).apply(oldTheoremWithContext)
      val validatedProofs = rewritten.proofs.zip(oldTheorem.proofs).zipWithIndex.map { case ((proof, oldProof), index) =>
        val initialContext = StepContext.withPremisesAndVariables(rewritten.premises, rewritten.variableDefinitions)
        val validatedSteps = revalidateSteps(proof.steps, initialContext)
        val stepsWithConclusion = if (validatedSteps.lastOption.exists(_.statement == rewritten.conclusion)) validatedSteps else {
          reportLines += s"    $currentLabel proof ${index + 1}: final statement no longer matches conclusion - appending target"
          validatedSteps :+ Step.TargetStep(rewritten.conclusion)
        }
        val newTargets = countTargets(stepsWithConclusion) - countTargets(oldProof.steps)
        if (newTargets > 0) {
          reportLines += s"  $currentLabel${if (rewritten.proofs.length > 1) s" (proof ${index + 1})" else ""}: $newTargets new target(s)"
        }
        Proof(stepsWithConclusion)
      }
      rewritten.copy(proofs = validatedProofs)
    }

    private def countTargets(steps: Seq[Step]): Int = steps.map {
      case _: Step.TargetStep => 1
      case step: Step.WithSubsteps => countTargets(step.substeps)
      case _ => 0
    }.sum

    private def collectCandidates(theoremWithContext: TheoremWithContext): Candidates = {
      val expressions = mutable.ArrayBuffer.empty[Expression]
      object Collector extends CompoundTheoremUpdater[Id] {
        override def updateStatement(statement: Statement, substitutionContext: SubstitutionContext): Statement = {
          expressions += statement
          statement
        }
        override def updateSubstitutions(substitutions: Substitutions, stepWithContext: StepWithContext): Substitutions = {
          expressions ++= substitutions.statements
          expressions ++= substitutions.terms
          substitutions
        }
        override def updatePremise(premise: Premise, stepWithContext: StepWithContext): Premise = {
          expressions += premise.statement
          premise
        }
        override def updateAutogenerated(step: Step.Autogenerated, stepWithContext: StepWithContext): Id[Step] = {
          this.apply(stepWithContext.forSubsteps(step))
          step
        }
      }
      Collector(theoremWithContext)
      closedCandidates(expressions.toSeq)
    }

    private class ProofRewriter(candidates: Candidates, newProvingContext: ProvingContext) extends CompoundTheoremUpdater[Id] {
      override def updateStatement(statement: Statement, substitutionContext: SubstitutionContext): Statement = {
        Rework.this.statement(statement, candidates)
      }
      override def updateInference(inference: Inference.Summary, stepWithContext: StepWithContext): Inference.Summary = {
        Rework.this.inference(inference)
      }
      override def updatePremise(premise: Premise, stepWithContext: StepWithContext): Premise = {
        Rework.this.premise(premise, candidates)
      }
      override def updateSubstitutions(substitutions: Substitutions, stepWithContext: StepWithContext): Substitutions = {
        Rework.this.substitutions(substitutions, candidates)
      }
      override def updateDeductionDefinition(deductionDefinition: DeductionDefinition): DeductionDefinition = {
        newProvingContext.availableEntries.deductionDefinitionOption.get
      }
      override def updateGeneralizationDefinition(generalizationDefinition: GeneralizationDefinition): GeneralizationDefinition = {
        newProvingContext.availableEntries.generalizationDefinitionOption.get
      }
      override def updateAssertion(step: Step.AssertionStep, stepWithContext: StepWithContext): Step = {
        val updated = super.updateAssertion(step, stepWithContext).asInstanceOf[Step.AssertionStep]
        implicit val substitutionContext: SubstitutionContext = stepWithContext.stepContext
        fixSubstitutions(updated.inference, updated.premises.map(_.statement), updated.statement, updated.substitutions)
          .map(s => updated.copy(substitutions = s))
          .getOrElse(updated)
      }
      override def updateNaming(step: Step.NamingStep, stepWithContext: StepWithContext): Step = {
        val updated = super.updateNaming(step, stepWithContext).asInstanceOf[Step.NamingStep]
        implicit val substitutionContext: SubstitutionContext = stepWithContext.stepContext
        val internalPremise = updated.generalizationDefinition(updated.variableName, updated.deductionDefinition(updated.assumption, updated.substeps.last.statement))
        fixSubstitutions(updated.inference, updated.premises.map(_.statement) :+ internalPremise, updated.statement, updated.substitutions)
          .map(s => updated.copy(substitutions = s))
          .getOrElse(updated)
      }
      override def updateAutogenerated(step: Step.Autogenerated, stepWithContext: StepWithContext): Id[Step] = {
        // The default implementation compares the new final statement with the old one, which will usually differ here
        val newSubsteps = this.apply(stepWithContext.forSubsteps(step))
        step match {
          case _: Step.WrappedInferenceApplicationStep => Step.WrappedInferenceApplicationStep(newSubsteps)(newProvingContext)
        }
      }
    }

    /** Keep the rewritten substitutions if they're still valid, otherwise try recalculating them from scratch. */
    private def fixSubstitutions(
      inference: Inference,
      premises: Seq[Statement],
      conclusion: Statement,
      substitutions: Substitutions)(
      implicit substitutionContext: SubstitutionContext
    ): Option[Substitutions] = {
      if (Try(inference.validatePremisesAndConclusion(premises, conclusion, substitutions)).toOption.flatten.nonEmpty) {
        Some(substitutions)
      } else {
        Try {
          for {
            zipped <- inference.premises.zipStrict(premises)
            afterConclusion <- inference.conclusion.calculateSubstitutions(conclusion)
            afterPremises <- zipped.foldLeft(Option(afterConclusion)) { case (so, (inferencePremise, premise)) => so.flatMap(inferencePremise.calculateSubstitutions(premise, _)) }
            total <- afterPremises.confirmTotality(inference.variableDefinitions)
            _ <- inference.validatePremisesAndConclusion(premises, conclusion, total)
          } yield total
        }.toOption.flatten
      }
    }

    // ---------------------------------------------------------------------------------------------------------------
    // Revalidation
    // ---------------------------------------------------------------------------------------------------------------

    private def revalidateSteps(steps: Seq[Step], outerContext: StepContext)(implicit provingContext: ProvingContext): Seq[Step] = {
      steps.foldLeft((Vector.empty[Step], outerContext)) { case ((done, stepContext), step) =>
        val validated = revalidateStep(step, stepContext)
        (done :+ validated, stepContext.addStep(validated))
      }._1
    }

    private def tryParse(step: Step, stepContext: StepContext)(implicit provingContext: ProvingContext): Option[Step] = {
      Try(Step.parser(stepContext, provingContext).parseFromString(step.serializedLines.mkString("\n"), "revalidated step")).toOption
    }

    private def revalidateStep(step: Step, stepContext: StepContext)(implicit provingContext: ProvingContext): Step = {
      tryParse(step, stepContext).getOrElse {
        step match {
          case step: Step.DeductionStep =>
            step.copy(substeps = revalidateSteps(step.substeps, stepContext.addAssumption(step.assumption).forChild()))
          case step: Step.GeneralizationStep =>
            step.copy(substeps = revalidateSteps(step.substeps, stepContext.addBoundVariable(step.variableName).forChild()))
          case step: Step.SubproofStep =>
            step.copy(substeps = revalidateSteps(step.substeps, stepContext.forChild()))
          case step: Step.ElidedStep =>
            Step.ElidedStep(
              revalidateSteps(step.substeps, stepContext.forChild()),
              step.highlightedInference.filter(i => provingContext.availableEntries.inferencesById.contains(i.id)),
              step.description)
          case step: Step.NamingStep =>
            val innerContext = stepContext.addBoundVariable(step.variableName).addAssumption(step.assumption).forChild()
            val rebuilt = step.copy(substeps = revalidateSteps(step.substeps, innerContext))
            tryParse(rebuilt, stepContext).getOrElse {
              reportLines += s"    $currentLabel: naming step for ${step.variableName} replaced by target (inner steps lost)"
              Step.TargetStep(step.statement)
            }
          case step =>
            Step.TargetStep(step.statement)
        }
      }
    }
  }
}
