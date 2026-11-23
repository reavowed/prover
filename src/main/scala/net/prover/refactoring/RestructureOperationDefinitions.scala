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
  *   type unaryOperation f (definition: function f domain f domain f)
  *   qualifier on on unaryOperation (A) (on A)
  *   type binaryOperation ∘ (definition: function ∘ product baseSet ∘ baseSet ∘ baseSet ∘)
  *   qualifier on on binaryOperation (A) (on A)
  *
  * with types whose default qualifier is (A) (on A), defined by the right-hand sides of the "Alternative Definition for
  * Unary/Binary Operation on a Set" theorems. Properties and related objects on binary operations gain the qualifier
  * variable, and type relations between binary operations name it explicitly.
  *
  * Statements are rewritten as follows:
  *  - `∧ binaryOperation X binaryOperationOn X A` and `binaryOperationOn X A` → `binaryOperation X A` (likewise unary)
  *  - bare `binaryOperation X`, `associativeBinaryOperation X`, `identityBinaryOperation e X`, `distributes X Y` etc.
  *    gain the set A that X is an operation on, determined from (in order of preference) a sibling conjunct, a
  *    unique statement about X in the same theorem and its proofs, or a unique statement about X anywhere in the corpus
  *    (if X contains no variables). If X is a term variable of a theorem whose premises and conclusion don't determine
  *    its set, a new term variable is added to the theorem. Otherwise falls back to `baseSet X` / `domain X`.
  *  - `baseSet X` → A wherever A is known as above.
  *
  * Every proof step is then re-validated by parsing it against the new definitions; any step that fails becomes a
  * target with the same (rewritten) statement, unless it's a trivial equality `= a a`, which is proved by reflexivity.
  */
object RestructureOperationDefinitions {
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
    println("Restructure complete")
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

  private enum Kind {
    case Unary, Binary
  }
  /** (kind, operation) -> the set it's an operation on */
  private type Candidates = Map[(Kind, Term), Term]

  private class Rework(oldGlobalContext: GlobalContext) {
    private val allOldEntries = oldGlobalContext.allEntries

    private def findOld[T <: ChapterEntry](description: String)(f: PartialFunction[ChapterEntry, T]): T = {
      val matches = allOldEntries.map(_.entry).collect(f)
      matches match {
        case Seq(single) => single
        case _ => throw new Exception(s"Expected exactly one $description, found ${matches.length}")
      }
    }

    private val oldUnaryType = findOld("unary operation type") { case t: TypeDefinition if t.symbol == "unaryOperation" => t }
    private val oldBinaryType = findOld("binary operation type") { case t: TypeDefinition if t.symbol == "binaryOperation" => t }
    private val oldUnaryOn = findOld("unary on qualifier") { case q: TypeQualifierDefinition if q.parentType == oldUnaryType && q.symbol == "on" => q }
    private val oldBinaryOn = findOld("binary on qualifier") { case q: TypeQualifierDefinition if q.parentType == oldBinaryType && q.symbol == "on" => q }
    private val oldBaseSet = findOld("baseSet term") { case t: TermDefinitionEntry if t.baseSymbol == "baseSet" && t.disambiguator.isEmpty => t }
    private val oldDomain = findOld("domain term") { case t: TermDefinitionEntry if t.baseSymbol == "domain" && t.disambiguator.isEmpty => t }
    private val oldUnaryAlternative = findOld("unary alternative definition") { case t: Theorem if t.name == "Alternative Definition for Unary Operation on a Set" => t }
    private val oldBinaryAlternative = findOld("binary alternative definition") { case t: Theorem if t.name == "Alternative Definition for Binary Operation on a Set" => t }

    private def kindOf(typeDefinition: TypeDefinition): Option[Kind] = {
      if (typeDefinition == oldUnaryType) Some(Kind.Unary)
      else if (typeDefinition == oldBinaryType) Some(Kind.Binary)
      else None
    }
    private def oldTypeFor(kind: Kind): TypeDefinition = kind match {
      case Kind.Unary => oldUnaryType
      case Kind.Binary => oldBinaryType
    }
    private def oldOnFor(kind: Kind): TypeQualifierDefinition = kind match {
      case Kind.Unary => oldUnaryOn
      case Kind.Binary => oldBinaryOn
    }

    private val oldTypeDefinitions: Map[StatementDefinition, Kind] = Map(oldUnaryType.statementDefinition -> Kind.Unary, oldBinaryType.statementDefinition -> Kind.Binary)
    private val oldOnDefinitions: Map[StatementDefinition, Kind] = Map(oldUnaryOn.statementDefinition -> Kind.Unary, oldBinaryOn.statementDefinition -> Kind.Binary)

    /** Properties and related objects on the operation types, whose statements gain the qualifier variable: (kind, index of the operation) */
    private val qualifierInsertions: Map[StatementDefinition, (Kind, Int)] = allOldEntries.map(_.entry).flatMap {
      case p: PropertyDefinitionOnType if kindOf(p.parentType).isDefined =>
        checkParentQualifier(p.parentTypeConditions, p.name)
        if (p.parentTypeConditions.requiredParentQualifier.isEmpty) Some(p.statementDefinition -> (kindOf(p.parentType).get, 0)) else None
      case r: RelatedObjectDefinition if kindOf(r.parentType).isDefined =>
        checkParentQualifier(r.parentTypeConditions, r.name)
        if (r.parentTypeConditions.requiredParentQualifier.isEmpty) Some(r.statementDefinition -> (kindOf(r.parentType).get, 1)) else None
      case _ =>
        None
    }.toMap
    private def checkParentQualifier(parentTypeConditions: ParentTypeConditions, name: String): Unit = {
      if (parentTypeConditions.termListAdapter.nonEmpty) throw new Exception(s"Unexpected term list adapter on $name")
      parentTypeConditions.requiredParentQualifier.foreach { q =>
        if (q != oldOnFor(kindOf(parentTypeConditions.parentType).get)) throw new Exception(s"Unexpected parent qualifier ${q.symbol} on $name")
      }
    }

    private val oldTypeRelations: Map[StatementDefinition, TypeRelationDefinition] = allOldEntries.map(_.entry).collect {
      case r: TypeRelationDefinition if kindOf(r.firstType).isDefined || kindOf(r.secondType).isDefined => r.statementDefinition -> r
    }.toMap

    private val oldConjunctionDefinition: StatementDefinition = allOldEntries
      .find(_.entry == oldBinaryType).get.availableEntries.conjunctionDefinitionOption.get.statementDefinition
    private val specialEntries: Set[ChapterEntry] = Set[ChapterEntry](oldUnaryType, oldBinaryType, oldUnaryOn, oldBinaryOn) ++
      qualifierInsertions.keys.map(_.associatedChapterEntry) ++
      oldTypeRelations.values

    private val entryMap = mutable.Map.empty[ChapterEntry, ChapterEntry]
    private val definitionMap = mutable.Map.empty[ExpressionDefinition, ExpressionDefinition]
    private val inferenceMap = mutable.Map.empty[String, Inference.Summary]

    private val reportLines = mutable.LinkedHashSet.empty[String]
    private var currentLabel: String = ""
    def report: Report = Report(reportLines.toSeq)

    /** While set, fallbacks are recorded here rather than reported */
    private var dryRunMisses: Option[mutable.LinkedHashSet[(Kind, Term)]] = None

    // ---------------------------------------------------------------------------------------------------------------
    // Candidate sets
    // ---------------------------------------------------------------------------------------------------------------

    private def hasNoParameters(expression: Expression): Boolean = expression match {
      case _: FunctionParameter => false
      case definedExpression: DefinedExpression[?] => definedExpression.components.forall(hasNoParameters)
      case StatementVariable(_, arguments) => arguments.forall(hasNoParameters)
      case TermVariable(_, arguments) => arguments.forall(hasNoParameters)
      case _ => true
    }
    private def isGround(expression: Expression): Boolean = expression match {
      case definedExpression: DefinedExpression[?] => definedExpression.components.forall(isGround)
      case _ => false
    }
    private def containsSubterm(expression: Expression, subterm: Term): Boolean = expression == subterm || (expression match {
      case definedExpression: DefinedExpression[?] => definedExpression.components.exists(containsSubterm(_, subterm))
      case StatementVariable(_, arguments) => arguments.exists(containsSubterm(_, subterm))
      case TermVariable(_, arguments) => arguments.exists(containsSubterm(_, subterm))
      case _ => false
    })

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

    /** Replace term variables in a template with the given components. Templates never contain bound variables. */
    private def instantiate(template: Term, components: Seq[Expression]): Option[Term] = template match {
      case TermVariable(index, Nil) => components.lift(index).collect { case t: Term => t }
      case definedTerm @ DefinedTerm(templateComponents, definition) if definedTerm.boundVariableNames.isEmpty =>
        templateComponents.map {
          case t: Term => instantiate(t, components)
          case _ => None
        }.traverseOption.map(DefinedTerm(_, definition)(Nil))
      case _ => None
    }
    private def isSimpleTemplate(term: Term): Boolean = term match {
      case TermVariable(_, Nil) => true
      case definedTerm @ DefinedTerm(components, _) => definedTerm.boundVariableNames.isEmpty && components.forall {
        case t: Term => isSimpleTemplate(t)
        case _ => false
      }
      case _ => false
    }

    /**
      * For a statement definition, the operations and sets it implies (in terms of its own components). The qualifiers
      * imply them directly, and any other definition implies those of the conjuncts of its definition - so for instance,
      * `group G ∘` implies that ∘ is a binary operation on G.
      */
    private val impliedTemplatesCache = mutable.Map.empty[StatementDefinition, Seq[(Kind, Term, Term)]]
    private def impliedTemplates(definition: StatementDefinition): Seq[(Kind, Term, Term)] = {
      impliedTemplatesCache.get(definition) match {
        case Some(result) => result
        case None =>
          val result = oldOnDefinitions.get(definition) match {
            case Some(kind) =>
              Seq((kind, TermVariable(0, Nil), TermVariable(1, Nil)))
            case None =>
              definition.definingStatement.toSeq.flatMap(conjunctionLeaves).flatMap {
                case DefinedStatement(components, innerDefinition) =>
                  impliedTemplates(innerDefinition).flatMap { case (kind, x, a) =>
                    for {
                      newX <- instantiate(x, components)
                      newA <- instantiate(a, components)
                      if isSimpleTemplate(newX) && isSimpleTemplate(newA)
                    } yield (kind, newX, newA)
                  }
                case _ => Nil
              }.distinct
          }
          impliedTemplatesCache.put(definition, result)
          result
      }
    }

    private def directCandidates(statement: Statement): Seq[((Kind, Term), Term)] = statement match {
      case DefinedStatement(components, definition) =>
        impliedTemplates(definition).flatMap { case (kind, xTemplate, aTemplate) =>
          for {
            x <- instantiate(xTemplate, components)
            a <- instantiate(aTemplate, components)
            if !containsSubterm(a, x)
          } yield (kind, x) -> a
        }
      case _ => Nil
    }

    private def uniqueCandidates(candidates: Seq[((Kind, Term), Term)]): Candidates = {
      candidates.groupBy(_._1).view.mapValues(_.map(_._2).distinct).collect { case (key, Seq(single)) => key -> single }.toMap
    }

    private def closedCandidates(expressions: Seq[Expression]): Candidates = {
      uniqueCandidates(expressions.flatMap(allSubStatements).flatMap(directCandidates).filter { case ((_, x), a) => hasNoParameters(x) && hasNoParameters(a) })
    }

    /** The candidates implied by an entry's own statement, in terms of its own variables - e.g. for a property on groups */
    private def ownCandidates(statementDefinition: StatementDefinition): Candidates = {
      uniqueCandidates(impliedTemplates(statementDefinition).map { case (kind, x, a) => (kind, x) -> a })
    }

    /** Candidates involving no variables at all, from anywhere in the corpus */
    private val globalCandidates: Candidates = {
      val expressions = oldGlobalContext.allTheorems.flatMap(collectExpressions)
      uniqueCandidates(expressions.flatMap(allSubStatements).flatMap(directCandidates).filter { case ((_, x), a) => isGround(x) && isGround(a) })
    }

    // ---------------------------------------------------------------------------------------------------------------
    // Expression rewriting
    // ---------------------------------------------------------------------------------------------------------------

    private def newStatementDefinition(old: StatementDefinition): StatementDefinition = {
      definitionMap.getOrElse(old, throw new Exception(s"No replacement for statement definition ${old.symbol} ($currentLabel)")).asInstanceOf[StatementDefinition]
    }
    private def newTermDefinition(old: TermDefinition): TermDefinition = {
      definitionMap.getOrElse(old, throw new Exception(s"No replacement for term definition ${old.symbol} ($currentLabel)")).asInstanceOf[TermDefinition]
    }

    private def newTypeStatement(kind: Kind, x: Term, a: Term): Statement = {
      DefinedStatement(Seq(x, a), newStatementDefinition(oldTypeFor(kind).statementDefinition))(Nil)
    }

    private def qualifierTerm(kind: Kind, x: Term, candidates: Candidates, description: String): Term = {
      candidates.get((kind, x)) match {
        case Some(a) =>
          term(a, candidates)
        case None =>
          val newX = term(x, candidates)
          val (fallbackDefinition, fallbackName) = kind match {
            case Kind.Binary => (oldBaseSet, "baseSet")
            case Kind.Unary => (oldDomain, "domain")
          }
          dryRunMisses match {
            case Some(misses) => misses += ((kind, x))
            case None => reportLines += s"    FALLBACK $currentLabel: $description ${newX.serialized} -> $fallbackName"
          }
          DefinedTerm(Seq(newX), newTermDefinition(fallbackDefinition))(Nil)
      }
    }

    private def restrictToBinder(candidates: Candidates): Candidates = {
      candidates.filter { case ((_, x), a) => hasNoParameters(x) && hasNoParameters(a) }
    }

    def statement(statement: Statement, candidates: Candidates): Statement = statement match {
      case DefinedStatement(Seq(l: Statement, r: Statement), `oldConjunctionDefinition`) =>
        (l, r) match {
          case (DefinedStatement(Seq(x: Term), typeDefinition), DefinedStatement(Seq(x2: Term, a: Term), onDefinition))
            if x == x2 && oldTypeDefinitions.contains(typeDefinition) && oldOnDefinitions.get(onDefinition).contains(oldTypeDefinitions(typeDefinition)) =>
            newTypeStatement(oldTypeDefinitions(typeDefinition), term(x, candidates), term(a, candidates))
          case _ =>
            val localCandidates = candidates ++ uniqueCandidates(conjunctionLeaves(statement).flatMap(directCandidates))
            DefinedStatement(
              Seq(this.statement(l, localCandidates), this.statement(r, localCandidates)),
              newStatementDefinition(oldConjunctionDefinition))(Nil)
        }
      case DefinedStatement(Seq(x: Term), definition) if oldTypeDefinitions.contains(definition) =>
        val kind = oldTypeDefinitions(definition)
        newTypeStatement(kind, term(x, candidates), qualifierTerm(kind, x, candidates, definition.symbol))
      case DefinedStatement(Seq(x: Term, a: Term), definition) if oldOnDefinitions.contains(definition) =>
        newTypeStatement(oldOnDefinitions(definition), term(x, candidates), term(a, candidates))
      case DefinedStatement(components, definition) if qualifierInsertions.contains(definition) =>
        val (kind, operationIndex) = qualifierInsertions(definition)
        val operation = components(operationIndex).asInstanceOf[Term]
        DefinedStatement(
          components.map(expression(_, candidates)) :+ qualifierTerm(kind, operation, candidates, definition.symbol),
          newStatementDefinition(definition))(Nil)
      case DefinedStatement(components @ Seq(first: Term, second: Term), definition) if oldTypeRelations.contains(definition) =>
        val oldRelation = oldTypeRelations(definition)
        val newRelation = entryMap(oldRelation).asInstanceOf[TypeRelationDefinition]
        val qualifierTerms = newRelation.qualifierVariableDefinitions.map(_.name).map { name =>
          val sources = (if (newRelation.firstQualifierVariableNames.contains(name)) kindOf(oldRelation.firstType).map(_ -> first).toSeq else Nil) ++
            (if (newRelation.secondQualifierVariableNames.contains(name)) kindOf(oldRelation.secondType).map(_ -> second).toSeq else Nil)
          sources.find(candidates.contains) match {
            case Some((kind, operation)) => qualifierTerm(kind, operation, candidates, definition.symbol)
            case None => qualifierTerm(sources.head._1, sources.head._2, candidates, definition.symbol)
          }
        }
        DefinedStatement(components.map(expression(_, candidates)) ++ qualifierTerms, newStatementDefinition(definition))(Nil)
      case definedStatement @ DefinedStatement(components, definition) =>
        val innerCandidates = if (definedStatement.boundVariableNames.nonEmpty) restrictToBinder(candidates) else candidates
        DefinedStatement(components.map(expression(_, innerCandidates)), newStatementDefinition(definition))(definedStatement.boundVariableNames)
      case StatementVariable(index, arguments) =>
        StatementVariable(index, arguments.map(term(_, candidates)))
      case other =>
        throw new Exception(s"Unexpected statement type ${other.getClass.getName}")
    }

    def term(term: Term, candidates: Candidates): Term = term match {
      case DefinedTerm(Seq(x: Term), definition) if definition == oldBaseSet && candidates.contains((Kind.Binary, x)) =>
        this.term(candidates((Kind.Binary, x)), candidates)
      case definedTerm @ DefinedTerm(components, definition) =>
        val innerCandidates = if (definedTerm.boundVariableNames.nonEmpty) restrictToBinder(candidates) else candidates
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

    /** Unmapped inferences (the definitions of the removed qualifiers) are left as-is, and so will fail validation. */
    def inference(inference: Inference.Summary): Inference.Summary = inferenceMap.getOrElse(inference.id, inference)

    private def isTrivialEquality(statement: Statement): Boolean = statement match {
      case DefinedStatement(Seq(a: Term, b: Term), definition) => definition.attributes.contains("equality") && a == b
      case _ => false
    }
    /** Remove any conjuncts that have become `= A A` */
    private def dropTrivialEqualities(statement: Statement): Statement = statement match {
      case DefinedStatement(Seq(l: Statement, r: Statement), definition) if definition == oldConjunctionDefinition =>
        if (isTrivialEquality(l)) dropTrivialEqualities(r)
        else if (isTrivialEquality(r)) dropTrivialEqualities(l)
        else DefinedStatement(Seq(dropTrivialEqualities(l), dropTrivialEqualities(r)), definition)(Nil)
      case other => other
    }

    // ---------------------------------------------------------------------------------------------------------------
    // Entries
    // ---------------------------------------------------------------------------------------------------------------

    def transformEntry(oldEntryWithContext: EntryWithContext, entryInNewWorld: EntryWithContext): Option[ChapterEntry] = {
      val oldEntry = oldEntryWithContext.entry
      currentLabel = s"${oldEntryWithContext.chapterWithContext.chapter.title} / ${oldEntry.name}"
      lazy val newAvailableEntries = entryInNewWorld.availableEntries
      lazy val newConjunctionDefinition: ConjunctionDefinition = newAvailableEntries.conjunctionDefinitionOption.get

      def newParentTypeConditions(old: ParentTypeConditions): ParentTypeConditions = {
        if (kindOf(old.parentType).isDefined) {
          ParentTypeConditions(
            entryMap(old.parentType).asInstanceOf[TypeDefinition],
            None,
            old.requiredParentObjects.map(_.replaceDefinitions(entryMap.toMap, newAvailableEntries)),
            None,
            newConjunctionDefinition)
        } else {
          old.replaceDefinitions(entryMap.toMap, definitionMap.toMap, newAvailableEntries)
        }
      }
      /** For an entry on an operation type, the operation's variable index and the qualifier variable's index */
      def operationSeeds(parentType: TypeDefinition, operationIndex: Int): Candidates = {
        kindOf(parentType).map(kind => Map((kind, TermVariable(operationIndex, Nil): Term) -> (TermVariable(operationIndex + 1, Nil): Term))).getOrElse(Map.empty)
      }

      val newEntryOption: Option[ChapterEntry] = oldEntry match {
        case `oldUnaryOn` | `oldBinaryOn` =>
          None
        case typeDefinition: TypeDefinition if kindOf(typeDefinition).isDefined =>
          val kind = kindOf(typeDefinition).get
          val alternative = kind match {
            case Kind.Unary => oldUnaryAlternative
            case Kind.Binary => oldBinaryAlternative
          }
          val definingStatement = alternative.conclusion match {
            case DefinedStatement(Seq(_, rhs: Statement), _) => statement(rhs, operationSeeds(typeDefinition, 0))
            case other => throw new Exception(s"Unexpected alternative definition $other")
          }
          Some(TypeDefinition(
            typeDefinition.symbol,
            typeDefinition.mainVariableDefinition,
            Some(oldOnFor(kind).qualifier),
            typeDefinition.explicitName,
            definingStatement))
        case theorem: Theorem =>
          Some(transformTheorem(oldEntryWithContext.asInstanceOf[TheoremWithContext], entryInNewWorld.asInstanceOf[TheoremWithContext]))
        case axiom: Axiom =>
          val candidates = globalCandidates ++ closedCandidates(axiom.premises :+ axiom.conclusion)
          Some(Axiom(axiom.name, axiom.variableDefinitions, axiom.premises.map(statement(_, candidates)), statement(axiom.conclusion, candidates)))
        case termDefinition: TermDefinitionEntry =>
          val candidates = globalCandidates ++ closedCandidates(termDefinition.premises :+ termDefinition.definitionPredicate)
          Some(termDefinition.copy(
            premises = termDefinition.premises.map(statement(_, candidates)),
            definitionPredicate = statement(termDefinition.definitionPredicate, candidates),
            disambiguatorAdders = termDefinition.disambiguatorAdders.map(_.replaceDefinitions(definitionMap.toMap))))
        case statementDefinition: StatementDefinitionEntry =>
          Some(statementDefinition.copy(definingStatement = statementDefinition.definingStatement.map(statement(_, Map.empty))))
        case typeDefinition: TypeDefinition =>
          Some(typeDefinition.copy(definingStatement = statement(typeDefinition.definingStatement, ownCandidates(typeDefinition.statementDefinition))))
        case qualifierDefinition: TypeQualifierDefinition =>
          Some(TypeQualifierDefinition(
            qualifierDefinition.symbol,
            entryMap(qualifierDefinition.parentType).asInstanceOf[TypeDefinition],
            qualifierDefinition.qualifier,
            qualifierDefinition.explicitName,
            statement(qualifierDefinition.definingStatement, ownCandidates(qualifierDefinition.statementDefinition)),
            newConjunctionDefinition))
        case property: PropertyDefinitionOnType =>
          Some(PropertyDefinitionOnType(
            property.symbol,
            newParentTypeConditions(property.parentTypeConditions),
            property.explicitName,
            statement(property.definingStatement, ownCandidates(property.statementDefinition) ++ operationSeeds(property.parentType, 0))))
        case property: StandalonePropertyDefinition =>
          Some(property.copy(definingStatement = statement(property.definingStatement, ownCandidates(property.statementDefinition))))
        case relatedObject: RelatedObjectDefinition =>
          Some(RelatedObjectDefinition(
            relatedObject.symbol,
            relatedObject.mainVariableDefinition,
            newParentTypeConditions(relatedObject.parentTypeConditions),
            relatedObject.explicitName,
            statement(relatedObject.definingStatement, ownCandidates(relatedObject.statementDefinition) ++ operationSeeds(relatedObject.parentType, 1))))
        case relation: TypeRelationDefinition if oldTypeRelations.values.exists(_ == relation) =>
          val takenNames = Set(relation.firstVariable.name, relation.secondVariable.name)
          val qualifierName = Seq("A", "S", "X", "B").find(n => !takenNames.contains(n)).get
          val firstKind = kindOf(relation.firstType)
          val secondKind = kindOf(relation.secondType)
          val firstQualifierVariableNames = if (firstKind.isDefined) Seq(qualifierName) else relation.firstQualifierVariableNames
          val secondQualifierVariableNames = if (secondKind.isDefined) Seq(qualifierName) else relation.secondQualifierVariableNames
          val qualifierIndex = TypeRelationDefinition.variableDefinitions(relation.firstVariable, relation.secondVariable, firstQualifierVariableNames, secondQualifierVariableNames)
            .indexWhere(_.name == qualifierName)
          val seeds: Candidates = (firstKind.map(k => (k, TermVariable(0, Nil): Term) -> (TermVariable(qualifierIndex, Nil): Term)).toSeq ++
            secondKind.map(k => (k, TermVariable(1, Nil): Term) -> (TermVariable(qualifierIndex, Nil): Term)).toSeq).toMap
          Some(TypeRelationDefinition(
            relation.symbol,
            entryMap(relation.firstType).asInstanceOf[TypeDefinition],
            entryMap(relation.secondType).asInstanceOf[TypeDefinition],
            relation.firstVariable,
            relation.secondVariable,
            firstQualifierVariableNames,
            secondQualifierVariableNames,
            relation.linkingPhrase,
            relation.explicitName,
            dropTrivialEqualities(statement(relation.definingStatement, seeds)),
            newConjunctionDefinition))
        case other =>
          if (other.referencedEntries.exists(specialEntries.contains)) {
            throw new Exception(s"Unhandled entry type ${other.getClass.getSimpleName} referencing operation definitions: $currentLabel")
          }
          Some(other.replaceDefinitions(entryMap.toMap, definitionMap.toMap, entryInNewWorld))
      }
      newEntryOption.foreach(recordEntry(oldEntry, _))
      newEntryOption
    }

    private def recordEntry(oldEntry: ChapterEntry, newEntry: ChapterEntry): Unit = {
      entryMap += (oldEntry -> newEntry)
      (AvailableEntries.getStatementDefinitionFromEntry(oldEntry), AvailableEntries.getStatementDefinitionFromEntry(newEntry)) match {
        case (Some(oldDefinition), Some(newDefinition)) =>
          definitionMap += (oldDefinition -> newDefinition)
        case _ =>
      }
      (oldEntry, newEntry) match {
        case (oldTerm: TermDefinitionEntry, newTerm: TermDefinitionEntry) => definitionMap += (oldTerm -> newTerm)
        case _ =>
      }
      if (oldEntry.inferences.length == newEntry.inferences.length) {
        oldEntry.inferences.zip(newEntry.inferences).foreach { case (oldInference, newInference) =>
          inferenceMap += (oldInference.id -> Inference.Summary(newInference))
        }
      } else {
        reportLines += s"  WARNING: inference count changed for $currentLabel"
      }
    }

    // ---------------------------------------------------------------------------------------------------------------
    // Theorems and proofs
    // ---------------------------------------------------------------------------------------------------------------

    private val newVariableNames = Seq("A", "B", "C", "S", "T", "X", "Y", "Z")

    private def transformTheorem(oldTheoremWithContext: TheoremWithContext, theoremInNewWorld: TheoremWithContext): Theorem = {
      val oldTheorem = oldTheoremWithContext.theorem
      println("    - " + oldTheorem.name)
      val baseCandidates = globalCandidates ++ collectCandidates(oldTheoremWithContext)

      // Find any operations in the theorem statement that are term variables whose set can't be determined, and add a
      // new variable for each of them
      val misses = mutable.LinkedHashSet.empty[(Kind, Term)]
      dryRunMisses = Some(misses)
      try {
        (oldTheorem.premises :+ oldTheorem.conclusion).foreach(statement(_, baseCandidates))
      } finally {
        dryRunMisses = None
      }
      val (newVariableDefinitions, candidates) = misses.toSeq.collect { case (kind, variable @ TermVariable(_, Nil)) => (kind, variable) }
        .foldLeft((oldTheorem.variableDefinitions, baseCandidates)) { case ((variableDefinitions, candidatesSoFar), (kind, variable)) =>
          val existingNames = variableDefinitions.terms.map(_.name)
          val name = newVariableNames.find(n => !existingNames.contains(n)).getOrElse(throw new Exception(s"No free variable name for $currentLabel"))
          reportLines += s"  $currentLabel: added variable $name for the set that ${existingNames(variable.index)} is an operation on"
          (
            variableDefinitions.copy(terms = variableDefinitions.terms :+ VariableDefinition(name, 0, Nil)),
            candidatesSoFar + ((kind, variable) -> TermVariable(variableDefinitions.terms.length, Nil)))
        }

      val newAvailableEntries = theoremInNewWorld.availableEntries
      implicit val newProvingContext: ProvingContext = ProvingContext(newAvailableEntries, Definitions(newAvailableEntries))
      val reflexivity = findReflexivity(newAvailableEntries)

      // Traverse the old theorem's steps in the old world - the step contexts only matter for their structure and bound
      // variables, and using the old world avoids recalculating definitions for every theorem
      val rewritten = new ProofRewriter(candidates, newProvingContext).apply(oldTheoremWithContext).copy(variableDefinitions = newVariableDefinitions)
      val validatedProofs = rewritten.proofs.zip(oldTheorem.proofs).zipWithIndex.map { case ((proof, oldProof), index) =>
        val initialContext = StepContext.withPremisesAndVariables(rewritten.premises, rewritten.variableDefinitions)
        val validatedSteps = revalidateSteps(proof.steps, initialContext, reflexivity)
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

    private def findReflexivity(availableEntries: AvailableEntries): Option[Inference] = {
      for {
        equalityDefinition <- availableEntries.statementDefinitions.find(_.attributes.contains("equality"))
        inference <- availableEntries.allInferences.find(i => i.premises.isEmpty && (i.conclusion match {
          case DefinedStatement(Seq(TermVariable(a, Nil), TermVariable(b, Nil)), `equalityDefinition`) => a == b
          case _ => false
        }))
      } yield inference
    }

    private def countTargets(steps: Seq[Step]): Int = steps.map {
      case _: Step.TargetStep => 1
      case step: Step.WithSubsteps => countTargets(step.substeps)
      case _ => 0
    }.sum

    private def collectExpressions(theoremWithContext: TheoremWithContext): Seq[Expression] = {
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
      expressions.toSeq
    }

    private def collectCandidates(theoremWithContext: TheoremWithContext): Candidates = {
      closedCandidates(collectExpressions(theoremWithContext))
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

    private def revalidateSteps(steps: Seq[Step], outerContext: StepContext, reflexivity: Option[Inference])(implicit provingContext: ProvingContext): Seq[Step] = {
      steps.foldLeft((Vector.empty[Step], outerContext)) { case ((done, stepContext), step) =>
        val validated = revalidateStep(step, stepContext, reflexivity)
        (done :+ validated, stepContext.addStep(validated))
      }._1
    }

    private def tryParse(step: Step, stepContext: StepContext)(implicit provingContext: ProvingContext): Option[Step] = {
      Try(Step.parser(stepContext, provingContext).parseFromString(step.serializedLines.mkString("\n"), "revalidated step")).toOption
    }

    /** A failed step whose statement has become `= a a` can be proved directly by reflexivity */
    private def reflexivityStep(statement: Statement, stepContext: StepContext, reflexivity: Option[Inference])(implicit provingContext: ProvingContext): Option[Step] = {
      if (!isTrivialEquality(statement)) None else for {
        inference <- reflexivity
        substitutions <- fixSubstitutions(inference, Nil, statement, Substitutions.empty)(stepContext)
        step <- tryParse(Step.AssertionStep(statement, inference.summary, Nil, substitutions), stepContext)
      } yield step
    }

    private def revalidateStep(step: Step, stepContext: StepContext, reflexivity: Option[Inference])(implicit provingContext: ProvingContext): Step = {
      tryParse(step, stepContext).getOrElse {
        step match {
          case step: Step.DeductionStep =>
            step.copy(substeps = revalidateSteps(step.substeps, stepContext.addAssumption(step.assumption).forChild(), reflexivity))
          case step: Step.GeneralizationStep =>
            step.copy(substeps = revalidateSteps(step.substeps, stepContext.addBoundVariable(step.variableName).forChild(), reflexivity))
          case step: Step.SubproofStep =>
            step.copy(substeps = revalidateSteps(step.substeps, stepContext.forChild(), reflexivity))
          case step: Step.ElidedStep =>
            Step.ElidedStep(
              revalidateSteps(step.substeps, stepContext.forChild(), reflexivity),
              step.highlightedInference.filter(i => provingContext.availableEntries.inferencesById.contains(i.id)),
              step.description)
          case step: Step.NamingStep =>
            val innerContext = stepContext.addBoundVariable(step.variableName).addAssumption(step.assumption).forChild()
            val rebuilt = step.copy(substeps = revalidateSteps(step.substeps, innerContext, reflexivity))
            tryParse(rebuilt, stepContext).getOrElse {
              reportLines += s"    $currentLabel: naming step for ${step.variableName} replaced by target (inner steps lost)"
              Step.TargetStep(step.statement)
            }
          case step =>
            reflexivityStep(step.statement, stepContext, reflexivity).getOrElse(Step.TargetStep(step.statement))
        }
      }
    }
  }
}
