package net.prover.model

import net.prover.model.Inference.RearrangementType
import net.prover.model.entries._
import net.prover.model.expressions._
import net.prover.model.proof.StepContext

case class EntryContext(availableEntries: Seq[ChapterEntry], termVariableNames: Seq[String]) {
  val inferences: Seq[Inference] = availableEntries.flatMap(_.inferences)
  val statementDefinitions: Seq[StatementDefinition] = availableEntries.collect {
    case statementDefinition: StatementDefinition => statementDefinition
    case typeDefinition: TypeDefinition => typeDefinition.statementDefinition
    case propertyDefinition: PropertyDefinition => propertyDefinition.statementDefinition
  }
  val termDefinitions: Seq[TermDefinition] = availableEntries.ofType[TermDefinition]
  val typeDefinitions: Seq[TypeDefinition] = availableEntries.ofType[TypeDefinition]
  val propertyDefinitionsByType: Map[String, Seq[PropertyDefinition]] = availableEntries.ofType[PropertyDefinition].groupBy(_.parentType.symbol)
  val writingShorthands: Seq[WritingShorthand] = availableEntries.ofType[WritingShorthand]

  lazy val simplificationInferences: Seq[Inference] = inferences.filter(_.rearrangementType == RearrangementType.Simplification)

  def addEntry(entry: ChapterEntry): EntryContext = copy(availableEntries = availableEntries :+ entry)
  def addEntries(entries: Seq[ChapterEntry]): EntryContext = copy(availableEntries = availableEntries ++ entries)

  def deductionDefinitionOption: Option[StatementDefinition] = {
    statementDefinitions.find(_.attributes.contains("deduction"))
  }
  def scopingDefinitionOption: Option[StatementDefinition] = {
    statementDefinitions.find(_.attributes.contains("scoping"))
  }
  def conjunctionDefinitionOption: Option[StatementDefinition] = {
    statementDefinitions.find(_.attributes.contains("conjunction"))
  }
  def equalityDefinitionOption: Option[StatementDefinition] = {
    statementDefinitions.find(_.attributes.contains("equality"))
  }

  def matchScopingStatement(statement: Statement): Option[(Statement, String, StatementDefinition)] = {
    scopingDefinitionOption.flatMap { scopingDefinition =>
      statement match {
        case definedStatement @ DefinedStatement(Seq(substatement), `scopingDefinition`) =>
          substatement.asOptionalInstanceOf[Statement].map((_, definedStatement.scopedBoundVariableNames.head, scopingDefinition))
        case _ =>
          None
      }
    }
  }
  def matchDeductionStatement(statement: Statement): Option[(Statement, Statement, StatementDefinition)] = {
    deductionDefinitionOption.flatMap { deductionDefinition =>
      statement match {
        case DefinedStatement(Seq(antecedentExpression, consequentExpression), `deductionDefinition`) =>
          for {
            antecedent <- antecedentExpression.asOptionalInstanceOf[Statement]
            consequent <- consequentExpression.asOptionalInstanceOf[Statement]
          } yield (antecedent, consequent, deductionDefinition)
        case _ =>
          None
      }
    }
  }
  def matchEqualityStatement(statement: Statement): Option[(Term, Term, StatementDefinition)] = {
    equalityDefinitionOption.flatMap { equalityDefinition =>
      statement match {
        case DefinedStatement(Seq(lhsExpression, rhsExpression), `equalityDefinition`) =>
          for {
            lhs <- lhsExpression.asOptionalInstanceOf[Term]
            rhs <- rhsExpression.asOptionalInstanceOf[Term]
          } yield (lhs, rhs, equalityDefinition)
        case _ =>
          None
      }

    }
  }

  def transitivityInferences: Map[ExpressionDefinition, Inference] = {
    inferences.mapCollect {
      case inference @ Inference(
        _,
        Seq(
          DefinedExpression(d1, Nil, Seq(ExpressionVariable(a1), ExpressionVariable(b1))),
          DefinedExpression(d2, Nil, Seq(ExpressionVariable(b2), ExpressionVariable(c1)))),
        DefinedExpression(d3, Nil, Seq(ExpressionVariable(a2), ExpressionVariable(c2)))
      ) if d1 == d2 && d2 == d3 && a1 == a2 && b1 == b2 && c1 == c2 =>
        Some((d1, inference))
      case _ =>
        None
    }.toMap
  }
  def rewriteInferences: Seq[(Inference, Statement)] = {
    availableEntries.ofType[Inference].collect {
      case inference @ Inference(_, Seq(singlePremise), conclusion)
        if conclusion.complexity == singlePremise.complexity &&
          conclusion.requiredSubstitutions.isEquivalentTo(singlePremise.requiredSubstitutions) &&
          conclusion != singlePremise
      => (inference, singlePremise)
    }
  }

  def findRearrangableFunctions(equalityDefinition: StatementDefinition): Seq[(Term, Inference, Inference)] = {
    inferences
      .mapCollect { inference =>
        val substitutions = inference.requiredSubstitutions
        if (substitutions.statements.isEmpty && substitutions.predicates.isEmpty && substitutions.functions.isEmpty) {
          substitutions.terms match {
            case Seq(a, b) =>
              Some((inference, TermVariable(a), TermVariable(b)))
            case _ =>
              None
          }
        } else {
          None
        }
      }
      .mapCollect { case (inference, firstTerm, secondTerm) =>
        for {
          Seq(l, r) <- equalityDefinition.unapplySeq(inference.conclusion)
          (function, _) <- l.calculateApplicatives(Seq(firstTerm, secondTerm), Substitutions.empty, 0, 0, 0)
            .find { case (function, substitutions) => function.requiredSubstitutions.isEmpty && substitutions == Substitutions(terms = Seq(firstTerm, secondTerm).map(v => v.name -> v).toMap) }
          if function.specify(Seq(secondTerm, firstTerm), 0, 0).contains(r)
        } yield (inference, function.asInstanceOf[Term])
      }
      .mapCollect { case (commutativityInference, function) =>
        inferences
          .mapCollect { inference =>
            val substitutions = inference.requiredSubstitutions
            if (substitutions.statements.isEmpty && substitutions.predicates.isEmpty && substitutions.functions.isEmpty) {
              substitutions.terms match {
                case Seq(a, b, c) =>
                  Some((inference, TermVariable(a), TermVariable(b), TermVariable(c)))
                case _ =>
                  None
              }
            } else {
              None
            }
          }
          .mapFind { case (inference, a, b, c) =>
            def specify(l: Term, r: Term): Term = function.specify(Seq(l, r), 0, 0).get
            if (inference.conclusion == equalityDefinition(specify(a, specify(b, c)), specify(specify(a, b), c)))
              Some((function, commutativityInference, inference))
            else
              None
          }
      }
  }

  def findAssociativityInference(operatorDefinition: TermDefinition): Option[Inference] = {
    for {
      equalityDefinition <- equalityDefinitionOption
      inference <- inferences.find {
        case Inference(
          _,
          Nil,
          DefinedStatement(
            Seq(
              DefinedTerm(Seq(ExpressionVariable(a), DefinedTerm(Seq(ExpressionVariable(b), ExpressionVariable(c)), `operatorDefinition`)), `operatorDefinition`),
              DefinedTerm(Seq(DefinedTerm(Seq(ExpressionVariable(d), ExpressionVariable(e)), `operatorDefinition`), ExpressionVariable(f)), `operatorDefinition`)),
            `equalityDefinition`)
        ) if a == d && b == e && c == f =>
          true
        case _ =>
          false
      }
    } yield inference
  }
  def findCommutativityInference(operatorDefinition: TermDefinition): Option[Inference] = {
    for {
      equalityDefinition <- equalityDefinitionOption
      inference <- inferences.find {
        case Inference(
          _,
          Nil,
          DefinedStatement(
            Seq(
              DefinedTerm(Seq(ExpressionVariable(a), ExpressionVariable(b)), `operatorDefinition`),
              DefinedTerm(Seq(ExpressionVariable(c), ExpressionVariable(d)), `operatorDefinition`)),
            `equalityDefinition`)
        ) if a == d && b == c =>
          true
        case _ =>
          false
      }
    } yield inference
  }
  def findReversalInference(statementDefinition: StatementDefinition): Option[Inference] = {
    inferences.find {
      case Inference(
        _,
        Seq(statementDefinition(ExpressionVariable(a), ExpressionVariable(b))),
        statementDefinition(ExpressionVariable(c), ExpressionVariable(d))
      ) if a == d && b == c =>
        true
      case _ =>
        false
    }
  }
  def findTransitivityInference(statementDefinition: StatementDefinition): Option[Inference] = {
    inferences.find {
      case Inference(
        _,
        Seq(statementDefinition(ExpressionVariable(a), ExpressionVariable(b)), statementDefinition(ExpressionVariable(c), ExpressionVariable(d))),
        statementDefinition(ExpressionVariable(e), ExpressionVariable(f))
      ) if a == e && b == c && d == f =>
        true
      case _ =>
        false
    }
  }
  def findExpansionInference(statementDefinition: StatementDefinition): Option[Inference] = {
    inferences.find {
      case Inference(
      _,
      Seq(statementDefinition(TermVariable(a), TermVariable(b))),
      statementDefinition(FunctionApplication(f, Seq(TermVariable(c))), FunctionApplication(g, Seq(TermVariable(d))))
      ) if a == c && b == d && f == g =>
        true
      case _ =>
        false
    }
  }
  def findSubstitutionInference(statementDefinition: StatementDefinition): Option[Inference] = {
    inferences.find {
      case Inference(
        _,
        Seq(
          statementDefinition(TermVariable(a), TermVariable(b)),
          PredicateApplication(phi, Seq(TermVariable(c)))),
        PredicateApplication(psi, Seq(TermVariable(d))))
      if a == c && b == d && phi == psi =>
        true
      case _ =>
        false
    }

  }

  object RecognisedStatementDefinition {
    def unapply(string: String): Option[StatementDefinition] = {
      statementDefinitions.find(_.symbol == string)
    }
  }
  object RecognisedTermDefinition {
    def unapply(s: String): Option[TermDefinition] = {
      termDefinitions.find(_.symbol == s)
    }
  }

  object RecognisedStatementShorthand {
    def unapply(string: String): Option[Template.DefinedStatement] = {
      writingShorthands.find(_.symbol == string).flatMap(_.template.asOptionalInstanceOf[Template.DefinedStatement])
    }
  }
  object RecognisedTermShorthand {
    def unapply(string: String): Option[Template.DefinedTerm] = {
      writingShorthands.find(_.symbol == string).flatMap(_.template.asOptionalInstanceOf[Template.DefinedTerm])
    }
  }
}

object EntryContext {
  def forBooks(books: Seq[Book], termVariableNames: Seq[String]): EntryContext = {
    EntryContext(books.flatMap(_.chapters).flatMap(_.entries), termVariableNames)
  }
  def forBookExclusive(allBooks: Seq[Book], book: Book): EntryContext = {
    forBooks(Book.getDependencies(book.imports, allBooks), book.termVariableNames)
  }
  def forChapterExclusive(allBooks: Seq[Book], book: Book, chapter: Chapter): EntryContext = {
    forBookExclusive(allBooks, book).addEntries(book.chapters.until(chapter).flatMap(_.entries))
  }
  def forChapterInclusive(allBooks: Seq[Book], book: Book, chapter: Chapter): EntryContext = {
    forChapterExclusive(allBooks, book, chapter).addEntries(chapter.entries)
  }
  def forEntry(allBooks: Seq[Book], book: Book, chapter: Chapter, entry: ChapterEntry): EntryContext = {
    forChapterExclusive(allBooks, book, chapter).addEntries(chapter.entries.until(entry))
  }
}
