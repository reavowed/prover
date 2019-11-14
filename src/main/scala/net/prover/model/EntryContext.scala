package net.prover.model

import net.prover.model.Inference.RearrangementType
import net.prover.model.definitions._
import net.prover.model.entries._
import net.prover.model.expressions._
import net.prover.model.proof.SubstitutionContext

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
  def equalityOption: Option[Equality] = {
    for {
      definition <- statementDefinitions.find(_.attributes.contains("equality"))
      relation = BinaryRelation(definition.defaultValue)
      expansion <- findExpansion(relation)
      substitution <- findSubstitution(relation)
      reversal <- findReversal(relation)
      transitivity <- findTransitivity(relation)
    } yield Equality(relation, expansion, substitution, reversal, transitivity)
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

  def getTransitivityDefinitions: Seq[(String, Transitivity)] = {
    // Find all statement definitions that have default infix formats and a proven transitivity
    def fromDefinitions = for {
      definition <- statementDefinitions
      if definition.componentTypes.length == 2 && definition.format.baseFormatString == s"%0 ${definition.symbol} %1"
      relation = BinaryRelation(definition.defaultValue)
      transitivity <- findTransitivity(relation)
    } yield (definition.symbol, transitivity)

    // Find all term definitions that can be built into an infix statement via a shorthand and have a proven transitivity
    def fromShorthands = for {
      shorthand <- availableEntries.ofType[DisplayShorthand]
      if shorthand.template.isInstanceOf[Template.DefinedStatement]
      if shorthand.template.variables.length == 3
      Seq(lhsIndex, symbolIndex, rhsIndex) <- "%(\\d) %(\\d) %(\\d)".r.unapplySeq(shorthand.format.baseFormatString).map(_.map(_.toInt)).toSeq
      if lhsIndex != symbolIndex && lhsIndex != rhsIndex && symbolIndex != rhsIndex
      lhsVariable = shorthand.template.variables(lhsIndex)
      symbolVariable = shorthand.template.variables(symbolIndex)
      rhsVariable = shorthand.template.variables(rhsIndex)
      if lhsVariable.isInstanceOf[Template.TermVariable] && symbolVariable.isInstanceOf[Template.TermVariable] && rhsVariable.isInstanceOf[Template.TermVariable]
      if shorthand.conditions.forall(_._1 == symbolVariable.name)
      definition <- termDefinitions
      if definition.componentTypes.isEmpty
      if shorthand.conditions.map(_._2).forall(definition.attributes.contains)
      relation = BinaryRelation(
        shorthand.template.expand(
          Map.empty,
          Map(
            lhsVariable.name -> TermVariable(lhsVariable.name),
            rhsVariable.name -> TermVariable(rhsVariable.name),
            symbolVariable.name -> definition.defaultValue)
        ).asInstanceOf[Statement])
      transitivity <- findTransitivity(relation)
    } yield (definition.symbol, transitivity)

    fromDefinitions ++ fromShorthands
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

  def findRearrangableFunctions(equality: Equality): Seq[(BinaryOperator, Commutativity, Associativity)] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    val appropriateInferences = inferences.filter { inference =>
      val substitutions = inference.requiredSubstitutions
      substitutions.statements.isEmpty &&
        substitutions.predicates.isEmpty &&
        substitutions.functions.isEmpty &&
        inference.conclusion.requiredSubstitutions.isEquivalentTo(substitutions)
    }
    appropriateInferences.mapCollect { inference =>
        for {
          (l, r) <- equality.unapply(inference.conclusion)
          operator = BinaryOperator(l)
          (first: TermVariable, second: TermVariable) <- operator.unapply(l)
          if r == operator(second, first)
        } yield (operator, Commutativity(operator, inference.summary, equality))
      }
      .mapCollect { case (operator, commutativity) =>
        appropriateInferences
          .mapFind { inference =>
            for {
              (l, r) <- equality.unapply(inference.conclusion)
              (a: TermVariable, bc) <- operator.unapply(l)
              (b: TermVariable, c: TermVariable) <- operator.unapply(bc)
              if r == operator(operator(a, b), c)
            } yield (operator, commutativity, Associativity(operator, inference.summary, equality))
          }
      }
  }

  def findReversal(relation: BinaryRelation): Option[Reversal] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    inferences.find {
      case Inference(
        _,
        Seq(relation(ExpressionVariable(a), ExpressionVariable(b))),
        relation(ExpressionVariable(c), ExpressionVariable(d))
      ) if a == d && b == c =>
        true
      case _ =>
        false
    }.map(i => Reversal(relation, i.summary))
  }
  def findTransitivity(relation: BinaryRelation): Option[Transitivity] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    inferences.find {
      case Inference(
        _,
        Seq(relation(ExpressionVariable(a), ExpressionVariable(b)), relation(ExpressionVariable(c), ExpressionVariable(d))),
        relation(ExpressionVariable(e), ExpressionVariable(f))
      ) if a == e && b == c && d == f =>
        true
      case _ =>
        false
    }.map(i => Transitivity(relation, i.summary))
  }
  def findExpansion(relation: BinaryRelation): Option[Expansion] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    inferences.find {
      case Inference(
        _,
        Seq(relation(TermVariable(a), TermVariable(b))),
        relation(FunctionApplication(f, Seq(TermVariable(c))), FunctionApplication(g, Seq(TermVariable(d))))
      ) if a == c && b == d && f == g =>
        true
      case _ =>
        false
    }.map(i => Expansion(relation, i.summary))
  }
  def findSubstitution(relation: BinaryRelation): Option[Substitution] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    inferences.find {
      case Inference(
        _,
        Seq(
        relation(TermVariable(a), TermVariable(b)),
          PredicateApplication(phi, Seq(TermVariable(c)))),
        PredicateApplication(psi, Seq(TermVariable(d))))
      if a == c && b == d && phi == psi =>
        true
      case _ =>
        false
    }.map(i => Substitution(relation, i.summary))
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
