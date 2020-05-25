package net.prover.model.expressions

import net.prover.model._
import net.prover.model.entries.{StandalonePropertyDefinition, TypeDefinition, TypeQualifierDefinition}

trait Statement extends Expression with TypedExpression[Statement]

object Statement {
  def parser(implicit context: ExpressionParsingContext): Parser[Statement] = {
    Parser.selectWordParser("statement") {
      case "with" =>
        for {
          arguments <- Term.parser.listInParensOrSingle(None)
          name <- Parser.singleWord
        } yield context.getStatementVariable(name, arguments).getOrElse(throw new Exception(s"Unrecognised statement variable $name"))
      case "is" =>
        typeOrPropertyStatementParser
      case context.entryContext.RecognisedStatementDefinition(statementDefinition) =>
        statementDefinition.statementParser
      case context.SimpleStatementVariable(variable) =>
        Parser.constant(variable)
      case context.entryContext.RecognisedStatementShorthand(template) =>
        template.expressionParser.map(_.asInstanceOf[Statement])
    }
  }

  def typeOrPropertyStatementParser(implicit context: ExpressionParsingContext): Parser[Statement] = {
    for {
      term <- Term.parser
      symbol <- Parser.singleWord
      result <- context.entryContext.typeDefinitions.get(symbol).map(typeStatementParser(term, _)) orElse
        context.entryContext.standalonePropertyDefinitions.find(_.symbol == symbol).map(propertyStatementParser(term, _)) orElse
        typePropertyStatementParser(term, symbol) orElse
        typeObjectStatementParser(term, symbol) getOrElse
        (throw new Exception(s"Unrecognised type or property '$symbol'"))
    } yield result
  }

  def typeStatementParser(term: Term, typeDefinition: TypeDefinition)(implicit context: ExpressionParsingContext): Parser[Statement] = {
    def qualifierParser: Parser[(Option[TypeQualifierDefinition], Seq[Term])] = {
      typeDefinition.qualifier match {
        case Some(qualifier) =>
          qualifier.termNames.map(_ => Term.parser).traverse.map(None -> _)
        case None =>
          for {
            qualifierOption <- Parser.optional(qualifierSymbol => context.entryContext.qualifiersByType.get(typeDefinition.symbol).flatMap(_.find(_.symbol == qualifierSymbol)))
            qualifierTerms <- qualifierOption match {
              case Some(qualifier) =>
                qualifier.qualifier.termNames.map(_ => Term.parser).traverse
              case None =>
                Parser.constant(Nil)
            }
          } yield (qualifierOption, qualifierTerms)
      }
    }

    def propertiesAndObjectStatementsParser(
      explicitQualifierOption: Option[TypeQualifierDefinition],
      mainTerm: Term,
      qualifierTerms: Seq[Term])(
      implicit context: ExpressionParsingContext
    ): Parser[Seq[Statement]] = {
      def getTerms(requiredQualifierOption: Option[TypeQualifierDefinition], description: String): Seq[Term] = {
        requiredQualifierOption match {
          case Some(requiredQualifier) =>
            if (explicitQualifierOption.contains(requiredQualifier))
              mainTerm +: qualifierTerms
            else
              throw new Exception(s"$description on ${typeDefinition.symbol} requires qualifier ${requiredQualifier.symbol}")
          case None =>
            if (typeDefinition.qualifier.nonEmpty)
              mainTerm +: qualifierTerms
            else
              Seq(mainTerm)
        }
      }
      def getProperty(w: String): Option[Parser[Statement]] = {
        context.entryContext.propertyDefinitionsByType.getOrElse(typeDefinition.symbol, Nil).find(_.symbol == w).map { d =>
          val terms = getTerms(d.requiredParentQualifier, s"property ${d.symbol}")
          Parser.constant(d.statementDefinition(terms:_*))
        }
      }
      def getObject(w: String): Option[Parser[Statement]] = {
        context.entryContext.relatedObjectsByType.getOrElse(typeDefinition.symbol, Nil).find(_.symbol == w).map { d =>
          for {
            objectTerm <- Term.parser
            otherTerms = getTerms(d.requiredParentQualifier, s"object ${d.symbol}")
          } yield d.statementDefinition(objectTerm +: otherTerms:_*)
        }
      }
      val parser = for {
        word <- Parser.singleWord
        result <- getProperty(word) orElse getObject(word) getOrElse { throw new Exception(s"Unrecognised property or object $word")}
      } yield result
      parser.listInParensOrSingle(None)
    }

    for {
      qualifierAndTerms <- qualifierParser
      (qualifierOption, qualifierTerms) = qualifierAndTerms
      defaultQualifierComponents = if (typeDefinition.qualifier.isDefined) qualifierTerms else Nil
        qualifierStatementOption = qualifierOption.map(q => q.statementDefinition(term +: qualifierTerms: _*))
      propertiesAndObjectStatements <- Parser.optional("with", propertiesAndObjectStatementsParser(qualifierOption, term, qualifierTerms), Nil)
    } yield {
      val baseStatement = DefinedStatement(term +: defaultQualifierComponents, typeDefinition.statementDefinition)(Nil)
      val statementWithQualifier = qualifierStatementOption.map { qualifierStatement =>
        val conjunctionDefinition = context.entryContext.conjunctionDefinitionOption.getOrElse(throw new Exception("Cannot add a qualifier to a type without a conjunction definition"))
        conjunctionDefinition(baseStatement, qualifierStatement)
      }.getOrElse(baseStatement)
      propertiesAndObjectStatements.foldLeft(statementWithQualifier) { (statement, propertyOrObjectStatement) =>
        val conjunctionDefinition = context.entryContext.conjunctionDefinitionOption.getOrElse(throw new Exception("Cannot add properties or objects to a type without a conjunction definition"))
        conjunctionDefinition(statement, propertyOrObjectStatement)
      }
    }
  }

  def propertyStatementParser(term: Term, standalonePropertyDefinition: StandalonePropertyDefinition)(implicit context: ExpressionParsingContext): Parser[Statement] = {
    Parser.constant(standalonePropertyDefinition.statementDefinition(term))
  }

  def typePropertyStatementParser(mainTerm: Term, symbol: String)(implicit context: ExpressionParsingContext): Option[Parser[Statement]] = {
    for {
      Seq(typeSymbol, propertySymbol) <- "^(\\w+)\\.(\\w+)$".r.unapplySeq(symbol)
      propertyDefinition <- context.entryContext.propertyDefinitionsByType.getOrElse(typeSymbol, Nil).find(_.symbol == propertySymbol)
    } yield propertyDefinition.qualifierTermNames.map(_ => Term.parser).traverse.map(qualifierTerms => propertyDefinition.statementDefinition(mainTerm +: qualifierTerms:_*))
  }

  def typeObjectStatementParser(mainTerm: Term, symbol: String)(implicit context: ExpressionParsingContext): Option[Parser[Statement]] = {
    for {
      Seq(typeSymbol, objectSymbol) <- "^(\\w+)\\.(\\w+)$".r.unapplySeq(symbol)
      objectDefinition <- context.entryContext.relatedObjectsByType.getOrElse(typeSymbol, Nil).find(_.symbol == objectSymbol)
    } yield objectDefinition.parentTermNames.map(_ => Term.parser).traverse.map(parentTerms => objectDefinition.statementDefinition(mainTerm +: parentTerms:_*))
  }

  def listParser(implicit context: ExpressionParsingContext): Parser[Seq[Statement]] = parser.listInParens(Some(","))

  def variableParser(implicit context: ExpressionParsingContext): Parser[StatementVariable] = parser.map {
    case variable: StatementVariable =>
      variable
    case nonVariable =>
      throw new Exception(s"Expected statement variable, got $nonVariable")
  }

  def templateParser(implicit templateParsingContext: TemplateParsingContext): Parser[Template] = {
    Parser.selectWordParser("statement template")(templateParserFunction)
  }

  def templateParserFunction(implicit templateParsingContext: TemplateParsingContext): PartialFunction[String, Parser[Template]] = {
      case ExpressionParsingContext.RecognisedStatementVariableName(name) =>
        Parser.constant(StatementVariableTemplate(name))
      case templateParsingContext.entryContext.RecognisedStatementDefinition(definition) =>
        definition.templateParser
  }
}
