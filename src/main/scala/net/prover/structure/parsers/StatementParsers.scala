package net.prover.structure.parsers

import net.prover.core.expressions.{CompoundStatement, Statement, Term}
import net.prover.model.definitions.CompoundStatementDefinition
import net.prover.model.expressions.{CompoundStatementTemplate, StatementVariableTemplate}
import net.prover.model.template.{CompoundStatementTemplate, StatementTemplate, StatementVariableTemplate}
import net.prover.model.{ExpressionParsingContext, Parser, TemplateParsingContext}
import net.prover.structure.model.entries.{StandalonePropertyDefinition, TypeDefinition, TypeQualifierDefinition}
import net.prover.structure.parsers.CompoundExpressionComponentParsers._
import net.prover.structure.parsers.TermParsers._

object StatementParsers {
  def statementParser(implicit context: ExpressionParsingContext): Parser[Statement] = {
    Parser.selectWordParser("statement") {
      case "with" =>
        for {
          arguments <- termParser.listInParensOrSingle(None)
          name <- Parser.singleWord
        } yield context.getStatementVariable(name, arguments).getOrElse(throw new Exception(s"Unrecognised statement variable $name"))
      case "is" =>
        typeOrPropertyStatementParser
      case context.entryContext.RecognisedCompoundStatementDefinition(statementDefinition) =>
        compoundStatementParser(statementDefinition)
      case context.SimpleStatementVariable(variable) =>
        Parser.constant(variable)
      case context.entryContext.RecognisedStatementShorthand(template) =>
        template.expressionParser.map(_.asInstanceOf[Statement])
    }
  }

  def compoundStatementParser(compoundStatementDefinition: CompoundStatementDefinition)(implicit context: ExpressionParsingContext): Parser[CompoundStatement] = {
    boundVariableNamesAndComponentExpressionsParser(compoundStatementDefinition).map { case (newBoundVariableNames, components) =>
      CompoundStatement(compoundStatementDefinition, components)(newBoundVariableNames)
    }
  }

  def typeOrPropertyStatementParser(implicit context: ExpressionParsingContext): Parser[Statement] = {
    for {
      term <- termParser
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
      typeDefinition.defaultQualifier match {
        case Some(qualifier) =>
          qualifier.variableDefinitions.map(_ => termParser).traverse.map(None -> _)
        case None =>
          for {
            qualifierOption <- Parser.optional(qualifierSymbol => context.entryContext.qualifiersByType.get(typeDefinition.symbol).flatMap(_.find(_.symbol == qualifierSymbol)))
            qualifierTerms <- qualifierOption match {
              case Some(qualifier) =>
                qualifier.qualifier.variableDefinitions.map(_ => termParser).traverse
              case None =>
                Parser.constant(Nil)
            }
          } yield (qualifierOption, qualifierTerms)
      }
    }

    def propertiesAndObjectStatementsParser(
      explicitQualifierOption: Option[TypeQualifierDefinition],
      mainTerm: Term,
      qualifierTerms: Seq[Term]
    )(
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
            if (typeDefinition.defaultQualifier.nonEmpty)
              mainTerm +: qualifierTerms
            else
              Seq(mainTerm)
        }
      }

      def getProperty(w: String): Option[Parser[Statement]] = {
        context.entryContext.propertyDefinitionsByType.getOrElse(typeDefinition.symbol, Nil).find(_.symbol == w).map { d =>
          val terms = getTerms(d.parentTypeConditions.requiredParentQualifier, s"property ${d.symbol}")
          Parser.constant(d.statementDefinition(terms: _*))
        }
      }

      def getObject(w: String): Option[Parser[Statement]] = {
        context.entryContext.relatedObjectsByType.getOrElse(typeDefinition.symbol, Nil).find(_.symbol == w).map { d =>
          for {
            objectTerm <- termParser
            otherTerms = getTerms(d.parentTypeConditions.requiredParentQualifier, s"object ${d.symbol}")
          } yield d.statementDefinition(objectTerm +: otherTerms: _*)
        }
      }

      val parser = for {
        word <- Parser.singleWord
        result <- getProperty(word) orElse getObject(word) getOrElse {
          throw new Exception(s"Unrecognised property or object $word")
        }
      } yield result
      parser.listInParensOrSingle(None)
    }

    for {
      qualifierAndTerms <- qualifierParser
      (qualifierOption, qualifierTerms) = qualifierAndTerms
      defaultQualifierComponents = if (typeDefinition.defaultQualifier.isDefined) qualifierTerms else Nil
      qualifierStatementOption = qualifierOption.map(q => q.statementDefinition(term +: qualifierTerms: _*))
      propertiesAndObjectStatements <- Parser.optional("with", propertiesAndObjectStatementsParser(qualifierOption, term, qualifierTerms), Nil)
    } yield {
      val baseStatement = CompoundStatement(typeDefinition.statementDefinition, term +: defaultQualifierComponents)(Nil)
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
    } yield propertyDefinition.parentTypeConditions.qualifierVariableDefinitions.map(_ => termParser).traverse.map(qualifierTerms => propertyDefinition.statementDefinition(mainTerm +: qualifierTerms: _*))
  }

  def typeObjectStatementParser(mainTerm: Term, symbol: String)(implicit context: ExpressionParsingContext): Option[Parser[Statement]] = {
    for {
      Seq(typeSymbol, objectSymbol) <- "^(\\w+)\\.(\\w+)$".r.unapplySeq(symbol)
      objectDefinition <- context.entryContext.relatedObjectsByType.getOrElse(typeSymbol, Nil).find(_.symbol == objectSymbol)
    } yield objectDefinition.parentVariableDefinitions.map(_ => termParser).traverse.map(parentTerms => objectDefinition.statementDefinition(mainTerm +: parentTerms: _*))
  }

  def statementListParser(implicit context: ExpressionParsingContext): Parser[Seq[Statement]] = statementParser.listInParens(Some(","))


  def statementTemplateParser(implicit context: TemplateParsingContext): Parser[StatementTemplate] = {
    Parser.selectWordParser("statement template")(statementTemplateParserFunction)
  }

  def statementTemplateParserFunction(implicit context: TemplateParsingContext): PartialFunction[String, Parser[StatementTemplate]] = {
    case ExpressionParsingContext.RecognisedStatementVariableName(name) =>
      Parser.constant(StatementVariableTemplate(name))
    case context.entryContext.RecognisedCompoundStatementDefinition(definition) =>
      compoundStatementTemplateParser(definition)
  }

  def compoundStatementTemplateParser(compoundStatementDefinition: CompoundStatementDefinition)(implicit context: TemplateParsingContext): Parser[CompoundStatementTemplate] = {
    boundVariableNamesAndComponentTemplateParser(compoundStatementDefinition).map { case (boundVariableNames, componentTemplates) =>
      CompoundStatementTemplate(compoundStatementDefinition, boundVariableNames, componentTemplates)
    }
  }
}
