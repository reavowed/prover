package net.prover.model.expressions

import net.prover.model._
import net.prover.model.entries.{StandalonePropertyDefinition, TypeDefinition}

trait Statement extends Expression with TypedExpression[Statement]

object Statement {
  def parser(implicit context: ExpressionParsingContext): Parser[Statement] = {
    Parser.selectWordParser("statement") {
      case "with" =>
        for {
          arguments <- Term.parser.listOrSingle(None)
          name <- Parser.selectWord("variable name") {
            case ExpressionParsingContext.RecognisedStatementVariableName(name) => name
          }
        } yield StatementVariable(name, arguments)
      case "is" =>
        typeOrPropertyStatementParser
      case context.entryContext.RecognisedStatementDefinition(statementDefinition) =>
        statementDefinition.statementParser
      case ExpressionParsingContext.RecognisedStatementVariableName(name) =>
        Parser.constant(StatementVariable(name, Nil))
      case context.entryContext.RecognisedStatementShorthand(template) =>
        template.expressionParser.map(_.asInstanceOf[Statement])
    }
  }

  def typeOrPropertyStatementParser(implicit context: ExpressionParsingContext): Parser[Statement] = {
    for {
      term <- Term.parser
      symbol <- Parser.singleWord
      result <- context.entryContext.typeDefinitions.find(_.symbol == symbol).map(typeStatementParser(term, _)) orElse
        context.entryContext.standalonePropertyDefinitions.find(_.symbol == symbol).map(propertyStatementParser(term, _)) getOrElse
        (throw new Exception(s"Unrecognised type or property'$symbol'"))
    } yield result
  }

  def typeStatementParser(term: Term, typeDefinition: TypeDefinition)(implicit context: ExpressionParsingContext): Parser[Statement] = {
    for {
      otherComponents <- typeDefinition.qualifier.termNames.map(_ => Term.parser).traverse
      qualifierOption <- Parser.optional(qualifierSymbol => context.entryContext.qualifiersByType.get(typeDefinition.symbol).flatMap(_.find(_.symbol == qualifierSymbol)))
      qualifierStatementOption <- qualifierOption.map(q => {
        q.qualifier.termNames.map(_ => Term.parser).traverse.map(components => q.statementDefinition(term +: components: _*))
      }).traverse
      availableProperties = context.entryContext.propertyDefinitionsByType.get(typeDefinition.symbol).toSeq.flatten
      properties <- Parser.optional("with", Parser.allInParens.map(_.splitByWhitespace().map(s => availableProperties.find(_.symbol == s).getOrElse(throw new Exception(s"Unrecognised property '$s' for '${typeDefinition.symbol}'")))), Nil)
    } yield {
      val baseStatement = DefinedStatement(term +: otherComponents, typeDefinition.statementDefinition)(Nil)
      val statementWithQualifier = qualifierStatementOption.map { qualifierStatement =>
        val conjunctionDefinition = context.entryContext.conjunctionDefinitionOption.getOrElse(throw new Exception("Cannot add properties to a type without a conjunction definition"))
        conjunctionDefinition(baseStatement, qualifierStatement)
      }.getOrElse(baseStatement)
      properties.foldLeft(statementWithQualifier) { (statement, property) =>
        val conjunctionDefinition = context.entryContext.conjunctionDefinitionOption.getOrElse(throw new Exception("Cannot add properties to a type without a conjunction definition"))
        val propertyStatement = property.statementDefinition(term +: otherComponents: _*)
        conjunctionDefinition(statement, propertyStatement)
      }
    }
  }

  def propertyStatementParser(term: Term, standalonePropertyDefinition: StandalonePropertyDefinition)(implicit context: ExpressionParsingContext): Parser[Statement] = {
    Parser.constant(standalonePropertyDefinition.statementDefinition(term))
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
