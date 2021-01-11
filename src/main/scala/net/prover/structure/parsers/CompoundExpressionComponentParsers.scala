package net.prover.structure.parsers

import net.prover.core.expressions.Expression
import net.prover.model.definitions.CompoundExpressionDefinition
import net.prover.model.definitions.CompoundExpressionDefinition.ComponentType
import net.prover.model.definitions.CompoundExpressionDefinition.ComponentType.{StatementComponent, TermComponent}
import net.prover.model.template.Template
import net.prover.model.{ExpressionParsingContext, Parser, TemplateParsingContext}

object CompoundExpressionComponentParsers {

  def boundVariableNamesAndComponentExpressionsParser(compoundExpressionDefinition: CompoundExpressionDefinition)(implicit context: ExpressionParsingContext): Parser[(Seq[String], Seq[Expression])] = {
    for {
      newBoundVariableNames <- Parser.nWords(compoundExpressionDefinition.boundVariableNames.length)
      components <- compoundExpressionDefinition.componentTypes.map(componentExpressionParser(_, newBoundVariableNames)).traverse
    } yield (newBoundVariableNames, components)
  }
  private def componentExpressionParser(componentType: ComponentType, newBoundVariableNames: Seq[String])(implicit context: ExpressionParsingContext): Parser[Expression] = {
    val innerContext = addParametersToContext(componentType, context, newBoundVariableNames)
    componentType match {
      case _: ComponentType.StatementComponent =>
        StatementParsers.statementParser(innerContext)
      case _: ComponentType.TermComponent =>
        TermParsers.termParser(innerContext)
    }
  }

  def boundVariableNamesAndComponentTemplateParser(compoundExpressionDefinition: CompoundExpressionDefinition)(implicit context: TemplateParsingContext): Parser[(Seq[String], Seq[Template])] = {
    for {
      newBoundVariableNames <- Parser.nWords(compoundExpressionDefinition.boundVariableNames.length)
      templates <- compoundExpressionDefinition.componentTypes.map(componentTemplateParser(_, newBoundVariableNames)).traverse
    } yield (newBoundVariableNames, templates)
  }
  private def componentTemplateParser(componentType: ComponentType, newBoundVariableNames: Seq[String])(implicit context: TemplateParsingContext): Parser[Template] = {
    componentType match {
      case statementComponent: StatementComponent =>
        StatementParsers.statementTemplateParser
      case termComponent: TermComponent =>
        TermParsers.termTemplateParser
    }
  }

  private def addParametersToContext(componentType: ComponentType, context: ExpressionParsingContext, newBoundVariableNames: Seq[String]): ExpressionParsingContext = {
    if (newBoundVariableNames.nonEmpty)
      context.addInnerParameters(componentType.arguments.map(a => newBoundVariableNames(a.index) -> a.index))
    else
      context
  }
}
