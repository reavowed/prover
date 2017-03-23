package net.prover.model

import java.nio.file.Paths

import net.prover.model.Inference.{DirectPremise, Premise}
import org.specs2.mutable.Specification

trait ProverSpec extends Specification {

  def φ = StatementVariable("φ")
  def ψ = StatementVariable("ψ")
  def χ = StatementVariable("χ")

  def x = TermVariable("x")
  def y = TermVariable("y")
  def z = TermVariable("z")

  def connective(
    symbol: String,
    size: Int,
    definingStatement: Option[Statement]
  ): StatementDefinition = {
    val variables = Seq(φ, ψ, χ).take(size)
    StatementDefinition(
      symbol,
      variables,
      Format.default(symbol, variables.map(_.text)),
      Nil,
      DistinctVariables.empty,
      definingStatement)
  }
  def predicate(
    symbol: String,
    size: Int,
    definingStatement: Option[Statement]
  ): StatementDefinition = {
    val variables = Seq(x, y, z).take(size)
    StatementDefinition(
      symbol,
      variables,
      Format.default(symbol, variables.map(_.text)),
      Nil,
      DistinctVariables.empty,
      definingStatement)
  }

  def quantifier(
    symbol: String,
    definingStatement: Option[Statement],
    distinctVariables: DistinctVariables
  ): StatementDefinition = {
    StatementDefinition(
      symbol,
      Seq(x, φ),
      Format(s"($symbol%1)%2", requiresBrackets = false),
      Seq(x),
      DistinctVariables.empty,
      definingStatement)
  }


  val Implication = connective("→", 2, None)
  val Negation = connective("¬", 1, None)
  val Conjunction = connective("∧", 2, Some(Negation(Implication(φ, Negation(ψ)))))
  val Disjunction = connective("∨", 2, Some(Implication(Negation(φ), ψ)))
  val Equivalence = connective("↔", 2, None)

  val ForAll = quantifier("∀", None, DistinctVariables.empty)
  var Exists = quantifier("∃", Some(Negation(ForAll(x, Negation(φ)))), DistinctVariables.empty)
  val ElementOf = predicate("∈", 2, None)
  val Equals = predicate("=", 2, None)

  val EmptySetSpecification = TermSpecification("∅", Nil, Format.default("∅", Nil))
  val EmptySet = DefinedTerm(Nil, EmptySetSpecification)
  val EmptySetDefinition = TermDefinition(
    EmptySetSpecification,
    Nil,
    Nil,
    ForAll(x, Negation(ElementOf(x, EmptySet))))

  val statementDefinitions = Seq(
    Implication, Negation, Conjunction, Disjunction, Equivalence,
    ForAll, Exists,
    ElementOf, Equals)

  val baseContext = Context.empty.copy(variables = Variables(
    Seq(φ, ψ, χ),
    Seq(x, y, z)))

  implicit val defaultContext = statementDefinitions.foldLeft(baseContext) { case (context, statementDefinition) =>
    context.addStatementDefinition(statementDefinition)
  }.addTermDefinition(EmptySetDefinition)

  def contextWith(inferences: Inference*): Context = {
    defaultContext.copy(inferences = inferences)
  }

  implicit class ParserOps[T](parser: Parser[T]) {
    def parseAndDiscard(text: String): T = {
      parser.parse(Tokenizer.fromString(text, Paths.get("")))._1
    }
  }

  implicit def statementToPremise(statement: Statement): Premise = DirectPremise(statement)
  implicit def statementToProvenStatement(statement: Statement): ProvenStatement = ProvenStatement.withNoConditions(statement)
}
