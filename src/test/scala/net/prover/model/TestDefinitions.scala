package net.prover.model

import net.prover.model.Inference.RearrangementType
import net.prover.model.definitions.Definitions
import net.prover.model.entries.ExpressionDefinition.{ComponentType, StatementComponent, TermComponent}
import net.prover.model.entries.{Axiom, ExpressionDefinition, StatementDefinition, TermDefinition, Theorem}
import net.prover.model.expressions.{DefinedStatement, DefinedTerm, ExpressionVariable, FunctionApplication, FunctionParameter, PredicateApplication, Statement, StatementVariable, Term, TermVariable}
import net.prover.model.proof.{Premise, Step, StepContext, StepProvingContext}
import org.specs2.matcher.Matcher

trait VariableDefinitions {
  val φ = StatementVariable("φ")
  val ψ = StatementVariable("ψ")
  val χ = StatementVariable("χ")

  implicit class StatementVariableOps(statementVariable: StatementVariable) {
    def apply(terms: Term*) = PredicateApplication(statementVariable.name, terms)
  }
  implicit def statementVariableToComponentType(statementVariable: StatementVariable): StatementComponent = StatementComponent(statementVariable.name)

  val a = TermVariable("a")
  val b = TermVariable("b")
  val c = TermVariable("c")
  val d = TermVariable("d")
  val A = TermVariable("A")
  val B = TermVariable("B")
  val C = TermVariable("C")
  val D = TermVariable("D")
  val n = TermVariable("n")
  val F = TermVariable("F")

  implicit class TermVariableOps(termVariable: TermVariable) {
    def apply(terms: Term*) = FunctionApplication(termVariable.name, terms)
  }
  implicit def termVariableToComponentType(termVariable: TermVariable): TermComponent = TermComponent(termVariable.name)

  implicit def variableTupleToString[T](tuple: (ExpressionVariable[_], T)): (String, T) = tuple.mapLeft(_.name)
  implicit def variableTupleTupleToString[T, S](tuple: ((ExpressionVariable[_], S), T)): ((String, S), T) = tuple.mapLeft(_.mapLeft(_.name))
}

trait ExpressionDefinitions extends VariableDefinitions {
  private def connective(
    symbol: String,
    size: Int,
    definingStatement: Option[Statement]
  ): StatementDefinition = {
    val componentTypes = Seq[ComponentType](φ, ψ, χ).take(size)
    StatementDefinition(
      symbol,
      Nil,
      componentTypes,
      None,
      Format.default(symbol, componentTypes.map(_.name)),
      definingStatement,
      None,
      Nil)
  }
  private def predicate(
    symbol: String,
    size: Int,
    definingStatement: Option[Statement]
  ): StatementDefinition = {
    val componentTypes = Seq[ComponentType](a, b, c).take(size)
    StatementDefinition(
      symbol,
      Nil,
      componentTypes,
      None,
      Format.default(symbol, componentTypes.map(_.name)),
      definingStatement,
      None,
      Nil)
  }
  private def quantifier(
    symbol: String,
    definingStatement: Option[Statement]
  ): StatementDefinition = {
    StatementDefinition(
      symbol,
      Seq("x"),
      Seq(ExpressionDefinition.PredicateComponent("φ", Seq(ExpressionDefinition.ComponentArgument("x", 0)))),
      None,
      Format.Explicit(s"($symbol%0)%1", s"(${symbol}x)φ", requiresBrackets = false, requiresComponentBrackets = true),
      definingStatement,
      None,
      Nil)
  }

  val Implication = connective("→", 2, None).copy(attributes = Seq("deduction"))
  val Negation = connective("¬", 1, None)
  val Conjunction = connective("∧", 2, Some(Negation(Implication(φ, Negation(ψ)))))
  val Disjunction = connective("∨", 2, Some(Implication(Negation(φ), ψ)))
  val Equivalence = connective("↔", 2, Some(Conjunction(Implication(φ, ψ), Implication(ψ, φ))))

  val ForAll = quantifier("∀", None).copy(attributes = Seq("scoping"))
  val Exists = quantifier("∃", Some(Negation(ForAll("x")(Negation(φ(FunctionParameter(0, 0)))))))
  val Equals = predicate("=", 2, None).copy(attributes = Seq("equality"))
  val ExistsUnique = quantifier("∃!", Some(Exists("y")(ForAll("x")(Equivalence(
    φ(FunctionParameter(0, 0)),
    Equals(FunctionParameter(0, 0), FunctionParameter(0, 1)))))))
  val ElementOf = predicate("∈", 2, None)
  val Subset = predicate("⊆", 2, Some(ForAll("x")(Implication(
    ElementOf(FunctionParameter(0, 0), a),
    ElementOf(FunctionParameter(0, 0), b)))))


  val BlankDefinition = DefinedStatement(Nil, connective("false", 0, None))(Nil)
  val EmptySetDefinition = TermDefinition(
    "∅",
    Nil,
    Nil,
    None,
    Format.default("∅", Nil),
    Nil,
    ForAll("x")(Negation(ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 1)))),
    None,
    Nil)
  val EmptySet = DefinedTerm(Nil, EmptySetDefinition)(Nil)

  val PowerSet = TermDefinition(
    "powerSet",
    Nil,
    Seq(a),
    Some("Power Set"),
    Format.Explicit("𝒫%0", "𝒫a", requiresBrackets = false, requiresComponentBrackets = true),
    Nil,
    ForAll("y")(Equivalence(
      ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 1)),
      Subset(FunctionParameter(0, 0), a))),
    None,
    Nil)

  val Singleton = TermDefinition(
    "singleton",
    Nil,
    Seq(a),
    Some("Singleton"),
    Format.Explicit("{%0}", "{a}", requiresBrackets = false, requiresComponentBrackets = false),
    Nil,
    BlankDefinition,
    None,
    Nil)

  val Pair = TermDefinition(
    "pair",
    Nil,
    Seq(a, b),
    Some("Unordered Pair"),
    Format.Explicit("{%0, %1}", "{a, b}", requiresBrackets = false, requiresComponentBrackets = false),
    Nil,
    BlankDefinition,
    None,
    Nil)
  val Product = TermDefinition(
    "product",
    Nil,
    Seq(a, b),
    Some("Cartesian Product"),
    Format.Explicit("%0 × %1", "a × b", requiresBrackets = true, requiresComponentBrackets = true),
    Nil,
    BlankDefinition,
    None,
    Nil)
  val First = TermDefinition(
    "first",
    Nil,
    Seq(a),
    None,
    Format.Explicit("%0_0", "a_0", requiresBrackets = false, requiresComponentBrackets = true),
    Nil,
    BlankDefinition,
    None,
    Nil)
  val Second = TermDefinition(
    "second",
    Nil,
    Seq(a),
    None,
    Format.Explicit("%0_1", "a_1", requiresBrackets = false, requiresComponentBrackets = true),
    Nil,
    BlankDefinition,
    None,
    Nil)

  val ZeroDefinition = TermDefinition(
    "0",
    Nil,
    Nil,
    None,
    Format.default("0", Nil),
    Nil,
    BlankDefinition,
    None,
    Nil)
  val Zero = DefinedTerm(Nil, ZeroDefinition)(Nil)
  val NaturalsDefinition = TermDefinition(
    "ℕ",
    Nil,
    Nil,
    None,
    Format.default("ℕ", Nil),
    Nil,
    BlankDefinition,
    None,
    Nil)
  val Naturals = DefinedTerm(Nil, NaturalsDefinition)(Nil)
  val Successor = TermDefinition(
    "successor",
    Nil,
    Seq(a),
    None,
    Format.Explicit("a^+", Seq("a"), requiresBrackets = false, requiresComponentBrackets = true),
    Nil,
    BlankDefinition,
    None,
    Nil)
  val AdditionDefinition = TermDefinition(
    "+",
    Nil,
    Nil,
    None,
    Format.default("+", Nil),
    Nil,
    BlankDefinition,
    None,
    Nil)
  val Addition = DefinedTerm(Nil, AdditionDefinition)(Nil)
  val Apply = TermDefinition(
    "apply",
    Nil,
    Seq(a, b),
    None,
    Format.Explicit("%0(%1)", "a(b)", requiresBrackets = false, requiresComponentBrackets = true),
    Nil,
    BlankDefinition,
    None,
    Nil)

  def add(l: Term, r: Term) = Apply(Addition, Pair(l, r))
}

trait InferenceDefinitions extends ExpressionDefinitions {
  val specification = Axiom("Specification", Seq(ForAll("x")(φ(FunctionParameter(0, 0)))), φ(a))
  val modusPonens = Axiom("Modus Ponens", Seq(Implication(φ, ψ), φ), ψ)
  val modusTollens = Axiom("Modus Tollens", Seq(Implication(φ, ψ), Negation(ψ)), Negation(φ))
  val addDoubleNegation = Axiom("Add Double Negation", Seq(φ), Negation(Negation(φ)))
  val removeDoubleNegation = Axiom("Remove Double Negation", Seq(Negation(Negation(φ))), φ)
  val reverseImplicationFromEquivalence = Axiom("Reverse Implication from Equivalence", Seq(Equivalence(φ, ψ)), Implication(ψ, φ))
  val combineConjunction = Axiom("Combine Conjunction", Seq(φ, ψ), Conjunction(φ, ψ))

  val reverseEquality = Axiom("Reverse Equality", Seq(Equals(a, b)), Equals(b, a))
  val equalityIsTransitive = Axiom("Equality Is Transitive", Seq(Equals(a, b), Equals(b, c)), Equals(a, c))
  val substitutionOfEquals = Axiom("Substitution of Equals", Seq(Equals(a, b), φ(a)), φ(b))
  val substitutionOfEqualsIntoFunction = Axiom("Substitution of Equals Into Function", Seq(Equals(a, b)), Equals(F(a), F(b)))

  val elementOfCartesianProductFromCoordinates = Axiom("Element of Cartesian Product from Coordinates", Seq(ElementOf(a, Product(A, B))), Equals(a, Pair(First(a), Second(a))))
  val firstCoordinateOfElementOfCartesianProduct = Axiom("First Coordinate of Element of Cartesian Product", Seq(ElementOf(a, Product(A, B))), ElementOf(First(a), A))
  val secondCoordinateOfElementOfCartesianProduct = Axiom("Second Coordinate of Element of Cartesian Product", Seq(ElementOf(a, Product(A, B))), ElementOf(Second(a), B))
  val firstElement = Axiom("First Element", Nil, Equals(First(Pair(a, b)), a))

  val zeroIsANaturalNumber = Axiom("0 Is a Natural Number", Nil, ElementOf(Zero, Naturals))
  val successorOfNaturalIsNatural = Axiom("A Successor of a Natural Number Is a Natural Number", Seq(ElementOf(a, Naturals)), ElementOf(Successor(a), Naturals))
  val additionIsAssociative = Axiom("Addition Is Associative", Nil, Equals(add(a, add(b, c)), add(add(a, b), c)))
  val additionIsCommutative = Axiom("Addition Is Commutative", Nil, Equals(add(a, b), add(b, a)))
}

object TestDefinitions extends VariableDefinitions with ExpressionDefinitions with InferenceDefinitions  {

  import org.specs2.matcher.MustExpectations._
  import org.specs2.matcher.Matchers._

  implicit val entryContext: EntryContext = EntryContext(
    Seq(
      Implication, Negation, Conjunction, Disjunction, Equivalence,
      ForAll, Exists, ExistsUnique,
      ElementOf, Equals, Subset) ++
    Seq(
      EmptySetDefinition, PowerSet, Singleton, Pair, Product, First, Second,
      ZeroDefinition, NaturalsDefinition, Successor, AdditionDefinition, Apply) ++
    Seq(
      specification, modusPonens, modusTollens, addDoubleNegation, removeDoubleNegation, reverseImplicationFromEquivalence, combineConjunction,
      reverseEquality, equalityIsTransitive, substitutionOfEquals, substitutionOfEqualsIntoFunction,
      elementOfCartesianProductFromCoordinates, firstCoordinateOfElementOfCartesianProduct, secondCoordinateOfElementOfCartesianProduct, firstElement,
      zeroIsANaturalNumber, successorOfNaturalIsNatural, additionIsAssociative, additionIsCommutative),
    Nil)

  implicit class ParserOps[T](parser: Parser[T]) {
    def parseAndDiscard(text: String): T = {
      parser.parseFromString(text, "test")
    }
  }
  implicit def entryContextToParsingContext(implicit entryContext: EntryContext): ExpressionParsingContext = ExpressionParsingContext.outsideProof(entryContext)
  implicit def entryContextToProvingContext(implicit entryContext: EntryContext): ProvingContext = ProvingContext(entryContext, new Definitions(entryContext.availableEntries))
  implicit def entryContextAndStepContextToStepProvingContext(implicit entryContext: EntryContext, stepContext: StepContext): StepProvingContext = {
    StepProvingContext(stepContext, entryContextToProvingContext(entryContext))
  }


  def beValidTheorem(implicit entryContext: EntryContext): Matcher[Theorem] = (theorem: Theorem) => {
    val serializedTheorem = theorem.recalculateReferences(implicitly).serializedLines.mkString("\n").stripPrefix("theorem ")
    val parsedTheorem = Theorem.parser(entryContext).parseFromString(serializedTheorem, "Theorem")
    parsedTheorem must beTypedEqualTo(theorem)
    parsedTheorem.isComplete must beTrue
  }

  def beStepThatMakesValidTheorem(premises: Seq[Statement]): Matcher[Step] = {
    beValidTheorem ^^ { step: Step =>
      Theorem(
        "Test Theorem",
        premises,
        step.provenStatement.get,
        Seq(Theorem.Proof(Seq(step))),
        RearrangementType.NotRearrangement)
    }
  }

  def beStepThatMakesValidTheorem(premises: Seq[Statement], depth: Int)(implicit stepContext: StepContext): Matcher[Step] = {
    if (depth == 0)
      beStepThatMakesValidTheorem(premises)
    else {
      def generalizeOnce(statement: Statement, i: Int): Statement = ForAll(s"x_$i")(statement)
      def generalizeToDepth(statement: Statement, parameterDepth: Int): Statement = (0 until parameterDepth).foldLeft(statement)(generalizeOnce)
      def specificationStep(statement: Statement, parameterDepth: Int) = {
        Step.Assertion(
          statement,
          specification.summary,
          Seq(Premise.Pending(generalizeOnce(statement, parameterDepth).insertExternalParameters(1))),
          Substitutions(predicates = Map((φ, 1) -> statement.specify(Seq(FunctionParameter(0, depth - parameterDepth)), 0, 0).get), terms = Map(a -> FunctionParameter(0, 0))))
      }

      beStepThatMakesValidTheorem(premises.map(generalizeToDepth(_, depth))) ^^ { step: Step =>
        (0 until depth).foldLeft(step) { case (step, i) => Step.ScopedVariable(s"x_$i", premises.map(p => specificationStep(generalizeToDepth(p, i), i)) :+ step, ForAll)}
      }
    }
  }
}
