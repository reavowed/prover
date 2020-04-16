package net.prover.model

import net.prover.model.TestDefinitions.{A, BlankDefinition, Conjunction, ElementOf, Equivalence, ForAll, φ}
import net.prover.model.definitions.Definitions
import net.prover.model.entries.ExpressionDefinition.{ComponentArgument, ComponentType}
import net.prover.model.entries.ExpressionDefinition.ComponentType.{StatementComponent, TermComponent}
import net.prover.model.entries._
import net.prover.model.expressions._
import net.prover.model.proof.{Premise, Step, StepContext, StepProvingContext}
import org.specs2.matcher.Matcher

trait VariableDefinitions {
  trait Placeholder[T <: ExpressionVariable[_ <: Expression]] {
    def name: String
    def toVariable: T
    def ->[B, A1, B1](b: B)(implicit f1: Placeholder[T] => A1, f2: B => B1): (A1, B1) = (f1(this), f2(b))
    def ->[B, A1, B1](i: Int, b: B)(implicit f1: Placeholder[T] => A1, f2: B => B1): (A1, (Int, B1)) = (f1(this), (i, f2(b)))
  }
  case class StatementVariablePlaceholder(name: String) extends Placeholder[StatementVariable] {
    def apply(terms: Term*) = StatementVariable(name, terms)
    override def toVariable = StatementVariable(name, Nil)
  }
  implicit def placeholderToStatementComponent(placeholder: StatementVariablePlaceholder): StatementComponent = StatementComponent(placeholder.name, Nil)

  val φ = StatementVariablePlaceholder("φ")
  val ψ = StatementVariablePlaceholder("ψ")
  val χ = StatementVariablePlaceholder("χ")
  val ω = StatementVariablePlaceholder("ω")

  case class TermVariablePlaceholder(name: String) extends Placeholder[TermVariable] {
    def apply(terms: Term*) = TermVariable(name, terms)
    override def toVariable = TermVariable(name, Nil)
    def template: Template = TermVariableTemplate(name)
  }
  implicit def placeholderToTermComponent(placeholder: TermVariablePlaceholder): TermComponent = TermComponent(placeholder.name, Nil)

  case object $ {
    def template: Template = FunctionParameterTemplate($ToFunctionParameter(this))
    def apply(index: Int) = FunctionParameter(index, 0)
    def ^ : FunctionParameter = FunctionParameter(0, 1)
    def ^^ : FunctionParameter = ^^(0)
    def ^^(index: Int) : FunctionParameter = FunctionParameter(index, 2)
    def ^^^ : FunctionParameter = FunctionParameter(0, 3)
    def ^^^^ : FunctionParameter = FunctionParameter(0, 4)
  }
  implicit def $ToFunctionParameter(x: $.type): FunctionParameter = FunctionParameter(0, 0)


  implicit class StatementDefinitionOps(statementDefinition: StatementDefinition) {
    def template(components: Template*): Template = DefinedStatementTemplate(statementDefinition, Nil, components)
  }
  implicit class TermDefinitionOps(termDefinition: TermDefinition) {
    def template(components: Template*): Template = DefinedTermTemplate(termDefinition, Nil, components)
  }
  implicit class FunctionParameterOps(functionParameter: FunctionParameter) {
    def template: Template = FunctionParameterTemplate(functionParameter)
  }

  val a = TermVariablePlaceholder("a")
  val b = TermVariablePlaceholder("b")
  val c = TermVariablePlaceholder("c")
  val d = TermVariablePlaceholder("d")
  val A = TermVariablePlaceholder("A")
  val B = TermVariablePlaceholder("B")
  val C = TermVariablePlaceholder("C")
  val D = TermVariablePlaceholder("D")
  val X = TermVariablePlaceholder("X")
  val n = TermVariablePlaceholder("n")
  val F = TermVariablePlaceholder("F")
  val x = TermVariablePlaceholder("x")

  implicit def placeholderToVariable[T <: ExpressionVariable[_ <: Expression]](placeholder: Placeholder[T]): T = placeholder.toVariable
  implicit def placeholderToString(placeholder: Placeholder[_]): String = placeholder.name
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
      Format.default(Nil, componentTypes),
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
      Format.default(Nil, componentTypes),
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
      Seq(StatementComponent("φ", Seq(ExpressionDefinition.ComponentArgument("x", 0)))),
      None,
      Format.Explicit(s"(%0%1)%2", s"(${symbol}x)φ", requiresBrackets = false, requiresComponentBrackets = true),
      definingStatement,
      None,
      Nil)
  }

  val Implication = connective("→", 2, None).copy(attributes = Seq("deduction"))
  val Negation = connective("¬", 1, None)
  val Conjunction = connective("∧", 2, Some(Negation(Implication(φ, Negation(ψ)))))
  val Disjunction = connective("∨", 2, Some(Implication(Negation(φ), ψ)))
  val Equivalence = connective("↔", 2, Some(Conjunction(Implication(φ, ψ), Implication(ψ, φ))))

  val ForAllDefinition = quantifier("∀", None).copy(attributes = Seq("generalization"))
  def ForAll(name: String)(expression: Statement) = ForAllDefinition.bind(name)(expression)
  def ForAllIn(name: String, term: Term)(expression: Statement) = ForAll(name)(Implication(ElementOf($, term), expression))
  val ExistsDefinition = quantifier("∃", Some(Negation(ForAll("x")(Negation(φ($))))))
  def Exists(name: String)(expression: Statement) = ExistsDefinition.bind(name)(expression)
  def ExistsIn(name: String, term: Term)(expression: Statement) = Exists(name)(Conjunction(ElementOf($, term), expression))
  val Equals = predicate("=", 2, None).copy(attributes = Seq("equality"))
  val ExistsUnique = quantifier("∃!", Some(Exists("y")(ForAll("x")(Equivalence(φ($), Equals($, $.^))))))
  val ElementOf = predicate("∈", 2, None)
  val Subset = predicate("⊆", 2, Some(ForAll("x")(Implication(ElementOf($, a), ElementOf($, b)))))


  val BlankDefinition = DefinedStatement(Nil, connective("false", 0, None))(Nil)
  val EmptySetDefinition = TermDefinition(
    "∅",
    Nil,
    Nil,
    None,
    None,
    Format.default(Nil, Nil),
    Nil,
    ForAll("x")(Negation(ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 1)))),
    None,
    Nil,
    Nil)
  val EmptySet = DefinedTerm(Nil, EmptySetDefinition)(Nil)

  val PowerSet = TermDefinition(
    "powerSet",
    Nil,
    Seq(a),
    None,
    Some("Power Set"),
    Format.Explicit("𝒫%1", "𝒫a", requiresBrackets = false, requiresComponentBrackets = true),
    Nil,
    ForAll("y")(Equivalence(
      ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 1)),
      Subset(FunctionParameter(0, 0), a))),
    None,
    Nil,
    Nil)

  val Singleton = TermDefinition(
    "singleton",
    Nil,
    Seq(a),
    None,
    Some("Singleton"),
    Format.Explicit("{%1}", "{a}", requiresBrackets = false, requiresComponentBrackets = false),
    Nil,
    BlankDefinition,
    None,
    Nil,
    Nil)

  val Pair = TermDefinition(
    "pair",
    Nil,
    Seq(a, b),
    None,
    Some("Unordered Pair"),
    Format.Explicit("{%1, %2}", "{a, b}", requiresBrackets = false, requiresComponentBrackets = false),
    Nil,
    BlankDefinition,
    None,
    Nil,
    Nil)
  val Product = TermDefinition(
    "product",
    Nil,
    Seq(a, b),
    None,
    Some("Cartesian Product"),
    Format.Explicit("%1 × %2", "a × b", requiresBrackets = true, requiresComponentBrackets = true),
    Nil,
    BlankDefinition,
    None,
    Nil,
    Nil)
  val First = TermDefinition(
    "first",
    Nil,
    Seq(a),
    None,
    None,
    Format.Explicit("%1_0", "a_0", requiresBrackets = false, requiresComponentBrackets = true),
    Nil,
    BlankDefinition,
    None,
    Nil,
    Nil)
  val Second = TermDefinition(
    "second",
    Nil,
    Seq(a),
    None,
    None,
    Format.Explicit("%1_1", "a_1", requiresBrackets = false, requiresComponentBrackets = true),
    Nil,
    BlankDefinition,
    None,
    Nil,
    Nil)

  val Comprehension = TermDefinition(
    "comprehension",
    Seq("a"),
    Seq(ComponentType.TermComponent("A", Nil), ComponentType.StatementComponent("φ", Seq(ComponentArgument("a", 0)))),
    None,
    None,
    Format.Explicit("{ a ∈ A | φ }", Seq("a", "A", "φ"), false, true),
    Nil,
    ForAll("a")(Equivalence(ElementOf($, $.^), Conjunction(ElementOf($, A), φ($)))),
    None,
    Nil,
    Nil)

  val NaturalsDefinition = TermDefinition(
    "ℕ",
    Nil,
    Nil,
    None,
    None,
    Format.default(Nil, Nil),
    Nil,
    BlankDefinition,
    None,
    Nil,
    Nil)
  val Naturals = DefinedTerm(Nil, NaturalsDefinition)(Nil)
  val Successor = TermDefinition(
    "successor",
    Nil,
    Seq(a),
    None,
    None,
    Format.Explicit("a^+", Seq("successor", "a"), requiresBrackets = false, requiresComponentBrackets = true),
    Nil,
    BlankDefinition,
    None,
    Nil,
    Nil)
  val ZeroDefinition = TermDefinition(
    "0",
    Nil,
    Nil,
    Some("ℕ"),
    None,
    Format.default(Nil, Nil),
    Nil,
    BlankDefinition,
    None,
    Nil,
    Nil)
  val Zero = DefinedTerm(Nil, ZeroDefinition)(Nil)
  val OneDefinition = TermDefinition(
    "1",
    Nil,
    Nil,
    Some("ℕ"),
    None,
    Format.default(Nil, Nil),
    Nil,
    Equals($, Successor(Zero)),
    None,
    Nil,
    Nil)
  val One = DefinedTerm(Nil, OneDefinition)(Nil)
  val AdditionDefinition = TermDefinition(
    "+",
    Nil,
    Nil,
    Some("ℕ"),
    None,
    Format.default(Nil, Nil),
    Nil,
    BlankDefinition,
    None,
    Nil,
    Nil)
  val Addition = DefinedTerm(Nil, AdditionDefinition)(Nil)
  val Apply = TermDefinition(
    "apply",
    Nil,
    Seq(a, b),
    None,
    None,
    Format.Explicit("%1(%2)", "a(b)", requiresBrackets = false, requiresComponentBrackets = true),
    Nil,
    BlankDefinition,
    None,
    Nil,
    Nil)
  val LessThanDefinition = TermDefinition(
    "<",
    Nil,
    Nil,
    Some("ℕ"),
    None,
    Format.default(Nil, Nil),
    Nil,
    BlankDefinition,
    None,
    Seq("infix-relation"),
    Nil)
  val LessThan = DefinedTerm(Nil, LessThanDefinition)(Nil)
  def lessThan(a: Term, b: Term): Statement = ElementOf(Pair(a, b), LessThan)

  val IntegersDefinition = TermDefinition(
    "ℤ",
    Nil,
    Nil,
    None,
    None,
    Format.default(Nil, Nil),
    Nil,
    BlankDefinition,
    None,
    Nil,
    Nil)
  val Integers = IntegersDefinition()

  val InfixRelationShorthand = DisplayShorthand(
    ElementOf.template(Pair.template(a.template, b.template), TermVariableTemplate("<")),
    Format.Explicit("a < b", Seq("a", "b", "<"), false, false),
    Seq(("<", "infix-relation")))

  val NotElementOfShorthand = DisplayShorthand(
    Negation.template(ElementOf.template(a.template, b.template)),
    Format.Explicit("a ∉ b", Seq("a", "b"), false, false),
    Nil)
  val NotEqualShorthand = DisplayShorthand(
    Negation.template(Equals.template(a.template, b.template)),
    Format.Explicit("a ≠ b", Seq("a", "b"), false, false),
    Nil)

  def add(l: Term, r: Term) = Apply(Addition, Pair(l, r))
}

trait InferenceDefinitions extends ExpressionDefinitions {
  val specification = Axiom("Specification", Seq(ForAll("x")(φ($))), φ(a))
  val existence = Axiom("Existence", Seq(φ(a)), Exists("x")(φ($)))
  val modusPonens = Axiom("Modus Ponens", Seq(Implication(φ, ψ), φ), ψ)
  val modusTollens = Axiom("Modus Tollens", Seq(Implication(φ, ψ), Negation(ψ)), Negation(φ))

  val addDoubleNegation = Axiom("Add Double Negation", Seq(φ), Negation(Negation(φ)))
  val removeDoubleNegation = Axiom("Remove Double Negation", Seq(Negation(Negation(φ))), φ)

  val extractLeftConjunct = Axiom("Extract Left Conjunct", Seq(Conjunction(φ, ψ)), φ)
  val extractRightConjunct = Axiom("Extract Right Conjunct", Seq(Conjunction(φ, ψ)), ψ)
  val combineConjunction = Axiom("Combine Conjunction", Seq(φ, ψ), Conjunction(φ, ψ))

  val addLeftDisjunct = Axiom("Add Left Disjunct", Seq(φ), Disjunction(ψ, φ))
  val addRightDisjunct = Axiom("Add Right Disjunct", Seq(φ), Disjunction(φ, ψ))

  val reverseEquivalence = Axiom("Reverse Equivalence", Seq(Equivalence(φ, ψ)), Equivalence(ψ, φ))
  val equivalenceIsTransitive = Axiom("Equivalence Is Transitive", Seq(Equivalence(φ, ψ), Equivalence(ψ, χ)), Equivalence(φ, χ))
  val forwardImplicationFromEquivalence = Axiom("Forward Implication from Equivalence", Seq(Equivalence(φ, ψ)), Implication(φ, ψ))
  val reverseImplicationFromEquivalence = Axiom("Reverse Implication from Equivalence", Seq(Equivalence(φ, ψ)), Implication(ψ, φ))

  val distributeImplicationOverEquivalence = Axiom("Distribute Implication over Equivalence", Seq(Implication(φ, Equivalence(ψ, χ))), Equivalence(Implication(φ, ψ), Implication(φ, χ)))
  val distributeUniversalQuantifierOverEquivalence = Axiom("Distribute Universal Quantifier over Equivalence", Seq(ForAll("x")(Equivalence(φ($), ψ($)))), Equivalence(ForAll("x")(φ($)), ForAll("x")(ψ($))))

  val reverseEquality = Axiom("Reverse Equality", Seq(Equals(a, b)), Equals(b, a))
  val reverseNegatedEquality = Axiom("Reverse Negated Equality", Seq(Negation(Equals(a, b))), Negation(Equals(b, a)))
  val equalityIsTransitive = Axiom("Equality Is Transitive", Seq(Equals(a, b), Equals(b, c)), Equals(a, c))
  val substitutionOfEquals = Axiom("Substitution of Equals", Seq(Equals(a, b), φ(a)), φ(b))
  val substitutionOfEqualsIntoFunction = Axiom("Substitution of Equals Into Function", Seq(Equals(a, b)), Equals(F(a), F(b)))
  val equivalenceOfSubstitutedEquals = Axiom("Equivalence of Substituted Equals", Seq(Equals(a, b)), Equivalence(φ(a), φ(b)))

  val membershipConditionForSingleton = Axiom("Membership Condition for Singleton", Nil, ForAll("x")(Equivalence(ElementOf($, Singleton(a)), Equals($, a))))
  val elementOfCartesianProductFromCoordinates = Axiom("Element of Cartesian Product from Coordinates", Seq(ElementOf(a, Product(A, B))), Equals(a, Pair(First(a), Second(a))))
  val firstCoordinateOfOrderedPairInCartesianProduct = Axiom("First Coordinate of Ordered Pair in Cartesian Product", Seq(ElementOf(Pair(a, b), Product(A, B))), ElementOf(a, A))
  val firstCoordinateOfElementOfCartesianProduct = Axiom("First Coordinate of Element of Cartesian Product", Seq(ElementOf(a, Product(A, B))), ElementOf(First(a), A))
  val secondCoordinateOfElementOfCartesianProduct = Axiom("Second Coordinate of Element of Cartesian Product", Seq(ElementOf(a, Product(A, B))), ElementOf(Second(a), B))
  val orderedPairIsElementOfCartesianProduct = Axiom("Ordered Pair Is Element of Cartesian Product", Seq(ElementOf(a, A), ElementOf(b, B)), ElementOf(Pair(a, b), Product(A, B)))
  val firstElement = Axiom("First Element", Nil, Equals(First(Pair(a, b)), a))

  val zeroIsANaturalNumber = Axiom("0 Is a Natural Number", Nil, ElementOf(Zero, Naturals))
  val oneIsANaturalNumber = Axiom("1 Is a Natural Number", Nil, ElementOf(One, Naturals))
  val successorOfNaturalIsNatural = Axiom("A Successor of a Natural Number Is a Natural Number", Seq(ElementOf(a, Naturals)), ElementOf(Successor(a), Naturals))
  val additionIsClosed = Axiom("Addition Is Closed", Seq(ElementOf(a, Naturals), ElementOf(b, Naturals)), ElementOf(add(a, b), Naturals))
  val additionIsAssociative = Axiom("Addition Is Associative", Nil, Equals(add(a, add(b, c)), add(add(a, b), c)))
  val additionIsCommutative = Axiom("Addition Is Commutative", Nil, Equals(add(a, b), add(b, a)))
  val addingZeroIsSame = Axiom("Adding Zero Is Same", Nil, Equals(a, add(a, Zero)))
  val orderingIsTransitive = Axiom("Natural Ordering Is Transitive", Seq(lessThan(a, b), lessThan(b, c)), lessThan(a, c))
}

object TestDefinitions extends VariableDefinitions with ExpressionDefinitions with InferenceDefinitions  {

  import org.specs2.matcher.Matchers._
  import org.specs2.matcher.MustExpectations._

  implicit val defaultEntryContext: EntryContext = EntryContext(
    Seq(
      Implication, Negation, Conjunction, Disjunction, Equivalence,
      ForAllDefinition, ExistsDefinition, ExistsUnique,
      ElementOf, Equals, Subset) ++
    Seq(
      EmptySetDefinition, PowerSet, Singleton, Pair, Product, First, Second, Comprehension,
      NaturalsDefinition, Successor, ZeroDefinition, OneDefinition, AdditionDefinition, Apply, LessThanDefinition,
      IntegersDefinition) ++
    Seq(
      specification, existence, modusPonens, modusTollens,
      addDoubleNegation, removeDoubleNegation,
      extractLeftConjunct, extractRightConjunct, combineConjunction,
      addLeftDisjunct, addRightDisjunct,
      reverseEquivalence, equivalenceIsTransitive, forwardImplicationFromEquivalence, reverseImplicationFromEquivalence,
      distributeImplicationOverEquivalence, distributeUniversalQuantifierOverEquivalence,
      reverseEquality, reverseNegatedEquality, equalityIsTransitive, substitutionOfEquals, substitutionOfEqualsIntoFunction, equivalenceOfSubstitutedEquals,
      membershipConditionForSingleton, elementOfCartesianProductFromCoordinates, firstCoordinateOfOrderedPairInCartesianProduct, firstCoordinateOfElementOfCartesianProduct, secondCoordinateOfElementOfCartesianProduct, orderedPairIsElementOfCartesianProduct, firstElement,
      zeroIsANaturalNumber, oneIsANaturalNumber, successorOfNaturalIsNatural, additionIsClosed, additionIsAssociative, additionIsCommutative, addingZeroIsSame, orderingIsTransitive) ++
    Seq(InfixRelationShorthand, NotElementOfShorthand, NotEqualShorthand))

  implicit class ParserOps[T](parser: Parser[T]) {
    def parseAndDiscard(text: String): T = {
      parser.parseFromString(text, "test")
    }
  }
  implicit def entryContextToParsingContext(implicit entryContext: EntryContext): ExpressionParsingContext = ExpressionParsingContext.outsideProof(entryContext)
  implicit def entryContextToProvingContext(implicit entryContext: EntryContext): ProvingContext = ProvingContext(entryContext, new Definitions(entryContext))
  implicit def entryContextAndStepContextToStepProvingContext(implicit entryContext: EntryContext, stepContext: StepContext): StepProvingContext = {
    StepProvingContext(stepContext, entryContextToProvingContext(entryContext))
  }

  def beValidTheorem(implicit entryContext: EntryContext): Matcher[Theorem] = (theorem: Theorem) => {
    val serializedTheorem = theorem.recalculateReferences(entryContextToProvingContext(entryContext))._1.serializedLines.mkString("\n").stripPrefix("theorem ")
    val parsedTheorem = Theorem.parser(entryContext).parseFromString(serializedTheorem, "Theorem")
    parsedTheorem must beTypedEqualTo(theorem)
    parsedTheorem.isComplete(new Definitions(entryContext)) must beTrue
  }

  def beStepsThatMakeValidTheorem(premises: Seq[Statement], conclusion: Statement)(implicit entryContext: EntryContext): Matcher[Seq[Step]] = {
    beValidTheorem(entryContext) ^^ { steps: Seq[Step] =>
      Theorem(
        "Test Theorem",
        premises,
        conclusion,
        Seq(Theorem.Proof(steps)))
    }
  }

  def beStepThatMakesValidTheorem(premises: Seq[Statement], conclusion: Statement, depth: Int = 0)(implicit stepContext: StepContext): Matcher[Step] = {
    beStepsThatMakeValidTheorem(premises, conclusion, depth) ^^ { step: Step => Seq(step) }
  }

  def beStepsThatMakeValidTheorem(premises: Seq[Statement], conclusion: Statement, depth: Int)(implicit stepContext: StepContext): Matcher[Seq[Step]] = {
    if (depth == 0)
      beStepsThatMakeValidTheorem(premises, conclusion)
    else {
      def generalizeOnce(statement: Statement, i: Int): Statement = ForAll(s"x_$i")(statement)
      def generalizeToDepth(statement: Statement, parameterDepth: Int): Statement = (0 until parameterDepth).foldLeft(statement)(generalizeOnce)
      def specificationStep(statement: Statement, parameterDepth: Int) = {
        Step.Assertion(
          statement,
          specification.summary,
          Seq(Premise.Pending(generalizeOnce(statement, parameterDepth).insertExternalParameters(1))),
          Substitutions(statements = Map(φ -> (1, statement.specify(Seq(FunctionParameter(0, depth - parameterDepth)), 0, 0).get)), terms = Map(a -> (0, FunctionParameter(0, 0)))))
      }
      beStepsThatMakeValidTheorem(premises.map(generalizeToDepth(_, depth)), generalizeToDepth(conclusion, depth)) ^^ { steps: Seq[Step] =>
        (0 until depth).foldLeft(steps) { case (steps, i) => Seq(Step.Generalization(s"x_$i", premises.map(p => specificationStep(generalizeToDepth(p, i), i)) ++ steps, ForAllDefinition))}
      }
    }
  }
}
