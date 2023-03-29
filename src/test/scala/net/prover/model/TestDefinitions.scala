package net.prover.model

import net.prover.books.model.EntryParsingContext
import net.prover.books.reading.ProofFileReader
import net.prover.entries.TheoremWithContext
import net.prover.model.TestDefinitions.{DeductionDefinition, GeneralizationDefinition}
import net.prover.model.definitions.ExpressionDefinition.ComponentType.{StatementComponent, TermComponent}
import net.prover.model.definitions.ExpressionDefinition.{ComponentArgument, ComponentType}
import net.prover.model.definitions._
import net.prover.model.entries.ChapterEntry.HasStatementDefinition
import net.prover.model.entries._
import net.prover.model.expressions._
import net.prover.model.proof._
import net.prover.theorems.RecalculateReferences
import org.mockito.Mockito.when
import org.specs2.matcher.Matcher
import org.specs2.mock.mockito.MockitoStubs

trait Placeholder[T <: ExpressionVariable[_ <: Expression]] {
  def name: String
  def index: Int
  def toVariable: T
}
case class StatementVariablePlaceholder(name: String, index: Int) extends Placeholder[StatementVariable] {
  def apply(terms: Term*) = StatementVariable(index, terms)
  override def toVariable = apply()
  def toComponent(arguments: ComponentArgument*): StatementComponent = StatementComponent(name, arguments)
  def ->(statement: Statement): (Statement, Statement) = toVariable -> statement
}
case class TermVariablePlaceholder(name: String, index: Int) extends Placeholder[TermVariable] {
  def apply(terms: Term*) = TermVariable(index, terms)
  override def toVariable = apply()
  def template: Template = TermVariableTemplate(name)
  def ->(term: Term): (Term, Term) = toVariable -> term
}

trait TestVariableDefinitions {

  implicit def placeholderToStatementComponent(placeholder: StatementVariablePlaceholder): StatementComponent = placeholder.toComponent()
  implicit def placeholderToTermComponent(placeholder: TermVariablePlaceholder): TermComponent = TermComponent(placeholder.name, Nil)
  implicit def placeholderToVariable[T <: ExpressionVariable[_ <: Expression]](placeholder: Placeholder[T]): T = placeholder.toVariable
  implicit def stringToVariableDefinition(name: String): SimpleVariableDefinition = SimpleVariableDefinition(name, Nil)

  val φ = StatementVariablePlaceholder("φ", 0)
  val ψ = StatementVariablePlaceholder("ψ", 1)
  val χ = StatementVariablePlaceholder("χ", 2)
  val ω = StatementVariablePlaceholder("ω", 3)


  case object $ {
    def template: Template = FunctionParameterTemplate($ToFunctionParameter(this))
    def apply(index: Int) = FunctionParameter(index, 0)
    def ^ : FunctionParameter = FunctionParameter(0, 1)
    def ^(index: Int) : FunctionParameter = FunctionParameter(index, 1)
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
  implicit class HasStatementDefinitionOps(hasStatementDefinition: HasStatementDefinition) {
    def apply(components: Expression*) = hasStatementDefinition.statementDefinition(components: _*)
    def deconstructionInference: Inference.StatementDefinition = hasStatementDefinition.statementDefinition.deconstructionInference.get
  }

  val a = TermVariablePlaceholder("a", 0)
  val b = TermVariablePlaceholder("b", 1)
  val c = TermVariablePlaceholder("c", 2)
  val d = TermVariablePlaceholder("d", 3)

  protected val R = TermVariablePlaceholder("R", 0)
  protected val f = TermVariablePlaceholder("f", 0)
  protected val A = TermVariablePlaceholder("A", 1)
  protected val B = TermVariablePlaceholder("B", 2)


  def getVariableDefinitionsFromStatements(statements: Seq[Statement]): VariableDefinitions = {
    val usedVariables = statements.usedVariables
    val statementVariableDefinitions = Seq(φ, ψ, χ, ω).take((usedVariables.statements.variableIndices.map(_ + 1) :+ 0).max).map { p =>
      val arity = usedVariables.statements.find(_.index == p.index).map(_.arity).getOrElse(0)
      VariableDefinition(p.name, arity, Nil)
    }
    val termVariableDefinitions = Seq(a, b, c, d).take((usedVariables.terms.variableIndices.map(_ + 1) :+ 0).max).map { p =>
      val arity = usedVariables.terms.find(_.index == p.index).map(_.arity).getOrElse(0)
      VariableDefinition(p.name, arity, Nil)
    }
    VariableDefinitions(statementVariableDefinitions, termVariableDefinitions)
  }
  def getVariableDefinitions(statements: Seq[(StatementVariablePlaceholder, Int)], terms: Seq[(TermVariablePlaceholder, Int)]): VariableDefinitions = {
    VariableDefinitions(
      statements.map { case (v, arity) => VariableDefinition(v.name, arity, Nil) },
      terms.map { case (v, arity) => VariableDefinition(v.name, arity, Nil) })
  }
}

trait TestExpressionDefinitions extends TestVariableDefinitions {

  private def connective(
    symbol: String,
    size: Int,
    definingStatement: Option[Statement]
  ): StatementDefinitionEntry = {
    val componentTypes = Seq[ComponentType](φ, ψ, χ).take(size)
    StatementDefinitionEntry(
      symbol,
      Nil,
      componentTypes,
      None,
      Format.default(size),
      definingStatement,
      None,
      Nil)
  }
  private def predicate(
    symbol: String,
    size: Int,
    definingStatement: Option[Statement]
  ): StatementDefinitionEntry = {
    val componentTypes = Seq[ComponentType](a, b, c).take(size)
    StatementDefinitionEntry(
      symbol,
      Nil,
      componentTypes,
      None,
      Format.default(size),
      definingStatement,
      None,
      Nil)
  }
  private def quantifier(
    symbol: String,
    definingStatement: Option[Statement]
  ): StatementDefinitionEntry = {
    StatementDefinitionEntry(
      symbol,
      Seq("x"),
      Seq(StatementComponent("φ", Seq(ExpressionDefinition.ComponentArgument("x", 0)))),
      None,
      Format.Explicit(s"(%0%1)%2", s"(${symbol}x)φ", 3, requiresBrackets = false, requiresComponentBrackets = true),
      definingStatement,
      None,
      Nil)
  }

  def simpleTermDefinition(
    symbol: String,
    components: Seq[ComponentType],
    format: Format.Basic
  ): TermDefinitionEntry = simpleTermDefinition(symbol, components, format, Nil, BlankDefinition)
  def simpleTermDefinition(
    symbol: String,
    components: Seq[ComponentType],
    format: Format.Basic,
    premises: Seq[Statement],
    definition: Statement
  ): TermDefinitionEntry = simpleTermDefinition(symbol, Nil, components, format, premises, definition)
  def simpleTermDefinition(
    symbol: String,
    boundVariables: Seq[String],
    components: Seq[ComponentType],
    format: Format.Basic,
    premises: Seq[Statement],
    definition: Statement
  ): TermDefinitionEntry = TermDefinitionEntry(symbol, boundVariables, components, None, None, format, premises, definition, None, Nil, Nil)

  val Implication = connective("→", 2, None).copy(attributes = Seq("deduction"))
  val DeductionDefinition = definitions.DeductionDefinition(Implication)
  val Negation = connective("¬", 1, None)
  val Conjunction = connective("∧", 2, Some(Negation(Implication(φ, Negation(ψ))))).copy(attributes = Seq("conjunction"))
  val ConjunctionDefinition = definitions.ConjunctionDefinition(Conjunction)
  val Disjunction = connective("∨", 2, Some(Implication(Negation(φ), ψ)))
  val Equivalence = connective("↔", 2, Some(Conjunction(Implication(φ, ψ), Implication(ψ, φ))))

  val ForAllDefinition = quantifier("∀", None).copy(attributes = Seq("generalization"))
  def ForAll(name: String)(expression: Statement) = ForAllDefinition.bind(name)(expression)
  def ForAllIn(name: String, term: Term)(expression: Statement) = ForAll(name)(Implication(ElementOf($, term), expression))
  val GeneralizationDefinition = definitions.GeneralizationDefinition(ForAllDefinition)
  val ExistsDefinition = quantifier("∃", Some(Negation(ForAll("x")(Negation(φ($))))))
  def Exists(name: String)(expression: Statement) = ExistsDefinition.bind(name)(expression)
  def ExistsIn(name: String, term: Term)(expression: Statement) = Exists(name)(Conjunction(ElementOf($, term), expression))
  val Equals = predicate("=", 2, None).copy(attributes = Seq("equality"))
  val ExistsUniqueDefinition = quantifier("∃!", Some(Exists("y")(ForAll("x")(Equivalence(φ($), Equals($, $.^))))))
  def ExistsUnique(name: String)(expression: Statement) = ExistsUniqueDefinition.bind(name)(expression)
  val UniqueExistenceDefinition = definitions.UniqueExistenceDefinition(ExistsUniqueDefinition)
  val ElementOf = predicate("∈", 2, None)
  val Subset = predicate("⊆", 2, Some(ForAll("x")(Implication(ElementOf($, a), ElementOf($, b)))))


  val BlankDefinition = DefinedStatement(Nil, connective("false", 0, None))(Nil)
  val EmptySetDefinition = simpleTermDefinition("∅", Nil, Format.default(0), Nil, ForAll("x")(Negation(ElementOf($, $.^))))
  val EmptySet = DefinedTerm(Nil, EmptySetDefinition)(Nil)

  val PowerSet = simpleTermDefinition("powerSet", Seq(a), Format.Explicit("𝒫%1", "𝒫a", 2, false, true), Nil, ForAll("y")(Equivalence(ElementOf($, $.^), Subset($, a)))).copy(explicitName = Some("Power Set"))

  val Singleton = simpleTermDefinition("singleton", Seq(a), Format.Explicit("{%1}", "{a}", 2, false, false)).copy(explicitName = Some("Singleton"))

  val Pair = simpleTermDefinition("pair", Seq(a, b), Format.Explicit("{%1, %2}", "{a, b}", 3, false, false)).copy(explicitName = Some("Unordered Pair"))
  val Product = simpleTermDefinition("product", Seq(a, b), Format.Explicit("%1 × %2", "a × b", 3, true, true)).copy(explicitName = Some("Cartesian Product"))
  val First = simpleTermDefinition("first", Seq(a), Format.Explicit("%1_0", "a_0", 2, requiresBrackets = false, requiresComponentBrackets = true))
  val Second = simpleTermDefinition("second", Seq(a), Format.Explicit("%1_1", "a_10", 2, requiresBrackets = false, requiresComponentBrackets = true))

  val Union = simpleTermDefinition("union", Seq(a), Format.Explicit("⋃a", Seq("union", "a"), false, true))

  val Comprehension = simpleTermDefinition(
    "comprehension",
    Seq("x"),
    Seq(a, φ.toComponent(ComponentArgument("x", 0))),
    Format.Explicit("{ x ∈ a | φ }", Seq("comprehension", "x", "a", "φ"), false, true),
    Nil,
    ForAll("a")(Equivalence(ElementOf($, $.^), Conjunction(ElementOf($, a), φ($)))))

  val PairSet = TypeDefinition("pairSet", "a", None, None, ForAllIn("x", a)(Exists("a")(Exists("b")(Equals($.^^, Pair($.^, $))))))
  val Domain = simpleTermDefinition("domain", Seq(a), Format.Explicit("%0(%1)", "domain(a)", 2, false, false), Nil, Equals($, Comprehension.bind("x")(Union(Union(a)), Exists("b")(ElementOf(Pair($.^, $), a)))))
  val Range = simpleTermDefinition("range", Seq(a), Format.Explicit("%0(%1)", "range(a)", 2, false, false), Nil, Equals($, Comprehension.bind("x")(Union(Union(a)), Exists("a")(ElementOf(Pair($, $.^), a)))))
  val Relation = TypeDefinition("relation", "R", Some(Qualifier(Seq("A"), Format.Explicit("on A", Seq("A"), true, true))), None, Subset(R, Product(A, A)))
  val Function = TypeDefinition("function", "f", None, None, Conjunction(PairSet(f), ForAllIn("a", Domain(f))(ExistsUnique("b")(ElementOf(Pair($.^, $), f)))))
  val FunctionFrom = TypeQualifierDefinition("from", Function, Qualifier(Seq("A", "B"), Format.Explicit("from A to B", Seq("A", "B"), true, true)), None, Conjunction(Equals(Domain(f), A), Subset(Range(f), B)), ConjunctionDefinition)
  val Apply = simpleTermDefinition("apply", Seq(a, b), Format.Explicit("%1(%2)", "a(b)", 3, requiresBrackets = false, requiresComponentBrackets = true))
  def Apply2(f: Term, a: Term, b: Term): Term = Apply(f, Pair(a, b))

  val UnaryOperation = TypeDefinition("unaryOperation", "f", None, None, Conjunction(Function(f), FunctionFrom(f, Domain(f), Domain(f))))
  val UnaryOperationOn = TypeQualifierDefinition("on", UnaryOperation, Qualifier(Seq("A"), Format.Explicit("on A", Seq("A"), true, true)), None, Equals(Domain(f), A), ConjunctionDefinition)
  val BaseSet = simpleTermDefinition("baseSet", Seq(a), Format.Explicit("%0(%1)", "baseSet(a)", 2, false, false), Nil, Equals($, Domain(Domain(a))))
  val BinaryOperation = TypeDefinition("binaryOperation", SimpleVariableDefinition("f", Seq("infix-function")), None, None, Conjunction(Function(f), FunctionFrom(f, Product(BaseSet(f), BaseSet(f)), BaseSet(f))))
  val BinaryOperationOn = TypeQualifierDefinition("on", BinaryOperation, Qualifier(Seq("A"), Format.Explicit("on A", Seq("A"), true, true)), None, Equals(BaseSet(f), A), ConjunctionDefinition)
  val Commutative = PropertyDefinitionOnType("commutative", ParentTypeConditions(BinaryOperation, None, None, None, ConjunctionDefinition), None, ForAllIn("a", BaseSet(f))(ForAllIn("b", BaseSet(f))(Equals(Apply(f, Pair($.^, $)), Apply(f, Pair($, $.^))))))
  val Distributive = TypeRelationDefinition(
    "distributes",
    BinaryOperation,
    BinaryOperation,
    "∘",
    "∗",
    "distributes over",
    Some("distributivity"),
    Conjunction(
      Equals(BaseSet(a), BaseSet(b)),
      ForAllIn("a", BaseSet(a))(ForAllIn("b", BaseSet(a))(ForAllIn("c", BaseSet(a))(
        Conjunction(
          Equals(Apply(a, Pair($.^^, Apply(b, Pair($.^, $)))), Apply(b, Pair(Apply(a, Pair($.^^, $.^)), Apply(a, Pair($.^^, $))))),
          Equals(Apply(a, Pair(Apply(b, Pair($.^^, $.^)), $)), Apply(b, Pair(Apply(a, Pair($.^^, $)), Apply(a, Pair($.^, $)))))))))),
    ConjunctionDefinition)

  val NaturalsDefinition = simpleTermDefinition("ℕ", Nil, Format.default(0))
  val Naturals = DefinedTerm(Nil, NaturalsDefinition)(Nil)
  val Successor = simpleTermDefinition("successor", Seq(a), Format.Explicit("a^+", Seq("successor", "a"), requiresBrackets = false, requiresComponentBrackets = true))
  val ZeroDefinition = simpleTermDefinition("0", Nil, Format.default(Nil, Nil)).withDisambiguator(Some("ℕ"))
  val Zero = DefinedTerm(Nil, ZeroDefinition)(Nil)
  val OneDefinition = simpleTermDefinition("1", Nil, Format.default(Nil, Nil)).withDisambiguator(Some("ℕ"))
  val One = DefinedTerm(Nil, OneDefinition)(Nil)
  val AdditionDefinition = simpleTermDefinition("+", Nil, Format.default(0)).withDisambiguator(Some("ℕ"))
  val Addition = DefinedTerm(Nil, AdditionDefinition)(Nil)
  val MultiplicationDefinition = simpleTermDefinition("×", Nil, Format.default(0), Nil, BlankDefinition).withDisambiguator(Some("ℕ"))
  val Multiplication = DefinedTerm(Nil, MultiplicationDefinition)(Nil)
  val LessThanDefinition = TermDefinitionEntry(
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

  def add(l: Term, r: Term) = Apply(Addition, Pair(l, r))
  def multiply(l: Term, r: Term) = Apply(Multiplication, Pair(l, r))

  val IntegersDefinition = simpleTermDefinition("ℤ", Nil, Format.default(0))
  val Integers = IntegersDefinition()
  val IntegerEmbeddingDefinition = simpleTermDefinition("⍳", Nil, Format.default(0), Nil, Conjunction(Function($), FunctionFrom($, Naturals, Integers)))
  val IntegerEmbedding = IntegerEmbeddingDefinition()

  val IntegerAdditionDefinition = simpleTermDefinition("+", Nil, Format.default(0), Nil, Conjunction(BinaryOperation($), BinaryOperationOn($, Integers))).copy(disambiguator = Some("ℤ"))
  val IntegerAddition = DefinedTerm(Nil, IntegerAdditionDefinition)(Nil)
  val IntegerNegation = simpleTermDefinition("-", Seq(a), Format.default(1), Seq(ElementOf(a, Integers)), Conjunction(ElementOf($, Integers), Conjunction(Equals(addZ(a, $), toZ(Zero)), Equals(addZ($, a), toZ(Zero)))))
  val IntegerMultiplicationDefinition = simpleTermDefinition("×", Nil, Format.default(0), Nil, Conjunction(BinaryOperation($), BinaryOperationOn($, Integers))).copy(disambiguator = Some("ℤ"))
  val IntegerMultiplication = DefinedTerm(Nil, IntegerMultiplicationDefinition)(Nil)

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

  def toZ(t: Term) = Apply(IntegerEmbedding, t)
  def addZ(l: Term, r: Term) = Apply(IntegerAddition, Pair(l, r))
  def mulZ(l: Term, r: Term) = Apply(IntegerMultiplication, Pair(l, r))
}

trait TestInferenceDefinitions extends TestExpressionDefinitions {
  def createInference(name: String, premises: Seq[Statement], conclusion: Statement): Axiom = {
    Axiom(name, getVariableDefinitionsFromStatements(premises :+ conclusion), premises, conclusion)
  }

  val specification = createInference("Specification", Seq(ForAll("x")(φ($))), φ(a))
  val existence = createInference("Existence", Seq(φ(a)), Exists("x")(φ($)))
  val modusPonens = createInference("Modus Ponens", Seq(Implication(φ, ψ), φ), ψ)
  val modusTollens = createInference("Modus Tollens", Seq(Implication(φ, ψ), Negation(ψ)), Negation(φ))
  val implicationIsTransitive = createInference("Implication Is Transitive", Seq(Implication(φ, ψ), Implication(ψ, χ)), Implication(φ, χ))

  val addDoubleNegation = createInference("Add Double Negation", Seq(φ), Negation(Negation(φ)))
  val removeDoubleNegation = createInference("Remove Double Negation", Seq(Negation(Negation(φ))), φ)

  val extractLeftConjunct = createInference("Extract Left Conjunct", Seq(Conjunction(φ, ψ)), φ)
  val extractRightConjunct = createInference("Extract Right Conjunct", Seq(Conjunction(φ, ψ)), ψ)
  val combineConjunction = createInference("Combine Conjunction", Seq(φ, ψ), Conjunction(φ, ψ))

  val addLeftDisjunct = createInference("Add Left Disjunct", Seq(φ), Disjunction(ψ, φ))
  val addRightDisjunct = createInference("Add Right Disjunct", Seq(φ), Disjunction(φ, ψ))

  val reverseEquivalence = createInference("Reverse Equivalence", Seq(Equivalence(φ, ψ)), Equivalence(ψ, φ))
  val equivalenceIsTransitive = createInference("Equivalence Is Transitive", Seq(Equivalence(φ, ψ), Equivalence(ψ, χ)), Equivalence(φ, χ))
  val forwardImplicationFromEquivalence = createInference("Forward Implication from Equivalence", Seq(Equivalence(φ, ψ)), Implication(φ, ψ))
  val reverseImplicationFromEquivalence = createInference("Reverse Implication from Equivalence", Seq(Equivalence(φ, ψ)), Implication(ψ, φ))

  val distributeImplicationOverEquivalence = createInference("Distribute Implication over Equivalence", Seq(Implication(φ, Equivalence(ψ, χ))), Equivalence(Implication(φ, ψ), Implication(φ, χ)))
  val distributeUniversalQuantifierOverEquivalence = createInference("Distribute Universal Quantifier over Equivalence", Seq(ForAll("x")(Equivalence(φ($), ψ($)))), Equivalence(ForAll("x")(φ($)), ForAll("x")(ψ($))))

  val reverseEquality = createInference("Reverse Equality", Seq(Equals(a, b)), Equals(b, a))
  val reverseNegatedEquality = createInference("Reverse Negated Equality", Seq(Negation(Equals(a, b))), Negation(Equals(b, a)))
  val equalityIsReflexive = createInference("Equality Is Reflexive", Nil, Equals(a, a))
  val equalityIsTransitive = createInference("Equality Is Transitive", Seq(Equals(a, b), Equals(b, c)), Equals(a, c))
  val substitutionOfEquals = createInference("Substitution of Equals", Seq(Equals(a, b), φ(a)), φ(b))
  val substitutionOfEqualsIntoFunction = createInference("Substitution of Equals Into Function", Seq(Equals(a, b)), Equals(c(a), c(b)))
  val equivalenceOfSubstitutedEquals = createInference("Equivalence of Substituted Equals", Seq(Equals(a, b)), Equivalence(φ(a), φ(b)))

  val membershipConditionForSingleton = createInference("Membership Condition for Singleton", Nil, ForAll("x")(Equivalence(ElementOf($, Singleton(a)), Equals($, a))))
  val elementOfCartesianProductFromCoordinates = createInference("Element of Cartesian Product from Coordinates", Seq(ElementOf(a, Product(b, c))), Equals(a, Pair(First(a), Second(a))))
  val firstCoordinateOfOrderedPairInCartesianProduct = createInference("First Coordinate of Ordered Pair in Cartesian Product", Seq(ElementOf(Pair(a, b), Product(c, d))), ElementOf(a, c))
  val firstCoordinateOfElementOfCartesianProduct = createInference("First Coordinate of Element of Cartesian Product", Seq(ElementOf(a, Product(b, c))), ElementOf(First(a), b))
  val secondCoordinateOfElementOfCartesianProduct = createInference("Second Coordinate of Element of Cartesian Product", Seq(ElementOf(a, Product(b, c))), ElementOf(Second(a), c))
  val orderedPairIsElementOfCartesianProduct = createInference("Ordered Pair Is Element of Cartesian Product", Seq(ElementOf(a, b), ElementOf(c, d)), ElementOf(Pair(a, c), Product(b, d)))
  val firstElement = createInference("First Element", Nil, Equals(First(Pair(a, b)), a))

  val functionApplicationIsElementOfRange = createInference("Function Application Is Element of Range", Seq(Function(f), ElementOf(A, Domain(f))), ElementOf(Apply(f, A), Range(f)))

  val zeroIsANaturalNumber = createInference("0 Is a Natural Number", Nil, ElementOf(Zero, Naturals))
  val oneIsANaturalNumber = createInference("1 Is a Natural Number", Nil, ElementOf(One, Naturals))
  val successorOfNaturalIsNatural = createInference("A Successor of a Natural Number Is a Natural Number", Seq(ElementOf(a, Naturals)), ElementOf(Successor(a), Naturals))
  val additionIsClosed = createInference("Addition Is Closed", Seq(ElementOf(a, Naturals), ElementOf(b, Naturals)), ElementOf(add(a, b), Naturals))
  val additionIsAssociative = createInference("Addition Is Associative", Nil, Equals(add(a, add(b, c)), add(add(a, b), c)))
  val additionIsCommutative = createInference("Addition Is Commutative", Nil, Equals(add(a, b), add(b, a)))
  val zeroIsLeftIdentityForAddition = createInference("Adding Zero Is Same", Nil, Equals(a, add(Zero, a)))
  val zeroIsRightIdentityForAddition = createInference("Adding Zero Is Same", Nil, Equals(a, add(a, Zero)))
  val multiplicationIsAssociative = createInference("Multiplication Is Associative", Nil, Equals(multiply(a, multiply(b, c)), multiply(multiply(a, b), c)))
  val multiplicationIsCommutative = createInference("Multiplication Is Commutative", Nil, Equals(multiply(a, b), multiply(b, a)))
  val multiplicationDistributesOverAddition = createInference("Multiplication Distributes over Addition", Nil, Conjunction(Equals(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c))), Equals(multiply(add(a, b), c), add(multiply(a, c), multiply(b, c)))))
  val oneIsIdentityForMultiplication = createInference("Identity for Multiplication", Nil, Conjunction(Equals(multiply(a, One), a), Equals(multiply(One, a), a)))
  val zeroIsAbsorberForMultiplication = createInference("Absorber for Multiplication", Nil, Conjunction(Equals(multiply(a, Zero), Zero), Equals(multiply(Zero, a), Zero)))
  val orderingIsTransitive = createInference("Natural Ordering Is Transitive", Seq(lessThan(a, b), lessThan(b, c)), lessThan(a, c))

  val integerAdditionIsClosed = createInference("Integer Addition Is Closed", Seq(ElementOf(a, Integers), ElementOf(b, Integers)), ElementOf(addZ(a, b), Integers))
  val integerAdditionIsAssociative = createInference("Integer Addition Is Associative", Seq(ElementOf(a, Integers), ElementOf(b, Integers), ElementOf(c, Integers)), Equals(addZ(a, addZ(b, c)), addZ(addZ(a, b), c)))
  val integerAdditionIsCommutative = createInference("Integer Addition Is Commutative", Nil, Commutative(IntegerAddition))
  val identityForIntegerAddition = createInference("Identity for Integer Addition", Seq(ElementOf(a, Integers)), Conjunction(Equals(addZ(a, toZ(Zero)), a), Equals(addZ(toZ(Zero), a), a)))
  val integerMultiplicationIsClosed = createInference("Integer Multiplication Is Closed", Seq(ElementOf(a, Integers), ElementOf(b, Integers)), ElementOf(mulZ(a, b), Integers))
  val integerMultiplicationIsAssociative = createInference("Integer Multiplication Is Associative", Seq(ElementOf(a, Integers), ElementOf(b, Integers), ElementOf(c, Integers)), Equals(mulZ(a, mulZ(b, c)), mulZ(mulZ(a, b), c)))
  val integerMultiplicationIsCommutative = createInference("Integer Multiplication Is Commutative", Nil, Commutative(IntegerMultiplication))
  val integerMultiplicationDistributesOverAddition = createInference("Integer Multiplication Distributes over Addition", Nil, Distributive(IntegerMultiplication, IntegerAddition))

  val identityForIntegerMultiplication = createInference("Identity for Integer Multiplication", Seq(ElementOf(a, Integers)), Conjunction(Equals(mulZ(a, toZ(One)), a), Equals(mulZ(toZ(One), a), a)))
  val absorberForIntegerMultiplication = createInference("Absorber for Integer Multiplication", Seq(ElementOf(a, Integers)), Conjunction(Equals(mulZ(a, toZ(Zero)), toZ(Zero)), Equals(mulZ(toZ(Zero), a), toZ(Zero))))
  val negationOfIntegerMultiplication = createInference("Negation of Integer Multipliciation", Seq(ElementOf(a, Integers), ElementOf(b, Integers)), Conjunction(Equals(mulZ(a, IntegerNegation(b)), IntegerNegation(mulZ(a, b))), Equals(mulZ(IntegerNegation(a), b), IntegerNegation(mulZ(a, b)))))
}

trait StepHelpers extends TestVariableDefinitions with TestExpressionDefinitions {

  def assertion(inference: Inference, statements: Seq[Statement], terms: Seq[Term]): SubstitutionContext => Step.Assertion = { substitutionContext =>
    Step.Assertion.forInference(inference, Substitutions(statements, terms))(substitutionContext).get
  }
  def generalization(variableName: String, steps: SubstitutionContext => Seq[Step]): SubstitutionContext => Step.Generalization = sc => Step.Generalization(variableName, steps(SubstitutionContext.withExtraParameter(sc)), GeneralizationDefinition)
  def deduction(antecedent: Statement, steps: SubstitutionContext => Seq[Step]): SubstitutionContext => Step.Deduction = sc => Step.Deduction(antecedent, steps(sc), DeductionDefinition)
  def target(statement: Statement): SubstitutionContext => Step.Target = _ => Step.Target(statement)
  def elided(inference: Inference, steps: SubstitutionContext => Seq[Step]): SubstitutionContext => Step.Elided = sc => Step.Elided(steps(sc), Some(inference.summary), None)
  def elided(description: String, steps: SubstitutionContext => Seq[Step]): SubstitutionContext => Step.Elided = sc => Step.Elided(steps(sc), None, Some(description))
  def existingStatementExtraction(steps: SubstitutionContext => Seq[Step]): SubstitutionContext => Step.ExistingStatementExtraction = sc => Step.ExistingStatementExtraction(steps(sc))

  def fillerSteps(number: Int): SubstitutionContext => Seq[Step] = (0 until number).map(i => target(ForAll("x")(Equals($, TermVariable(i)))))

  implicit class StepsConstructor(createSteps: SubstitutionContext => Seq[Step]) {
    def :+(other: SubstitutionContext => Step): SubstitutionContext => Seq[Step] = { sc =>
      createSteps(sc) :+ other(sc)
    }
  }
  implicit def seqConstructorToConstructorSeq(seq: Seq[SubstitutionContext => Step]): SubstitutionContext => Seq[Step] = { sc =>
    seq.map(_(sc))
  }
}

object TestDefinitions extends TestVariableDefinitions with TestExpressionDefinitions with TestInferenceDefinitions with StepHelpers with MockitoStubs {
  import org.specs2.matcher.Matchers._
  import org.specs2.matcher.MustExpectations._
  val defaultEntryContext: EntryContext = EntryContext(
    Seq(
      Implication, Negation, Conjunction, Disjunction, Equivalence,
      ForAllDefinition, ExistsDefinition, ExistsUniqueDefinition,
      ElementOf, Equals, Subset) ++
    Seq(
      EmptySetDefinition, PowerSet, Singleton, Pair, Product, First, Second, Union, Comprehension,
      PairSet, Domain, Range, Relation, Function, FunctionFrom,
      UnaryOperation, UnaryOperationOn, BaseSet, BinaryOperation, BinaryOperationOn, Commutative, Distributive,
      NaturalsDefinition, Successor, ZeroDefinition, OneDefinition, AdditionDefinition, MultiplicationDefinition, Apply, LessThanDefinition,
      IntegersDefinition, IntegerEmbeddingDefinition, IntegerAdditionDefinition, IntegerNegation, IntegerMultiplicationDefinition) ++
    Seq(
      specification, existence, modusPonens, modusTollens, implicationIsTransitive,
      addDoubleNegation, removeDoubleNegation,
      extractLeftConjunct, extractRightConjunct, combineConjunction,
      addLeftDisjunct, addRightDisjunct,
      reverseEquivalence, equivalenceIsTransitive, forwardImplicationFromEquivalence, reverseImplicationFromEquivalence,
      distributeImplicationOverEquivalence, distributeUniversalQuantifierOverEquivalence,
      reverseEquality, reverseNegatedEquality, equalityIsReflexive, equalityIsTransitive, substitutionOfEquals, substitutionOfEqualsIntoFunction, equivalenceOfSubstitutedEquals,
      membershipConditionForSingleton, elementOfCartesianProductFromCoordinates, firstCoordinateOfOrderedPairInCartesianProduct, firstCoordinateOfElementOfCartesianProduct, secondCoordinateOfElementOfCartesianProduct, orderedPairIsElementOfCartesianProduct, firstElement,
      functionApplicationIsElementOfRange,
      zeroIsANaturalNumber, oneIsANaturalNumber, successorOfNaturalIsNatural, additionIsClosed, additionIsAssociative, additionIsCommutative, zeroIsLeftIdentityForAddition, zeroIsRightIdentityForAddition,
      multiplicationIsAssociative, multiplicationIsCommutative, multiplicationDistributesOverAddition, oneIsIdentityForMultiplication, zeroIsAbsorberForMultiplication,
      orderingIsTransitive,
      integerAdditionIsClosed, integerAdditionIsAssociative, integerAdditionIsCommutative, identityForIntegerAddition,
      integerMultiplicationIsClosed, integerMultiplicationIsAssociative, integerMultiplicationIsCommutative, integerMultiplicationDistributesOverAddition, identityForIntegerMultiplication, absorberForIntegerMultiplication, negationOfIntegerMultiplication) ++
    Seq(InfixRelationShorthand, NotElementOfShorthand, NotEqualShorthand))

  implicit class ParserOps[T](parser: Parser[T]) {
    def parseAndDiscard(text: String): T = {
      parser.parseFromString(text, "test")
    }
  }
  implicit def entryContextToProvingContext(implicit entryContext: EntryContext): ProvingContext = ProvingContext(entryContext, new Definitions(entryContext))
  implicit def entryContextToEntryParsingContext(entryContext: EntryContext): EntryParsingContext = EntryParsingContext(entryContext, mock[ProofFileReader])
  implicit def entryContextAndStepContextToStepProvingContext(implicit entryContext: EntryContext, stepContext: StepContext): StepProvingContext = {
    StepProvingContext(stepContext, entryContextToProvingContext(entryContext))
  }

  def createTheoremWithContext(theorem: Theorem)(implicit entryContext: EntryContext): TheoremWithContext = {
    val theoremWithContext = mock[TheoremWithContext]
    theoremWithContext.entry returns theorem
    theoremWithContext.theorem returns theorem
    theoremWithContext.entryContext returns entryContext
    when(theoremWithContext.proofsWithContext).thenCallRealMethod()
    theoremWithContext.provingContext returns entryContextToProvingContext
    theoremWithContext
  }

  def beValidTheorem(implicit entryContext: EntryContext): Matcher[Theorem] = (theorem: Theorem) => {
    val recalculatedTheorem = RecalculateReferences(createTheoremWithContext(theorem))._1
    val serializedTheorem = recalculatedTheorem.serializedLines.mkString("\n").stripPrefix("theorem ")
    val serializedProofs = recalculatedTheorem.proofs.map(_.serialized)
    val proofFileReader = mock[ProofFileReader]
    proofFileReader.getSerializedProofs(recalculatedTheorem.title) returns serializedProofs
    val entryParsingContext = EntryParsingContext(entryContext, proofFileReader)
    val parsedTheorem = Theorem.parser(entryParsingContext).parseFromString(serializedTheorem, "Theorem")
    parsedTheorem must beTypedEqualTo(theorem)
    parsedTheorem.isComplete(new Definitions(entryContext)) must beTrue
  }

  def beStepsThatMakeValidTheorem(premises: Seq[Statement], conclusion: Statement)(implicit entryContext: EntryContext, variableDefinitions: VariableDefinitions): Matcher[Seq[Step]] = {
    beValidTheorem(entryContext) ^^ { steps: Seq[Step] => {
      Theorem(
        "Test Theorem",
        variableDefinitions,
        premises,
        conclusion,
        Seq(Theorem.Proof(steps)))
    }}
  }

  def beStepThatMakesValidTheorem(premises: Seq[Statement], conclusion: Statement, depth: Int = 0)(implicit entryContext: EntryContext, variableDefinitions: VariableDefinitions): Matcher[Step] = {
    beStepsThatMakeValidTheorem(premises, conclusion, depth) ^^ { step: Step => Seq(step) }
  }

  def beStepsThatMakeValidTheorem(premises: Seq[Statement], conclusion: Statement, depth: Int)(implicit entryContext: EntryContext, variableDefinitions: VariableDefinitions): Matcher[Seq[Step]] = {
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
          Substitutions(Seq(statement.specify(Seq(FunctionParameter(0, depth - parameterDepth)), 0, 0).get), Seq($)))
      }
      beStepsThatMakeValidTheorem(premises.map(generalizeToDepth(_, depth)), generalizeToDepth(conclusion, depth)) ^^ { steps: Seq[Step] =>
        (0 until depth).foldLeft(steps) { case (steps, i) => Seq(Step.Generalization(s"x_$i", premises.map(p => specificationStep(generalizeToDepth(p, i), i)) ++ steps, GeneralizationDefinition))}
      }
    }
  }
}
