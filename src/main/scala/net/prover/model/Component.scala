package net.prover.model

import shapeless.{::, HList, HNil}

trait Component[T <: Component[T]] {
  def variables: Variables
  def freeVariables: Seq[TermVariable]
  def calculateSubstitutions(other: T): Option[Substitutions]
  def applySubstitutions(substitutions: Substitutions): T with Component[T]
  def substituteFreeVariable(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable
  ): T
  def attemptSimplification(other: T): Option[DistinctVariables]
  def makeSimplifications(distinctVariables: DistinctVariables): T
  def html: String
  def safeHtml: String = html
  override def toString: String = html
}

trait ComponentType[T <: Component[T]] {
  def parse(line: PartialLine, context: Context): (T, PartialLine)
}

trait ComponentTypeList{
  type Components <: HList
  def length: Int
  def parse(line: PartialLine, context: Context): (Components, PartialLine)
  def defaults(currentStatement: Int = 1, currentTerm: Int = 1): Components
  def format(formatString: String, components: Components): String
  def getVariables(components: Components): Variables
  def getFreeVariables(components: Components): Seq[TermVariable]
  def calculateSubstitutions(components: Components, otherComponents: Components): Option[Substitutions]
  def applySubstitutions(
    components: Components,
    substitutions: Substitutions
  ): Components
  def substituteFreeVariable(
    components: Components,
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable
  ): Components
  def attemptSimplification(
    components: Components,
    otherComponents: Components
  ): Option[DistinctVariables]
  def makeSimplifications(
    components: Components,
    distinctVariables: DistinctVariables
  ): Components
  def containsTerms(components: Components): Boolean

  def termSpecification(symbol: String, format: String, requiresBrackets: Boolean): TermSpecification[Components] = {
    TermSpecification[Components](symbol, this, format, requiresBrackets)
  }
}

object ComponentTypeList {
  type Aux[T <: HList] = ComponentTypeList {type Components = T}

  val empty: ComponentTypeList.Aux[HNil] = new ComponentTypeList {
    type Components = HNil
    val length: Int = 0
    override def parse(line: PartialLine, context: Context): (HNil, PartialLine) = (HNil, line)
    override def defaults(currentStatement: Int, currentTerm: Int) = HNil
    override def format(formatString: String, components: HNil): String = formatString
    override def getVariables(components: Components): Variables = Variables.empty
    override def getFreeVariables(components: Components): Seq[TermVariable] = Nil
    override def calculateSubstitutions(
      components: Components,
      otherComponents: Components
    ): Option[Substitutions] = Some(Substitutions.empty)
    override def applySubstitutions(
      components: Components,
      substitutions: Substitutions
    ): Components = HNil
    override def substituteFreeVariable(
      components: Components,
      termToReplaceWith: Term,
      termToBeReplaced: TermVariable
    ): Components = HNil
    override def attemptSimplification(
      components: Components,
      otherComponents: Components
    ) = Some(DistinctVariables.empty)
    override def makeSimplifications(
      components: Components,
      distinctVariables: DistinctVariables
    ): Components = HNil
    override def containsTerms(
      components: Components
    ) = false
  }

  def withTerm(inner: ComponentTypeList): ComponentTypeList.Aux[Term :: inner.Components] = new ComponentTypeList {
    type Components = Term :: inner.Components
    val length: Int = inner.length + 1
    override def parse(line: PartialLine, context: Context): (Term :: inner.Components, PartialLine) = {
      val (term, lineAfterTerm) = Term.parse(line, context)
      val (otherComponents, remainingLine) = inner.parse(lineAfterTerm, context)
      (::(term, otherComponents), remainingLine)
    }

    override def defaults(currentStatement: Int, currentTerm: Int): Term :: inner.Components = {
      val innerDefaults = inner.defaults(currentStatement, currentTerm + 1)
      ::(TermVariable(currentTerm), innerDefaults)
    }

    override def format(
      formatString: String,
      components: Term :: inner.Components
    ): String = {
      val updatedFormatString = formatString.replaceFirst("\\{\\}", components.head.safeHtml)
      inner.format(updatedFormatString, components.tail)
    }

    override def getVariables(components: Components): Variables = {
      components.head.variables ++ inner.getVariables(components.tail)
    }

    override def getFreeVariables(components: Components): Seq[TermVariable] = {
      components.head.freeVariables ++ inner.getFreeVariables(components.tail)
    }

    override def calculateSubstitutions(
      components: Components,
      otherComponents: Components
    ): Option[Substitutions] = {
      Substitutions.mergeAttempts(Seq(
        components.head.calculateSubstitutions(otherComponents.head),
        inner.calculateSubstitutions(components.tail, otherComponents.tail)))
    }

    override def applySubstitutions(
      components: Components,
      substitutions: Substitutions
    ): Components = {
      components.head.applySubstitutions(substitutions) :: inner.applySubstitutions(components.tail, substitutions)
    }

    override def substituteFreeVariable(
      components: Components,
      termToReplaceWith: Term,
      termToBeReplaced: TermVariable
    ): Components = {
      components.head.substituteFreeVariable(termToReplaceWith, termToBeReplaced) ::
        inner.substituteFreeVariable(components.tail, termToReplaceWith, termToBeReplaced)
    }

    override def attemptSimplification(
      components: Components,
      otherComponents: Components
    ): Option[DistinctVariables] = {
      for {
        head <- components.head.attemptSimplification(otherComponents.head)
        tail <- inner.attemptSimplification(components.tail, otherComponents.tail)
      } yield head ++ tail
    }

    override def makeSimplifications(
      components: Components,
      distinctVariables: DistinctVariables
    ): Components = {
      components.head.makeSimplifications(distinctVariables) ::
        inner.makeSimplifications(components.tail, distinctVariables)
    }

    override def containsTerms(
      components: Components
    ) = true
  }

  def withStatement(inner: ComponentTypeList): ComponentTypeList.Aux[Statement :: inner.Components] = new ComponentTypeList {
    type Components = Statement :: inner.Components
    val length: Int = inner.length + 1
    override def parse(line: PartialLine, context: Context): (Statement :: inner.Components, PartialLine) = {
      val (statement, lineAfterTerm) = Statement.parse(line, context)
      val (otherComponents, remainingLine) = inner.parse(lineAfterTerm, context)
      (::(statement, otherComponents), remainingLine)
    }

    override def defaults(currentStatement: Int, currentTerm: Int): Statement :: inner.Components = {
      val innerDefaults = inner.defaults(currentStatement + 1, currentTerm)
      ::(StatementVariable(currentStatement), innerDefaults)
    }

    override def format(
      formatString: String,
      components: Statement :: inner.Components
    ): String = {
      val updatedFormatString = formatString.replaceFirst("\\{\\}", components.head.safeHtml)
      inner.format(updatedFormatString, components.tail)
    }

    override def getVariables(components: Components): Variables = {
      components.head.variables ++ inner.getVariables(components.tail)
    }

    override def getFreeVariables(components: Components): Seq[TermVariable] = {
      components.head.freeVariables ++ inner.getFreeVariables(components.tail)
    }

    override def calculateSubstitutions(
      components: Components,
      otherComponents: Components
    ): Option[Substitutions] = {
      Substitutions.mergeAttempts(Seq(
        components.head.calculateSubstitutions(otherComponents.head),
        inner.calculateSubstitutions(components.tail, otherComponents.tail)))
    }

    override def applySubstitutions(
      components: Components,
      substitutions: Substitutions
    ): Components = {
      components.head.applySubstitutions(substitutions) :: inner.applySubstitutions(components.tail, substitutions)
    }

    override def substituteFreeVariable(
      components: Components,
      termToReplaceWith: Term,
      termToBeReplaced: TermVariable
    ): Components = {
      components.head.substituteFreeVariable(termToReplaceWith, termToBeReplaced) ::
        inner.substituteFreeVariable(components.tail, termToReplaceWith, termToBeReplaced)
    }

    override def attemptSimplification(
      components: Components,
      otherComponents: Components
    ): Option[DistinctVariables] = {
      for {
        head <- components.head.attemptSimplification(otherComponents.head)
        tail <- inner.attemptSimplification(components.tail, otherComponents.tail)
      } yield head ++ tail
    }

    override def makeSimplifications(
      components: Components,
      distinctVariables: DistinctVariables
    ): Components = {
      components.head.makeSimplifications(distinctVariables) ::
        inner.makeSimplifications(components.tail, distinctVariables)
    }

    override def containsTerms(
      components: Components
    ) = {
      components.head.containsTerms || inner.containsTerms(components.tail)
    }
  }
}
