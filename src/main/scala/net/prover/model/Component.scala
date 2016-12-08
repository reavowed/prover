package net.prover.model

import shapeless.{::, HList, HNil}

trait Component[T <: Component[T]] {
  def variables: Variables
  def freeVariables: Seq[TermVariable]
  def attemptMatch(other: T): Option[MatchWithSubstitutions]
  def applyMatch(m: Match): T with Component[T]
  def substituteFreeVariable(termToReplaceWith: Term, termToBeReplaced: TermVariable): T
  def html: String
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
  def attemptMatch(components: Components, otherComponents: Components): Option[MatchWithSubstitutions]
  def applyMatch(components: Components, m: Match): Components
  def substituteTermVariables(
    components: Components,
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable
  ): Components

  def termDefinition(symbol: String, format: String, definition: Option[Statement]): TermDefinition[Components] = {
    TermDefinition[Components](symbol, this, format, definition)
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
    override def attemptMatch(
      components: Components,
      otherComponents: Components
    ): Option[MatchWithSubstitutions] = Some(MatchWithSubstitutions.empty)
    override def applyMatch(components: Components, m: Match): Components = HNil
    override def substituteTermVariables(
      components: Components,
      termToReplaceWith: Term,
      termToBeReplaced: TermVariable
    ): Components = HNil
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
      val updatedFormatString = formatString.replaceFirst("\\{\\}", components.head.toString)
      inner.format(updatedFormatString, components.tail)
    }

    override def attemptMatch(components: Components, otherComponents: Components): Option[MatchWithSubstitutions] = {
      MatchWithSubstitutions.mergeAttempts(Seq(
        components.head.attemptMatch(otherComponents.head),
        inner.attemptMatch(components.tail, otherComponents.tail)))
    }

    override def applyMatch(components: Components, m: Match): Components = {
      components.head.applyMatch(m) :: inner.applyMatch(components.tail, m)
    }

    override def substituteTermVariables(
      components: Components,
      termToReplaceWith: Term,
      termToBeReplaced: TermVariable
    ): Components = {
      components.head.substituteFreeVariable(termToReplaceWith, termToBeReplaced) ::
        inner.substituteTermVariables(components.tail, termToReplaceWith, termToBeReplaced)
    }
  }
}
