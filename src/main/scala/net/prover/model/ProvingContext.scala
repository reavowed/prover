package net.prover.model

import net.prover.model.definitions.Definitions.RelationDefinitions
import net.prover.model.definitions._
import net.prover.model.entries.{ChapterEntry, StatementDefinition, TermDefinition}
import net.prover.model.expressions.{DefinedStatement, Expression, Statement, Term}
import net.prover.model.proof.StepProvingContext

case class ProvingContext(entryContext: EntryContext, private val definitions: Definitions) {

  trait Allowable[-T] {
    def isAllowed(t: T): Boolean
  }
  trait AlwaysAllowable[-T] extends Allowable[T] {
    def isAllowed(t: T): Boolean = true
  }
  case class Replacable[T](replace: T => T)
  object Allowable {
    private val alwaysAllowable = new AlwaysAllowable[Any] {}
    private def allowable[T](f: T => Boolean) = new Allowable[T] {
      def isAllowed(t: T): Boolean = f(t)
    }

    implicit val alwaysAllowableStatement: AlwaysAllowable[Statement] = alwaysAllowable
    implicit val alwaysAllowableTerm: AlwaysAllowable[Term] = alwaysAllowable
    implicit val alwaysAllowableExpression: AlwaysAllowable[Expression] = alwaysAllowable
    implicit val alwaysAllowableString: AlwaysAllowable[String] = alwaysAllowable
    implicit def alwaysAllowableSeq[T](implicit inner: AlwaysAllowable[T]): AlwaysAllowable[Seq[T]] = alwaysAllowable

    implicit val allowableInference: Allowable[Inference] = allowable(i => entryContext.inferences.exists(_.id == i.id))
    implicit val allowableStatementDefinition: Allowable[StatementDefinition] = allowable(entryContext.statementDefinitions.contains)
    implicit val allowableTermDefinition: Allowable[TermDefinition] = allowable(entryContext.termDefinitions.contains)

    implicit val allowableRelation: Allowable[BinaryRelation] = allowable(r => definedBinaryRelations.exists(_._2 == r))
    implicit val allowableReversal: Allowable[Reversal] = allowable(r => isAllowed(r.relation) && isAllowed(r.inference))
    implicit val allowableTransitivity: Allowable[Transitivity] = allowable(r => isAllowed(r.relation) && isAllowed(r.inference))
    implicit val allowableExpansion: Allowable[Expansion] = allowable(r => isAllowed(r.relation) && isAllowed(r.inference))
    implicit val allowableSubstitution: Allowable[Substitution] = allowable(r => isAllowed(r.relation) && isAllowed(r.inference))

    implicit val alwaysAllowableOperator: Allowable[BinaryOperator] = alwaysAllowable
    implicit val allowableCommutativity: Allowable[Commutativity] = allowable(r => isAllowed(r.inference) && isAllowed(r.equality))
    implicit val allowableAssociativity: Allowable[Associativity] = allowable(r => isAllowed(r.inference) && isAllowed(r.equality))

    implicit val allowableEquality: Allowable[Equality] = allowable(e =>
      isAllowed(e.relation) &&
        isAllowed(e.reversal) &&
        isAllowed(e.transitivity) &&
        isAllowed(e.expansion) &&
        isAllowed(e.substitution)
    )
    implicit def allowableTuple2[A, B](
      implicit allowableA: Allowable[A],
      allowableB: Allowable[B]
    ): Allowable[(A, B)] = {
      allowable(t => allowableA.isAllowed(t._1) && allowableB.isAllowed(t._2))
    }
    implicit def allowableTuple3[A, B, C](
      implicit allowableA: Allowable[A],
      allowableB: Allowable[B],
      allowableC: Allowable[C]
    ): Allowable[(A, B, C)] = {
      allowable(t => allowableA.isAllowed(t._1) && allowableB.isAllowed(t._2) &&  allowableC.isAllowed(t._3))
    }
    implicit def allowableTuple4[A, B, C, D](
      implicit allowableA: Allowable[A],
      allowableB: Allowable[B],
      allowableC: Allowable[C],
      allowableD: Allowable[D]
    ): Allowable[(A, B, C, D)] = {
      allowable(t => allowableA.isAllowed(t._1) && allowableB.isAllowed(t._2) &&  allowableC.isAllowed(t._3)  &&  allowableD.isAllowed(t._4))
    }

    implicit def allowableOption[A](implicit allowableA: Allowable[A]): Allowable[Option[A]] = {
      allowable(o => o.forall(allowableA.isAllowed))
    }
  }

  object Replacable {
    implicit def replacableSeqFromReplacable[T](implicit replacable: Replacable[T]): Replacable[Seq[T]] = Replacable[Seq[T]](_.map(replacable.replace))
    implicit def replacableSeqFromAllowable[T](implicit allowable: Allowable[T]): Replacable[Seq[T]] = Replacable[Seq[T]](_.filter(allowable.isAllowed))
    implicit def replacableOptionFromReplacable[T](implicit replacable: Replacable[T]): Replacable[Option[T]] = Replacable[Option[T]](_.map(replacable.replace))
    implicit def replacableOptionFromAllowable[T](implicit allowable: Allowable[T]): Replacable[Option[T]] = Replacable[Option[T]](_.filter(allowable.isAllowed))
    implicit def replacableMapFromReplacable[T, S](implicit allowableT: Allowable[T], replacableS: Replacable[S]): Replacable[Map[T, S]] = {
      Replacable[Map[T, S]](m => m.filterKeys(allowableT.isAllowed).mapValues(replacableS.replace))
    }
    implicit def replacableMapFromAllowable[T, S](implicit allowableT: Allowable[T], allowableS: Allowable[S]): Replacable[Map[T, S]] = {
      Replacable[Map[T, S]](m => m.filter { case (t, s) => allowableT.isAllowed(t) && allowableS.isAllowed(s) })
    }

    implicit val replacableRelationDefinitions: Replacable[RelationDefinitions] = {
      Replacable[RelationDefinitions](d => d.copy(
        replace(d.reversal),
        replace(d.transitivity),
        replace(d.expansion),
        replace(d.substitution)))
    }
  }

  def isAllowed[T](t: T)(implicit allowable: Allowable[T]): Boolean = allowable.isAllowed(t)
  def replace[T](t: T)(implicit replacable: Replacable[T]): T = replacable.replace(t)

  def matchScopingStatement(statement: Statement): Option[(Statement, String, StatementDefinition)] = {
    replace(definitions.scopingDefinitionOption)
      .flatMap { scopingDefinition =>
        statement match {
          case definedStatement @ DefinedStatement(Seq(substatement), `scopingDefinition`) =>
            substatement.asOptionalInstanceOf[Statement].map((_, definedStatement.scopedBoundVariableNames.head, scopingDefinition))
          case _ =>
            None
        }
      }
  }
  def matchDeductionStatement(statement: Statement): Option[(Statement, Statement, StatementDefinition)] = {
    replace(definitions.deductionDefinitionOption)
      .flatMap { deductionDefinition =>
        statement match {
          case DefinedStatement(Seq(antecedentExpression, consequentExpression), `deductionDefinition`) =>
            for {
              antecedent <- antecedentExpression.asOptionalInstanceOf[Statement]
              consequent <- consequentExpression.asOptionalInstanceOf[Statement]
            } yield (antecedent, consequent, deductionDefinition)
          case _ =>
            None
        }
      }
  }
  lazy val definedBinaryRelations: Seq[(String, BinaryRelation)] = {
    Definitions.getDefinedBinaryRelations(
      entryContext.statementDefinitions,
      entryContext.displayShorthands,
      entryContext.termDefinitions)
  }

  lazy val rearrangeableFunctions: Seq[(BinaryOperator, Commutativity, Associativity)] = {
    replace(definitions.rearrangeableFunctions)
  }

  lazy val equalityOption: Option[Equality] = {
    replace(definitions.equalityOption)
  }

  lazy val definitionsByRelation: Map[BinaryRelation, RelationDefinitions] = {
    replace(definitions.definitionsByRelation)
  }
  lazy val transitivityDefinitions: Seq[(String, Transitivity)] = {
    replace(definitions.transitivityDefinitions)
  }

  lazy val premiseSimplificationInferences: Seq[(Inference, Statement)] = {
    replace(definitions.premiseSimplificationInferences)
  }
  lazy val conclusionSimplificationInferences: Seq[Inference] = {
    replace(definitions.conclusionSimplificationInferences)
  }
  lazy val rewriteInferences: Seq[(Inference, Statement)] = {
    replace(definitions.rewriteInferences)
  }

  lazy val statementExtractionInferences: Seq[(Inference, Statement, Option[Statement])] = {
    replace(definitions.statementExtractionInferences)
  }
  lazy val specificationInferenceOption: Option[(Inference, Statement, String, String)] = {
    replace(definitions.specificationInferenceOption)
  }

  lazy val termRewriteInferences: Seq[(Inference, Term, Term)] = {
    replace(definitions.termRewriteInferences)
  }
  lazy val termSimplificationInferences: Seq[(Inference, Term, Term)] = {
    replace(definitions.termSimplificationInferences)
  }
  lazy val termDesimplificationInferences: Seq[(Inference, Term, Term)] = {
    replace(definitions.termDesimplificationInferences)
  }
  lazy val statementDefinitionSimplifications: Map[StatementDefinition, Seq[(Inference, Statement, Expression)]] = {
    replace(definitions.statementDefinitionSimplifications)
  }
  def getStatementDefinitionSimplifications(statementDefinition: StatementDefinition): Seq[(Inference, Statement, Expression)] = {
    statementDefinitionSimplifications.getOrElse(statementDefinition, Nil)
  }
  lazy val statementDefinitionDeconstructions: Map[StatementDefinition, Inference] = {
    replace(definitions.statementDefinitionDeconstructions)
  }
  lazy val structuralSimplificationInferences: Seq[(Inference, Statement)] = {
    replace(definitions.structuralSimplificationInferences)
  }
  lazy val facts: Seq[Inference] = {
    replace(definitions.facts)
  }
}

object ProvingContext {
  def forEntry(allBooks: Seq[Book], definitions: Definitions, book: Book, chapter: Chapter, entry: ChapterEntry): ProvingContext = {
    ProvingContext(EntryContext.forEntry(allBooks, book, chapter, entry), definitions)
  }
  implicit def fromStepProvingContext(implicit stepProvingContext: StepProvingContext): ProvingContext = stepProvingContext.provingContext
}
