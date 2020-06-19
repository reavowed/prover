package net.prover.model.definitions

import net.prover.model.ProvingContext
import net.prover.model.expressions.Term
import net.prover.model.proof.SubstitutionContext
import net.prover.util.Direction

sealed trait OperatorTree {
  def term: Term
  def canonicalForm(implicit provingContext: ProvingContext): OperatorTree
  override def toString = term.serialized
}
object OperatorTree {
  implicit def operatorTreeToTerm(operatorTree: OperatorTree): Term = operatorTree.term
  implicit def operatorTreeSeqToTermSeq(operatorTrees: Seq[OperatorTree]): Seq[Term] = operatorTrees.map(_.term)

  def parse(term: Term)(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): OperatorTree = {
    provingContext.rearrangeableOperators.mapFind { operator =>
      operator.unapply(term).map { case (left, right) =>
        BinaryOperatorTree(operator, parse(left), parse(right))
      }
    } orElse provingContext.unaryOperators.mapFind { operator =>
      operator.unapply(term).map { inner =>
        UnaryOperatorTree(operator, parse(inner))
      }
    } getOrElse Leaf(term)
  }

  @scala.annotation.tailrec
  def getComponentsOfCanonicalForm(operator: RearrangeableOperator, processedComponents: Seq[OperatorTree], componentsToProcess: Seq[OperatorTree])(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): (RearrangeableOperator, Seq[OperatorTree]) = {
    componentsToProcess match {
      case head +: tail =>
        head match {
          case BinaryOperatorTree(`operator`, left, right) =>
            getComponentsOfCanonicalForm(operator, processedComponents, Seq(left, right) ++ tail)
          case BinaryOperatorTree(innerOperator, left, right) =>
            val forwardDistributivity = provingContext.leftDistributivities.find(d => d.distributor == operator && d.distributee == innerOperator)
            val reverseDistributivity = provingContext.leftDistributivities.find(d => d.distributor == innerOperator && d.distributee == operator)
            if (forwardDistributivity.nonEmpty && (reverseDistributivity.isEmpty || innerOperator.operator.template.serialized < operator.operator.template.serialized))
              getComponentsOfCanonicalForm(innerOperator, Nil, Seq(rebuild(operator, left +: (processedComponents ++ tail)).canonicalForm, rebuild(operator, right +: (processedComponents ++ tail)).canonicalForm))
            else
              getComponentsOfCanonicalForm(operator, processedComponents :+ head, tail)
          case _ =>
            getComponentsOfCanonicalForm(operator, processedComponents :+ head, tail)
        }
      case Nil =>
        (operator, processedComponents)
    }
  }

  def rebuild(operator: RearrangeableOperator, inner: Seq[OperatorTree])(implicit substitutionContext: SubstitutionContext): OperatorTree = {
    inner.reduceRight(BinaryOperatorTree(operator, _, _))
  }
}

case class Leaf(rootTerm: Term) extends OperatorTree {
  def term: Term = rootTerm
  override def canonicalForm(implicit provingContext: ProvingContext): OperatorTree = this
}
case class UnaryOperatorTree(operator: UnaryOperator, inner: OperatorTree)(implicit val substitutionContext: SubstitutionContext) extends OperatorTree {
  def term: Term = operator(inner.term)
  override def canonicalForm(implicit provingContext: ProvingContext): OperatorTree = this.copy(inner = inner.canonicalForm)
}
case class BinaryOperatorTree(operator: RearrangeableOperator, left: OperatorTree, right: OperatorTree)(implicit val substitutionContext: SubstitutionContext) extends OperatorTree {
  def term: Term = operator(left.term, right.term)
  override def canonicalForm(implicit provingContext: ProvingContext): OperatorTree = {
    val (canonicalOperator, canonicalComponents) = OperatorTree.getComponentsOfCanonicalForm(operator, Nil, Seq(left.canonicalForm, right.canonicalForm))
    val absorbers = canonicalOperator.leftAbsorbers.map(a => OperatorTree.parse(a.absorberTerm))
    absorbers.find(canonicalComponents.contains) getOrElse {
      val identities = canonicalOperator.leftIdentities.map(i => OperatorTree.parse(i.identityTerm))
      val canonicalComponentsWithoutIdentities = canonicalComponents.filter(c => !identities.contains(c))
      OperatorTree.rebuild(canonicalOperator, canonicalComponentsWithoutIdentities.sortBy(_.term.serialized))
    }
  }
}
object BinaryOperatorTree {
  def apply(operator: RearrangeableOperator, left: OperatorTree, right: OperatorTree, direction: Direction)(implicit substitutionContext: SubstitutionContext): BinaryOperatorTree = {
    BinaryOperatorTree(operator, direction.getSource(left, right), direction.getResult(left, right))
  }
}
