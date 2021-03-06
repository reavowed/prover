package net.prover.model.definitions

import net.prover.model.ProvingContext
import net.prover.model.expressions.Term
import net.prover.model.proof.SubstitutionContext
import net.prover.util.Direction

sealed trait OperatorTree {
  def term: Term
  def canonicalForm(implicit provingContext: ProvingContext): OperatorTree
  override def toString = term.toString
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
    val absorbers = canonicalOperator.leftAbsorbers.map(a => Leaf(a.absorberTerm))

    def removeIdentities(components: Seq[OperatorTree]): Seq[OperatorTree] = {
      val identities = canonicalOperator.leftIdentities.map(i => Leaf(i.identityTerm))
      components.filter(c => !identities.contains(c))
    }
    def collapseInverses(components: Seq[OperatorTree]): Seq[OperatorTree] = {
      canonicalOperator.inverse match {
        case Some(inverse) =>
          def helper(previous: Seq[OperatorTree], next: Seq[OperatorTree]): Seq[OperatorTree] = next match {
            case (current @ UnaryOperatorTree(inverse.inverseOperator, inner)) +: tail =>
              previous.removeSingleValue(inner) match {
                case Some(newPrevious) =>
                  helper(newPrevious, tail)
                case None =>
                  tail.removeSingleValue(inner) match {
                    case Some(newTail) =>
                      helper(previous, newTail)
                    case None =>
                      helper(previous :+ current, tail)
                  }
              }
            case current +: tail =>
              helper(previous :+ current, tail)
            case Nil =>
              previous match {
                case Nil =>
                  Seq(Leaf(inverse.identity.identityTerm))
                case _ =>
                  previous
              }
          }
          helper(Nil, components)
        case None =>
          components
      }
    }
    // TODO: If there are ever two unary operators that can be extracted for a given binary operator, they can certainly be swapped and we should be handling that somehow
    def extractInverses(components: Seq[OperatorTree]): (Seq[(UnaryOperator, Int)], Seq[OperatorTree]) = {
      provingContext.leftOperatorExtractions.filter(_.binaryOperator == canonicalOperator.operator).foldLeft((Seq.empty[(UnaryOperator, Int)], components)) { case ((acc, components), operatorExtraction) =>
        val (numberOfOperators, resultingComponents) = components.mapFold(0) { case (i, component) =>
          component match {
            case UnaryOperatorTree(operatorExtraction.unaryOperator, inner) =>
              (i + 1, inner)
            case component =>
              (i, component)
          }
        }
        (acc :+ (operatorExtraction.unaryOperator, numberOfOperators), resultingComponents)
      }
    }

    absorbers.find(canonicalComponents.contains) getOrElse {
      val collapsedComponents = Seq(removeIdentities _, collapseInverses _).foldLeft(canonicalComponents) { case (c, f) => f(c) }
      val (unaryOperators, finalComponents) = extractInverses(collapsedComponents)
      val baseOperatorTree = OperatorTree.rebuild(canonicalOperator, finalComponents.sortBy(_.term.serialized))
      unaryOperators.foldLeft(baseOperatorTree) { case (operatorTree, (unaryOperator, count)) =>
        (1 to count).foldLeft(operatorTree) { case (operatorTree, _) => UnaryOperatorTree(unaryOperator, operatorTree)}
      }
    }
  }
}
object BinaryOperatorTree {
  def apply(operator: RearrangeableOperator, left: OperatorTree, right: OperatorTree, direction: Direction)(implicit substitutionContext: SubstitutionContext): BinaryOperatorTree = {
    BinaryOperatorTree(operator, direction.getSource(left, right), direction.getResult(left, right))
  }
}
