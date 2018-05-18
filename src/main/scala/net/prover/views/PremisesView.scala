package net.prover.views

import net.prover.model._
import net.prover.model.expressions.Statement
import net.prover.model.proof.{ProofLine, ReferenceMap}

import scala.xml.{Elem, Text}

object PremisesView {
  private def PremiseView(statement: Statement, referrers: Set[(String, Seq[Int])] = Set.empty)(implicit displayContext: DisplayContext): Elem = {
    <span class="premise">{ExpressionView(ProofLine.Expression.create(statement, referrers))}</span>
  }
  private def PremiseView(premise: Premise, referenceMap: ReferenceMap)(implicit displayContext: DisplayContext): Elem = {
    PremiseView(premise.statement, referenceMap.getReferrers(premise.reference.value))
  }
  private def applyToElements(premiseElems: Seq[Elem]): Elem = {
    <div>
      { premiseElems match {
        case Nil => {}
        case Seq(single) => {
          <div class="premises">
            Suppose {single}.
          </div>
        }
        case init :+ last => {
          <div class="premises">
            Suppose {init.interleave(Text(", "))} and {last}.
          </div>
        }
      }}
    </div>
  }

  def apply(premises: Seq[Premise], referenceMap: ReferenceMap)(implicit displayContext: DisplayContext): Elem = {
    applyToElements(premises.map(PremiseView(_, referenceMap)))
  }
  def apply(statements: Seq[Statement])(implicit displayContext: DisplayContext): Elem = {
    applyToElements(statements.map(PremiseView(_)))
  }
}
