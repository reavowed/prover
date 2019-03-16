package net.prover.views

import net.prover.model._
import net.prover.model.expressions.Statement
import scala.xml.{Elem, Text}

object PremisesView {
  private def PremiseView(statement: Statement)(implicit displayContext: DisplayContext): Elem = {
    <span class="premise">{ExpressionView(statement)}</span>
  }
  private def PremiseView(premise: Premise)(implicit displayContext: DisplayContext): Elem = {
    PremiseView(premise.statement)
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

  def apply(premises: Seq[Premise])(implicit displayContext: DisplayContext): Elem = {
    applyToElements(premises.map(PremiseView))
  }
  def apply(statements: Seq[Statement])(implicit displayContext: DisplayContext, dummyImplicit: DummyImplicit): Elem = {
    applyToElements(statements.map(PremiseView(_)))
  }
}
