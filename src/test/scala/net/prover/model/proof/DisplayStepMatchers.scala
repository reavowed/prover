package net.prover.model.proof

import net.prover.theorems.DisplayStep
import org.specs2.matcher.Matcher
import org.specs2.mutable.SpecificationLike

object DisplayStepMatchers extends SpecificationLike {

  def referenceStep(step: DisplayStep): Matcher[Premise] = {
    beAnInstanceOf[Premise.Given] and
      beAnInstanceOf[StepReference] ^^ ((_: Premise).asInstanceOf[Premise.Given].referencedLine)
      beEqualTo(step.statement) ^^ ((_: Premise).asInstanceOf[Premise.Given].statement) and
      beEqualTo(step.path) ^^ ((_: Premise).asInstanceOf[Premise.Given].referencedLine.asInstanceOf[StepReference].stepPath)
  }

}
