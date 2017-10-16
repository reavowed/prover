package net.prover.model

import net.prover.model.proof.InferenceApplication
import net.prover.model.proof.Reference.Elided

object HtmlHelper {
  def format(text: String): String = {
    text.replaceAll("_([^\\s)}]+)", "<sub>$1</sub>")
      .replaceAll("\\^([^\\s)}]+)", "<sup>$1</sup>")
  }
  def findInferenceToDisplay(inferenceApplication: InferenceApplication): Inference = {
    inferenceApplication.references.collect {
      case Elided(innerApplication) => innerApplication
    } match {
      case Seq(innerApplication) => findInferenceToDisplay(innerApplication)
      case Nil => inferenceApplication.inference
    }
  }
}
