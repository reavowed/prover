package net.prover.model.proof

case class PreviousLineReference(lineReference: String, internalPath: Seq[Int]) {
  def addPath(additionalPath: Seq[Int]): PreviousLineReference = copy(internalPath = internalPath ++ additionalPath)
}
