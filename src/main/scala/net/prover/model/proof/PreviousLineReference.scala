package net.prover.model.proof

sealed trait PreviousLineReference {
  def addInternalPath(additionalInternalPath: Seq[Int]): PreviousLineReference
}

case class PremiseReference(premiseIndex: Int) extends PreviousLineReference {
  override def addInternalPath(internalPath: Seq[Int]) = InternalPremiseReference(premiseIndex, internalPath)
}
case class StepReference(stepPath: Seq[Int]) extends PreviousLineReference {
  def forChild(index: Int): StepReference = copy(stepPath = stepPath :+ index)
  def withSuffix(suffix: String): StepChildReference = StepChildReference(stepPath, suffix)
  override def addInternalPath(internalPath: Seq[Int]) = InternalStepReference(stepPath, internalPath)
}
case class StepChildReference(stepPath: Seq[Int], suffix: String) extends PreviousLineReference {
  override def addInternalPath(internalPath: Seq[Int]) = InternalStepChildReference(stepPath, suffix, internalPath)
}

case class InternalPremiseReference(premiseIndex: Int, internalPath: Seq[Int]) extends PreviousLineReference.Internal {
  override protected def withInternalPath(newInternalPath: Seq[Int]): PreviousLineReference = copy(internalPath = newInternalPath)
}
case class InternalStepReference(stepPath: Seq[Int], internalPath: Seq[Int]) extends PreviousLineReference.Internal {
  override protected def withInternalPath(newInternalPath: Seq[Int]): PreviousLineReference = copy(internalPath = newInternalPath)
}
case class InternalStepChildReference(stepPath: Seq[Int], suffix: String, internalPath: Seq[Int]) extends PreviousLineReference.Internal {
  override protected def withInternalPath(newInternalPath: Seq[Int]): PreviousLineReference = copy(internalPath = newInternalPath)
}

object PreviousLineReference {
  sealed trait Internal extends PreviousLineReference {
    def internalPath: Seq[Int]
    protected def withInternalPath(newInternalPath: Seq[Int]): PreviousLineReference
    override def addInternalPath(additionalInternalPath: Seq[Int]): PreviousLineReference = withInternalPath(internalPath ++ additionalInternalPath)
  }
}

