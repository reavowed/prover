package net.prover.model.proof

import net.prover.model.Parser

sealed trait PreviousLineReference {
  def addInternalPath(additionalInternalPath: Seq[Int]): PreviousLineReference
  def withoutInternalPath: PreviousLineReference
  def serialize: String
}

case class PremiseReference(premiseIndex: Int) extends PreviousLineReference.TopLevel {
  override def addInternalPath(internalPath: Seq[Int]) = InternalPremiseReference(premiseIndex, internalPath)
  override def serialize: String = "p" + premiseIndex
}
case class StepReference(stepPath: Seq[Int]) extends PreviousLineReference.TopLevel {
  def forChild(index: Int): StepReference = copy(stepPath = stepPath :+ index)
  def withSuffix(suffix: String): StepChildReference = StepChildReference(stepPath, suffix)
  override def addInternalPath(internalPath: Seq[Int]) = InternalStepReference(stepPath, internalPath)
  override def serialize: String = stepPath.mkString(".")
}
case class StepChildReference(stepPath: Seq[Int], suffix: String) extends PreviousLineReference.TopLevel {
  override def addInternalPath(internalPath: Seq[Int]) = InternalStepChildReference(stepPath, suffix, internalPath)
  override def serialize: String = stepPath.mkString(".") + suffix
}

case class InternalPremiseReference(premiseIndex: Int, internalPath: Seq[Int]) extends PreviousLineReference.Internal {
  override protected def withInternalPath(newInternalPath: Seq[Int]): PreviousLineReference = copy(internalPath = newInternalPath)
  override def withoutInternalPath: PreviousLineReference = PremiseReference(premiseIndex)
  override def serialize: String = "p" + premiseIndex + "-" + internalPath.mkString(".")
}
case class InternalStepReference(stepPath: Seq[Int], internalPath: Seq[Int]) extends PreviousLineReference.Internal {
  override protected def withInternalPath(newInternalPath: Seq[Int]): PreviousLineReference = copy(internalPath = newInternalPath)
  override def withoutInternalPath: PreviousLineReference = StepReference(stepPath)
  override def serialize: String = stepPath.mkString(".") + "-" + internalPath.mkString(".")
}
case class InternalStepChildReference(stepPath: Seq[Int], suffix: String, internalPath: Seq[Int]) extends PreviousLineReference.Internal {
  override protected def withInternalPath(newInternalPath: Seq[Int]): PreviousLineReference = copy(internalPath = newInternalPath)
  override def withoutInternalPath: PreviousLineReference = StepChildReference(stepPath, suffix)
  override def serialize: String = stepPath.mkString(".") + suffix + "-" + internalPath.mkString(".")
}

object PreviousLineReference {
  sealed trait TopLevel extends PreviousLineReference {
    override def withoutInternalPath: PreviousLineReference = this
  }
  sealed trait Internal extends PreviousLineReference {
    def internalPath: Seq[Int]
    protected def withInternalPath(newInternalPath: Seq[Int]): PreviousLineReference
    override def addInternalPath(additionalInternalPath: Seq[Int]): PreviousLineReference = withInternalPath(internalPath ++ additionalInternalPath)
  }
  private val premiseRegex = "p(\\d+)".r
  private val stepRegex = "(\\d+(?:\\.\\d+)*)(\\w)?".r
  def parser: Parser[PreviousLineReference] = Parser.singleWord.map {
    case premiseRegex(indexText) =>
      val index = indexText.toInt
      PremiseReference(index)
    case stepRegex(pathText, suffix) =>
      val path = pathText.split("\\.").map(_.toInt)
      val base = StepReference(path)
      Option(suffix).map(base.withSuffix).getOrElse(base)
    case s =>
      throw new Exception(s"Unrecognised reference $s")
  }
}

