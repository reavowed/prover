package net.prover.model

case class FileLocation(fileName: String, lineNumber: Int) {
  override def toString = s"[$fileName line $lineNumber]"
}