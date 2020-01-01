package net.prover.controllers.models

case class StepMoveRequest(sourcePath: Seq[Int], sourceStartIndex: Int, sourceEndIndex: Int, destinationPath: Seq[Int], destinationIndex: Int)
