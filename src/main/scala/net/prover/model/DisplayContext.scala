package net.prover.model

import net.prover.model.entries.DisplayShorthand

case class DisplayContext(displayShorthands: Seq[DisplayShorthand], boundVariableNames: Seq[Seq[String]]) {
  def withBoundVariableList(newBoundVariableNames: Seq[String]): DisplayContext = {
    if (newBoundVariableNames.nonEmpty)
      copy(boundVariableNames = newBoundVariableNames +: boundVariableNames)
    else
      this
  }
  def withBoundVariableLists(newBoundVariableNames: Seq[Seq[String]]): DisplayContext = {
    copy(boundVariableNames = newBoundVariableNames.filter(_.nonEmpty) ++ boundVariableNames)
  }
}
