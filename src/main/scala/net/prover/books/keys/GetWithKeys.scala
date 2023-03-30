package net.prover.books.keys

import net.prover.model.StringOps

object GetWithKeys {
  def apply[T : WithKeyProperty](values: Seq[T]): List[(T, String)] = {
    values.foldLeft((List.empty[(T, String)], KeyAccumulator.Empty)) { case ((results, acc), t) =>
      val (key, newAcc) = acc.getNextKey(WithKeyProperty[T].getKey(t))
      (results :+ (t -> key), newAcc)
    }._1
  }
}
