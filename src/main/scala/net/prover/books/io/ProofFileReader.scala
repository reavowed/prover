package net.prover.books.io

import java.nio.file.{Files, Path}
import net.prover.model._
import net.prover.model.entries.ChapterEntry

import java.util.regex.Pattern
import java.util.stream.Collectors
import scala.collection.JavaConverters._

case class ProofFileReader(chapterDirectoryPath: Path, keyAccumulator: KeyAccumulator) {
  private lazy val files = Files.find(chapterDirectoryPath, 1, (_, attributes) => attributes.isRegularFile).collect(Collectors.toList[Path]).asScala

  def getSerializedProofs(theoremTitle: String): Seq[String] = {
    val (key, _) = keyAccumulator.getNextKey(theoremTitle.formatAsKey)
    val fileNameRegex = ("\\d+\\." + Pattern.quote(key) + "\\.(\\d+)\\.proof").r
    files.mapCollect(path => {
      val filename = path.getFileName.toString
      fileNameRegex.unapplySeq(filename).map(g => new String(Files.readAllBytes(path), "UTF-8") -> g.head.toInt)
    }).sortBy(_._2).map(_._1)
  }

  def addEntry(entry: ChapterEntry): ProofFileReader = {
    copy(keyAccumulator = keyAccumulator.getNextKey(entry.name.formatAsKey)._2)
  }
}
