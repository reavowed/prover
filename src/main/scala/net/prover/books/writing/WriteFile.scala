package net.prover.books.writing

import net.prover.books.model.FileDefinition

import java.nio.file.{Files, Path}

object WriteFile {
  def apply(fileDefinition: FileDefinition): Unit = {
    apply(fileDefinition.path, fileDefinition.contents)
  }
  def apply(path: Path, contents: String): Unit = {
    Files.createDirectories(path.getParent)
    Files.write(path, contents.getBytes("UTF-8"))
  }
}
