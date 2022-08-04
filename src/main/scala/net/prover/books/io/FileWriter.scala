package net.prover.books.io

import java.nio.file.{Files, Path}

object FileWriter {
  def write(fileDefinition: FileDefinition): Unit = {
    write(fileDefinition.path, fileDefinition.contents)
  }
  def write(path: Path, contents: String): Unit = {
    Files.createDirectories(path.getParent)
    Files.write(path, contents.getBytes("UTF-8"))
  }
}
