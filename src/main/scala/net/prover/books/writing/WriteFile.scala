package net.prover.books.writing

import java.nio.file.{Files, Path}

object WriteFile {
  def apply(path: Path, contents: String): Unit = {
    Files.createDirectories(path.getParent)
    Files.write(path, contents.getBytes("UTF-8"))
  }
}
