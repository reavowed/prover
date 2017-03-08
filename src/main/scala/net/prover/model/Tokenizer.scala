package net.prover.model

import java.nio.file.{Files, Path}

trait Tokenizer {
  def currentBook: Option[String]
  def currentFile: String
  def currentLine: Int
  def currentColumn: Int

  def isEmpty: Boolean
  def readNext(): (String, Tokenizer)
  def readUntilEndOfLine(): (String, Tokenizer)
  def readUntilCloseParen(): (String, Tokenizer)

  def addTokenizer(tokenizer: StringTokenizer): Tokenizer

  def throwParseException(message: String, cause: Option[Throwable]): Nothing = {
    throw ParseException(
      s"Parse error in book '${currentBook.getOrElse("unknown")}'" +
        s" ($currentFile line $currentLine col $currentColumn): " +
        message,
      cause)
  }
}

case class StringTokenizer(
    text: String,
    currentBook: Option[String],
    currentFile: String,
    currentLine: Int,
    currentColumn: Int)
  extends Tokenizer
{
  val singleCharacterTokens = "(){},"

  def isEmpty: Boolean = text.isEmpty

  def readNext(): (String, StringTokenizer) = {
    if (text.isEmpty) throw new Exception("No tokens remaining")
    val (nextChar, nextTokenizer) = readChar()
    if (singleCharacterTokens.contains(nextChar)) {
      (nextChar.toString, nextTokenizer.readUntilEndOfWhitespace())
    } else {
      nextTokenizer.readUntilEndOfToken(nextChar.toString)
    }
  }

  def readUntilEndOfLine(): (String, StringTokenizer) = {
    readUntilEndOfLine("")
  }

  private def readUntilEndOfLine(textSoFar: String): (String, StringTokenizer) = {
    if (isEmpty)
      (textSoFar, this)
    else {
      val (nextChar, nextTokenizer) = readChar()
      if ("\r\n".contains(nextChar)) {
        (textSoFar.trim, nextTokenizer.readUntilEndOfWhitespace())
      } else {
        nextTokenizer.readUntilEndOfLine(textSoFar + nextChar)
      }
    }
  }

  private def readChar(): (Char, StringTokenizer) = {
    val c = text.head
    val (newLine, newColumn) = if (c == '\n') (currentLine + 1, 1) else (currentLine, currentColumn + 1)
    (c, copy(text = text.tail, currentLine = newLine, currentColumn = newColumn))
  }

  private def readUntilEndOfToken(tokenSoFar: String): (String, StringTokenizer) = {
    if (isEmpty)
      (tokenSoFar, this)
    else {
      val (nextChar, nextTokenizer) = readChar()
      if (singleCharacterTokens.contains(nextChar)) {
        (tokenSoFar, this)
      } else if (nextChar.isWhitespace) {
        (tokenSoFar, nextTokenizer.readUntilEndOfWhitespace())
      } else {
        nextTokenizer.readUntilEndOfToken(tokenSoFar + nextChar)
      }
    }
  }

  def readUntilEndOfWhitespace(): StringTokenizer = {
    if (isEmpty)
      this
    else {
      val (nextChar, nextTokenizer) = readChar()
      if (nextChar.isWhitespace) {
        nextTokenizer.readUntilEndOfWhitespace()
      } else if (nextChar == '#') {
        nextTokenizer.readUntilEndOfLine()._2.readUntilEndOfWhitespace()
      } else {
        this
      }
    }
  }

  def readUntilCloseParen(): (String, StringTokenizer) = {
    readUntilCloseParen("", 1)
  }

  def readUntilCloseParen(textSoFar: String, parenCount: Int): (String, StringTokenizer) = {
    if (isEmpty) throw new Exception("File ended without close-paren")
    val (nextChar, nextTokenizer) = readChar()
    if (parenCount == 1 && nextChar == ')')
      (textSoFar, this)
    else {
      val newParenCount = if (nextChar == ')') parenCount - 1 else if (nextChar == '(') parenCount + 1 else parenCount
      nextTokenizer.readUntilCloseParen(textSoFar + nextChar, newParenCount)
    }
  }

  def addTokenizer(tokenizer: StringTokenizer): Tokenizer = {
    if (isEmpty) tokenizer
    else CombinedTokenizer(tokenizer, this, Nil)
  }
}

case class CombinedTokenizer(
    currentTokenizer: StringTokenizer,
    nextTokenizer: StringTokenizer,
    otherTokenizers: Seq[StringTokenizer])
  extends Tokenizer {

  def currentBook: Option[String] = currentTokenizer.currentBook
  def currentFile: String = currentTokenizer.currentFile
  def currentLine: Int = currentTokenizer.currentLine
  def currentColumn: Int = currentTokenizer.currentColumn

  def isEmpty: Boolean = false
  def readNext(): (String, Tokenizer) = {
    val (token, updatedTokenizer) = currentTokenizer.readNext()
    (token, removeWhitespace(updatedTokenizer))
  }
  def readUntilEndOfLine(): (String, Tokenizer) = {
    currentTokenizer.readUntilEndOfLine().mapRight(removeWhitespace)
  }
  def readUntilCloseParen(): (String, Tokenizer) = {
    currentTokenizer.readUntilCloseParen().mapRight(removeWhitespace)
  }

  private def removeWhitespace(updatedTokenizer: StringTokenizer): Tokenizer = {
    if (updatedTokenizer.isEmpty)
      getNextTokenizer
    else
      copy(currentTokenizer = updatedTokenizer)
  }

  def getNextTokenizer: Tokenizer = {
    otherTokenizers match {
      case Nil =>
        nextTokenizer
      case firstOtherTokenizer +: otherOtherTokenizers =>
        CombinedTokenizer(nextTokenizer, firstOtherTokenizer, otherOtherTokenizers)
    }
  }

  def addTokenizer(tokenizer: StringTokenizer): Tokenizer = {
    CombinedTokenizer(tokenizer, currentTokenizer, nextTokenizer +: otherTokenizers)
  }
}

object Tokenizer {
  def fromString(str: String, path: Path): StringTokenizer = {
    StringTokenizer(str, None, path.toString, 1, 1).readUntilEndOfWhitespace()
  }
  def fromPath(path: Path): StringTokenizer = {
    fromString(new String(Files.readAllBytes(path), "UTF-8"), path)
  }
}
