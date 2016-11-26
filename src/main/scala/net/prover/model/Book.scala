package net.prover.model

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

case class Book(
  title: String,
  connectives: Seq[Connective] = Nil,
  rules: Seq[Rule] = Nil,
  theorems: Seq[Theorem] = Nil) {
  def addConnective(statementDefinition: Connective): Book = {
    copy(connectives = connectives :+ statementDefinition)
  }
  def addRule(rule: Rule): Book = {
    copy(rules = rules :+ rule)
  }
  def addTheorem(theorem: Theorem): Book = {
    copy(theorems = theorems :+ theorem)
  }
}

object Book {

  private def addLinesToBook(lines: Seq[String], book: Book): Book = {
    lines match {
      case nextLine +: moreLines =>
        nextLine.splitKeyword match {
          case Seq("connective", connectiveText) =>
            val connective = Connective.parse(connectiveText)
            addLinesToBook(moreLines, book.addConnective(connective))
          case Seq("rule", ruleText) =>
            val rule = Rule.parse(ruleText, book.connectives)
            addLinesToBook(moreLines, book.addRule(rule))
          case Seq("theorem", theoremText) =>
            theoremText.splitKeyword match {
              case Seq(name, title) =>
                val (theorem, linesAfterTheorem) = Theorem.parse(name, title, moreLines, book.rules, book.connectives)
                addLinesToBook(linesAfterTheorem, book.addTheorem(theorem))
              case _ =>
                throw new Exception("Could not parse theorem name and title:\n" + nextLine)
            }
          case _ =>
            throw new Exception("Could not parse line:\n" + nextLine)
        }
      case Nil =>
        book
    }
  }

  def parse(s: String): Book = {
    val lines = s.lines.map(_.trim).filter(!_.isEmpty).filter(!_.startsWith("#")).toSeq
    lines match {
      case SingleWord("book", title) +: otherLines =>
        addLinesToBook(otherLines, Book(title))
      case _ =>
        throw new Exception("Book must start with title line")
    }
  }

  def fromFile(pathText: String): Book = {
    val path = Paths.get("book.txt")
    val bookText = new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
    parse(bookText)
  }
}
