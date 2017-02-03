package net.prover.model

case class Parser[T](parse: PartialLine => (T, PartialLine)) {
  def map[S](f: T => S): Parser[S] = Parser(l => parse(l).mapLeft(f))
  def mapWithLine[S](f: (T, PartialLine) => S) = Parser(l => parse(l).mapLeft(f(_, l)))
  def flatMap[S](f: T => Parser[S]): Parser[S] = Parser { line =>
    val (t, remainingLine) = parse(line)
    f(t).parse(remainingLine)
  }
  def withFilter(f: T => Boolean): Parser[T] = Parser { line =>
    val (t, remainingLine) = parse(line)
    if (f(t)) {
      (t, remainingLine)
    } else {
      line.throwParseException(s"Parse value '$t' did not match filter")
    }
  }
  def inParens: Parser[T] = Parser { line =>
    Parser.inParens(line, parse)
  }
  def optionalInParens: Parser[Option[T]] = Parser { line =>
    Parser.optionalInParens(line, parse)
  }
  def listInParens(separator: Option[String]) = Parser { line =>
    Parser.listInParens(line, parse, separator)
  }
}

object Parser {

  def constant[T](t: T): Parser[T] = Parser { (t, _) }

  def singleWord: Parser[String] = Parser(_.splitFirstWord)

  def inParens[T](line: PartialLine, f: PartialLine => (T, PartialLine)): (T, PartialLine) = {
    if (line.remainingText.head != '(') {
      throw ParseException.withMessage("Open-paren expected but not found", line.fullLine)
    }
    val (t, remainingLine) = f(line.tail)
    if (remainingLine.remainingText.head != ')') {
      throw ParseException.withMessage("Close-paren expected but not found", line.fullLine)
    }
    (t, remainingLine.tail)
  }

  def optionalInParens[T](line: PartialLine, f: PartialLine => (T, PartialLine)): (Option[T], PartialLine) = {
    if (line.remainingText.head != '(') {
      throw ParseException.withMessage("Open-paren expected but not found", line.fullLine)
    }
    val lineAfterOpenParen = line.tail
    if (lineAfterOpenParen.remainingText.head == ')') {
      (None, lineAfterOpenParen.tail)
    } else {
      val (t, remainingLine) = f(line.tail)
      if (remainingLine.remainingText.head != ')') {
        throw ParseException.withMessage("Close-paren expected but not found", line.fullLine)
      }
      (Some(t), remainingLine.tail)
    }
  }

  def listInParens[T](line: PartialLine, f: PartialLine => (T, PartialLine), separatorOption: Option[String] = Some(",")): (Seq[T], PartialLine) = {
    def parseRecursive(lineSoFar: PartialLine, acc: Seq[T]): (Seq[T], PartialLine) = {
      if (lineSoFar.remainingText.head == ')') {
        (acc, lineSoFar.tail)
      } else {
        val lineToParse = separatorOption match {
          case Some(separator) if acc.nonEmpty =>
            if (lineSoFar.remainingText.startsWith(separator)) lineSoFar.tail
            else throw ParseException.withMessage("Expected comma or end-paren after list item", line.fullLine)
          case _ =>
            lineSoFar
        }
        val (next, remainingLine) = f(lineToParse)
        parseRecursive(remainingLine, acc :+ next)
      }
    }
    if (line.remainingText.head != '(') {
      throw ParseException.withMessage("Open-paren expected but not found", line.fullLine)
    }
    parseRecursive(line.tail, Nil)
  }

  def allInParens: Parser[String] = Parser(_.toEndOfParens).inParens
}
