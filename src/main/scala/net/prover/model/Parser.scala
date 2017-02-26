package net.prover.model

import scala.util.control.NonFatal

case class Parser[T](attemptParse: PartialLine => (T, PartialLine)) {
  def map[S](f: T => S): Parser[S] = Parser(l => attemptParse(l).mapLeft(f))
  def mapWithLine[S](f: (T, PartialLine) => S) = Parser(l => attemptParse(l).mapLeft(f(_, l)))
  def flatMap[S](f: T => Parser[S]): Parser[S] = Parser { line =>
    val (t, remainingLine) = attemptParse(line)
    f(t).attemptParse(remainingLine)
  }
  def flatMapOption[S](f: T => Option[Parser[S]]): Parser[Option[S]] = Parser { line =>
    val (t, remainingLine) = attemptParse(line)
    f(t) match {
      case Some(otherParser) =>
        otherParser.attemptParse(remainingLine).mapLeft(Some.apply)
      case None =>
        (None, line)
    }
  }
  def withFilter(f: T => Boolean): Parser[T] = Parser { line =>
    val (t, remainingLine) = attemptParse(line)
    if (f(t)) {
      (t, remainingLine)
    } else {
      line.throwParseException(s"Parse value '$t' did not match filter")
    }
  }
  def onlyIf(f: T => Boolean): Parser[Option[T]] = map { t => if (f(t)) Some(t) else None }
  def inParens: Parser[T] = Parser { line =>
    if (line.remainingText.head != '(') {
      throw ParseException.withMessage("Open-paren expected but not found", line.fullLine)
    }
    val (t, remainingLine) = attemptParse(line.tail)
    if (remainingLine.remainingText.head != ')') {
      throw ParseException.withMessage("Close-paren expected but not found", line.fullLine)
    }
    (t, remainingLine.tail)
  }
  def optionalInParens: Parser[Option[T]] = Parser { line =>
    if (line.remainingText.head != '(') {
      throw ParseException.withMessage("Open-paren expected but not found", line.fullLine)
    }
    val lineAfterOpenParen = line.tail
    if (lineAfterOpenParen.remainingText.head == ')') {
      (None, lineAfterOpenParen.tail)
    } else {
      val (t, remainingLine) = attemptParse(line.tail)
      if (remainingLine.remainingText.head != ')') {
        throw ParseException.withMessage("Close-paren expected but not found", line.fullLine)
      }
      (Some(t), remainingLine.tail)
    }
  }
  def listInParens(separatorOption: Option[String]) = Parser { line =>
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
        val (next, remainingLine) = attemptParse(lineToParse)
        parseRecursive(remainingLine, acc :+ next)
      }
    }
    if (line.remainingText.head != '(') {
      throw ParseException.withMessage("Open-paren expected but not found", line.fullLine)
    }
    parseRecursive(line.tail, Nil)
  }

  def parse(partialLine: PartialLine): (T, PartialLine) = {
    try {
      attemptParse(partialLine)
    } catch {
      case e @ (_:ParseException | _:ArbitraryVariableException | _:DistinctVariableViolationException) => throw e
      case NonFatal(e) => partialLine.throwParseException(e.getMessage)
    }
  }
  def parseAndDiscard(line: PartialLine): T = parse(line)._1
  def parseAndDiscard(line: BookLine): T = parseAndDiscard(line.asPartialLine)
}

object Parser {
  def constant[T](t: T): Parser[T] = Parser { (t, _) }

  def allRemaining: Parser[String] = Parser { l => (l.remainingText, l.copy(remainingText = "")) }

  def singleWord: Parser[String] = Parser(_.splitFirstWord)

  def allInParens: Parser[String] = Parser(_.toEndOfParens).inParens

  implicit class OptionParserOps[T](parser: Parser[Option[T]]) {
    def orElse(otherParser: => Parser[T]): Parser[T] = Parser { line =>
      parser.attemptParse(line) match {
        case (Some(t), remainingLine) =>
          (t, remainingLine)
        case (None, _) =>
          otherParser.attemptParse(line)
      }
    }
  }
}
