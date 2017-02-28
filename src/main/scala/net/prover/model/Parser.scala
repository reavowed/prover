package net.prover.model

import scala.util.control.NonFatal

case class Parser[+T](attemptParse: PartialLine => (T, PartialLine)) {
  def map[S](f: T => S): Parser[S] = Parser(l => attemptParse(l).mapLeft(f))
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
  def onlyIf(f: T => Boolean): Parser[Option[T]] = map { t => if (f(t)) Some(t) else None }
  def inParens: Parser[T] = Parser { line =>
    if (line.remainingText.head != '(') {
      throw new Exception("Open-paren expected but not found")
    }
    val (t, remainingLine) = attemptParse(line.tail)
    if (remainingLine.remainingText.head != ')') {
      throw new Exception("Close-paren expected but not found")
    }
    (t, remainingLine.tail)
  }
  def optionalInParens: Parser[Option[T]] = Parser[Option[T]] { line =>
    if (line.remainingText.head != ')')
      attemptParse(line).mapLeft(Some.apply)
    else
      (None, line)
  }.inParens

  def listInParens(separatorOption: Option[String]) = {
    def parseNext(lineSoFar: PartialLine, acc: Seq[T]): (Seq[T], PartialLine) = {
      if (lineSoFar.remainingText.head == ')') {
        (acc, lineSoFar)
      } else {
        val lineToParse = separatorOption match {
          case Some(separator) if acc.nonEmpty =>
            if (lineSoFar.remainingText.startsWith(separator)) lineSoFar.tail
            else throw new Exception("Expected separator or end-paren after list item")
          case _ =>
            lineSoFar
        }
        val (next, remainingLine) = attemptParse(lineToParse)
        parseNext(remainingLine, acc :+ next)
      }
    }
    Parser { line =>
      parseNext(line, Nil)
    }.inParens
  }

  def parse(partialLine: PartialLine): (T, PartialLine) = {
    try {
      attemptParse(partialLine)
    } catch {
      case e @ (_:ParseException | _:ArbitraryVariableException | _:DistinctVariableViolationException) => throw e
      case NonFatal(e) => throw new ParseException(e.getMessage, partialLine.fullLine)
    }
  }
  def parseAndDiscard(line: PartialLine): T = parse(line)._1
  def parseAndDiscard(line: BookLine): T = parseAndDiscard(line.asPartialLine)
}

object Parser {
  def constant[T](t: T): Parser[T] = Parser { (t, _) }

  def allRemaining: Parser[String] = Parser { l => (l.remainingText, l.copy(remainingText = "")) }

  def singleWord: Parser[String] = Parser { l =>
    val (word, remainingText) = l.remainingText.trim.span(c => !c.isWhitespace && !"),".contains(c))
    (word, l.copy(remainingText = remainingText.trim))
  }

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
