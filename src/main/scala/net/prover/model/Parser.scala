package net.prover.model

import java.nio.file.{Path, Paths}

import scala.util.Try
import scala.util.control.NonFatal

case class Parser[+T](attemptParse: Tokenizer => (T, Tokenizer)) {
  def map[S](f: T => S): Parser[S] = Parser(tokenizer => attemptParse(tokenizer).mapLeft(f))
  def flatMap[S](f: T => Parser[S]): Parser[S] = Parser { tokenizer =>
    val (t, nextTokenizer) = attemptParse(tokenizer)
    f(t).parse(nextTokenizer)
  }
  def flatMapOption[S](f: T => Option[Parser[S]]): Parser[Option[S]] = Parser { tokenizer =>
    val (t, nextTokenizer) = attemptParse(tokenizer)
    f(t) match {
      case Some(otherParser) =>
        otherParser.parse(nextTokenizer).mapLeft(Some.apply)
      case None =>
        (None, tokenizer)
    }
  }
  def onlyIf(f: T => Boolean): Parser[Option[T]] = Parser { tokenizer =>
    val (t, nextTokenizer) = attemptParse(tokenizer)
    if (f(t))
      (Some(t), nextTokenizer)
    else
      (None, tokenizer)
  }
  def tryOrElse[S >: T](otherParser: => Parser[S]): Parser[S] = Parser { tokenizer =>
    Try(attemptParse(tokenizer)).toOption.getOrElse(otherParser.attemptParse(tokenizer))
  }

  private def inBrackets(openBracket: String, closeBracket: String): Parser[T] = {
    for {
      _ <- Parser.requiredWord(openBracket)
      t <- this
      _ <- Parser.requiredWord(closeBracket)
    } yield t
  }

  def inParens: Parser[T] = inBrackets("(", ")")
  def optionalInParens: Parser[Option[T]] = Parser[Option[T]] { tokenizer =>
    val (nextToken, _) = tokenizer.readNext()
    if (nextToken != ")")
      attemptParse(tokenizer).mapLeft(Some.apply)
    else
      (None, tokenizer)
  }.inParens
  def listInParens(separatorOption: Option[String]) = {
    def parseNext(tokenizer: Tokenizer, acc: Seq[T] = Nil): (Seq[T], Tokenizer) = {
      val (nextToken, nextTokenizer) = tokenizer.readNext()
      if (nextToken == ")") {
        (acc, tokenizer)
      } else {
        val tokenizerToUse = separatorOption match {
          case Some(separator) if acc.nonEmpty =>
            if (nextToken == separator) nextTokenizer
            else throw new Exception("Expected separator or end-paren after list item")
          case _ =>
            tokenizer
        }
        val (next, remainingTokenizer) = parse(tokenizerToUse)
        parseNext(remainingTokenizer, acc :+ next)
      }
    }
    Parser(parseNext(_, Nil)).inParens
  }

  def inBraces: Parser[T] = inBrackets("{", "}")

  def parse(tokenizer: Tokenizer): (T, Tokenizer) = {
    try {
      attemptParse(tokenizer)
    } catch {
      case e @ (_:ParseException | _:ParseException.NoWrap) => throw e
      case NonFatal(e) => tokenizer.throwParseException(e.getMessage, Some(e))
    }
  }
  def parseAndDiscard(text: String, path: Path): T = {
    parse(Tokenizer.fromString(text, path))._1
  }
}

object Parser {
  def constant[T](t: T): Parser[T] = Parser { (t, _) }

  def toEndOfLine: Parser[String] = Parser { t => t.readUntilEndOfLine() }

  def singleWord: Parser[String] = Parser { t => t.readNext() }

  def optionalWord(expectedWord: String): Parser[Option[Unit]] = Parser { tokenizer =>
    if (tokenizer.isEmpty) {
      (None, tokenizer)
    } else {
      val (word, nextTokenizer) = tokenizer.readNext()
      if (word == expectedWord)
        (Some(()), nextTokenizer)
      else
        (None, tokenizer)
    }
  }

  def selectWord[T](f: PartialFunction[String, Parser[T]]): Parser[Option[T]] = Parser { tokenizer =>
    if (tokenizer.isEmpty) {
      (None, tokenizer)
    } else {
      val (word, nextTokenizer) = tokenizer.readNext()
      if (f.isDefinedAt(word))
        f(word).attemptParse(nextTokenizer).mapLeft(Some.apply)
      else
        (None, tokenizer)
    }
  }

  def requiredWord(expectedWord: String): Parser[Unit] = Parser { tokenizer =>
    val (word, nextTokenizer) = tokenizer.readNext()
    if (word == expectedWord)
      ((), nextTokenizer)
    else
      throw new Exception(s"Expected '$expectedWord' but found '$word'")
  }

  def singleWordIfAny: Parser[Option[String]] = Parser { t =>
    if (t.isEmpty) {
      (None, t)
    } else {
      t.readNext().mapLeft(Some.apply)
    }
  }

  def int: Parser[Int] = Parser.singleWord.map(_.toInt)

  def allInParens: Parser[String] = Parser(_.readUntilCloseParen()).inParens

  implicit class OptionParserOps[T](parser: Parser[Option[T]]) {
    def mapMap[S](f: T => S): Parser[Option[S]] = parser.map(_.map(f))
    def mapFlatMap[S](f: T => Parser[S]): Parser[Option[S]] = parser.flatMap {
      case Some(t) =>
        f(t).map(Some.apply)
      case None =>
        Parser.constant(None)
    }
    def flatMapFlatMap[S](f: T => Parser[Option[S]]): Parser[Option[S]] = parser.flatMap {
      case Some(t) =>
        f(t)
      case None =>
        Parser.constant(None)
    }
    def orElse[S >: T](otherParser: => Parser[S]): Parser[S] = Parser { tokenizer =>
      parser.parse(tokenizer) match {
        case (Some(t), nextTokenizer) =>
          (t, nextTokenizer)
        case (None, _) =>
          otherParser.parse(tokenizer)
      }
    }
    def getOrElse[S >: T](other: => S): Parser[S] = Parser { tokenizer =>
      parser.parse(tokenizer) match {
        case (Some(t), nextTokenizer) =>
          (t, nextTokenizer)
        case (None, _) =>
          (other, tokenizer)
      }
    }
    def isDefined: Parser[Boolean] = parser.map(_.isDefined)
    def isUndefined: Parser[Boolean] = parser.map(_.isEmpty)
    def whileDefined: Parser[Seq[T]] = {
      def readNext(acc: Seq[T], tokenizer: Tokenizer): (Seq[T], Tokenizer) = {
        parser.parse(tokenizer) match {
          case (Some(t), nextTokenizer) =>
            readNext(acc :+ t, nextTokenizer)
          case (None, _) =>
            (acc, tokenizer)
        }
      }
      Parser { tokenizer =>
        readNext(Nil, tokenizer)
      }
    }

    def collectWhileDefined: Parser[Seq[T]] = {
      iterateWhileDefined[Seq[T]](Seq.empty) { seq =>
        parser.mapMap(seq :+ _)
      }
    }
  }

  def optional[T](
    name: String,
    parser: Parser[T],
    default: => T
  ): Parser[T] = {
    Parser.optionalWord(name)
      .mapFlatMap(_ => parser)
      .getOrElse(default)
  }

  def optional[T](
    name: String,
    parser: Parser[T]
  ): Parser[Option[T]] = {
    optional(name, parser.map(Some.apply), None)
  }

  def iterateWhileDefined[T](
    initial: T)(
    parseFn: T => Parser[Option[T]]
  ): Parser[T] = {
    parseFn(initial)
      .mapFlatMap(iterateWhileDefined(_)(parseFn))
      .getOrElse(initial)
  }
}
