package net.prover.model

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
  def inParens: Parser[T] = {
    for {
      _ <- Parser.singleWord.onlyIf(_ == "(").map(_.getOrElse(throw new Exception("Open-paren expected but not found")))
      t <- this
      _ <- Parser.singleWord.onlyIf(_ == ")").map(_.getOrElse(throw new Exception("Close-paren expected but not found")))
    } yield t
  }
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

  def parse(tokenizer: Tokenizer): (T, Tokenizer) = {
    try {
      attemptParse(tokenizer)
    } catch {
      case e @ (_:ParseException | _:ArbitraryVariableException | _:DistinctVariableViolationException) => throw e
      case NonFatal(e) => tokenizer.throwParseException(e.getMessage, Some(e))
    }
  }
}

object Parser {
  def constant[T](t: T): Parser[T] = Parser { (t, _) }

  def toEndOfLine: Parser[String] = Parser { t => t.readUntilEndOfLine() }

  def singleWord: Parser[String] = Parser { t => t.readNext() }

  def singleWordIfAny: Parser[Option[String]] = Parser { t =>
    if (t.isEmpty) {
      (None, t)
    } else {
      t.readNext().mapLeft(Some.apply)
    }
  }

  def allInParens: Parser[String] = Parser(_.readUntilCloseParen()).inParens

  implicit class OptionParserOps[T](parser: Parser[Option[T]]) {
    def mapFlatMap[S](f: T => Parser[S]): Parser[Option[S]] = parser.flatMap {
      case Some(t) =>
        f(t).map(Some.apply)
      case None =>
        Parser.constant(None)
    }
    def orElse(otherParser: => Parser[T]): Parser[T] = {
      parser.flatMap {
        case Some(t) =>
          Parser.constant(t)
        case None =>
          otherParser
      }
    }
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
  }

  def optional[T](
    name: String,
    parser: Parser[T],
    default: => T
  ): Parser[T] = {
    Parser.singleWordIfAny.onlyIf(_.exists(_ == name)).map(_.flatten)
      .mapFlatMap(_ => parser).orElse(Parser.constant(default))
  }
}
