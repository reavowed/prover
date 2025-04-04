package net.prover.parsing

import net.prover.model._
import scalaz.Monad

import java.nio.file.Path
import scala.util.Try
import scala.util.control.NonFatal

trait Parser[+T] {
  def map[S](f: T => S): Parser[S] = Parser(tokenStream => attemptParse(tokenStream).mapLeft(f))
  def flatMap[S](f: T => Parser[S]): Parser[S] = Parser { tokenStream =>
    val (t, nextTokenStream) = attemptParse(tokenStream)
    f(t).parse(nextTokenStream)
  }
  def tryOrElse[S >: T](otherParser: => Parser[S]): Parser[S] = Parser { tokenStream =>
    Try(attemptParse(tokenStream)).toOption.getOrElse(otherParser.attemptParse(tokenStream))
  }
  def tryOrNone: Parser[Option[T]] = Parser { tokenStream =>
    Try(attemptParse(tokenStream).mapLeft(Some(_))).toOption.getOrElse((None, tokenStream))
  }

  private def inBrackets(openBracket: String, closeBracket: String): Parser[T] = {
    for {
      _ <- Parser.requiredWord(openBracket)
      t <- this
      _ <- Parser.requiredWord(closeBracket)
    } yield t
  }

  private def listInBrackets(openBracket: String, closeBracket: String, separatorOption: Option[String]): Parser[Seq[T]] = {
    Parser.inBrackets(Iterator.continually(this), openBracket, closeBracket, separatorOption, isInfinite = true)
  }

  def inParens: Parser[T] = inBrackets("(", ")")
  def listInParens(separatorOption: Option[String]): Parser[Seq[T]] = listInBrackets("(", ")", separatorOption)
  def optionalListInParens(separatorOption: Option[String]): Parser[Seq[T]] = {
    Parser.optionalWord("(").flatMap {
      case Some(_) =>
        Parser.allInOpenBrackets(Iterator.continually(this), ")", separatorOption, isInfinite = true)
      case None =>
        Parser.constant(Nil)
    }
  }

  def inBraces: Parser[T] = inBrackets("{", "}")

  def attemptParse(tokenStream: TokenStream): (T, TokenStream)
  def parse(tokenStream: TokenStream): (T, TokenStream) = {
    try {
      attemptParse(tokenStream)
    } catch {
      case e: ParseException if e.token.nonEmpty => throw e
      case e: ParseException => throw e.copy(token = Some(tokenStream.tokenForException))
      case NonFatal(e) => tokenStream.throwParseException(e.getMessage, Some(e))
    }
  }
  def parseFromString(str: String, description: String): T = {
    val (result, remainingTokenizer) = parse(Tokenizer.fromString(str, description))
    if (!remainingTokenizer.isEmpty)
      remainingTokenizer.throwParseException("Parsing finished before end of string")
    result
  }
  def parseFromFile(path: Path, description: String): T = {
    parse(Tokenizer.fromPath(path, description))._1
  }
  def parseAndDiscard(tokenStream: TokenStream): T = {
    parse(tokenStream)._1
  }
  def listInParensOrSingle(separatorOption: Option[String]): Parser[Seq[T]] = {
    listInParens(separatorOption).tryOrElse(map(Seq(_)))
  }
  def toEndOfFile: Parser[Seq[T]] = Parser { tokenStream =>
    def helper(parsed: Seq[T], currentTokenStream: TokenStream): (Seq[T], TokenStream) = {
      if (currentTokenStream.isEmpty) {
        (parsed, currentTokenStream)
      } else {
        val (next, nextTokenStream) = parse(currentTokenStream)
        helper(parsed :+ next, nextTokenStream)
      }
    }
    helper(Nil, tokenStream)
  }
}

object Parser {
  def apply[T](attemptParseFn: TokenStream => (T, TokenStream)): Parser[T] = {
    (tokenStream: TokenStream) => attemptParseFn(tokenStream)
  }

  def constant[T](t: T): Parser[T] = Parser { (t, _) }

  def toEndOfLine: Parser[String] = Parser { tokenStream => tokenStream.restOfLine() }

  def singleWord: Parser[String] = Parser { tokenStream => (tokenStream.currentToken.text, tokenStream.advance()) }

  def nWords(n: Int): Parser[Seq[String]] = (1 to n).foldLeft(Parser.constant(Seq.empty[String])) { case (parserSoFar, _) =>
    for {
      wordsSoFar <- parserSoFar
      newWord <- Parser.singleWord
    } yield wordsSoFar :+ newWord
  }

  def optionalWord(expectedWord: String): Parser[Option[Unit]] = selectOptionalWord {
    case `expectedWord` => ()
  }

  def selectWord[T](name: String)(f: PartialFunction[String, T]): Parser[T] = {
    Parser.singleWordIfAny.map {
      case Some(word) if f.isDefinedAt(word) => f(word)
      case Some(unknownWord) => throw new Exception(s"Expected $name but found '$unknownWord'")
      case None => throw new Exception(s"Expected $name but found end of file")
    }
  }

  def selectWordParser[T](name: String)(f: PartialFunction[String, Parser[T]]): Parser[T] = {
    Parser.singleWordIfAny.flatMap {
      case Some(word) if f.isDefinedAt(word) => f(word)
      case Some(unknownWord) => throw new Exception(s"Expected $name but found '$unknownWord'")
      case None => throw new Exception(s"Expected $name but found end of file")
    }
  }

  def selectOptionalWord[T](f: PartialFunction[String, T]): Parser[Option[T]] = Parser { tokenStream =>
    if (tokenStream.isEmpty) {
      (None, tokenStream)
    } else {
      if (f.isDefinedAt(tokenStream.currentToken.text))
        (Some(f(tokenStream.currentToken.text)), tokenStream.advance())
      else
        (None, tokenStream)
    }
  }

  def selectOptionalWordParser[T](f: PartialFunction[String, Parser[T]]): Parser[Option[T]] = Parser { tokenStream =>
    if (tokenStream.isEmpty) {
      (None, tokenStream)
    } else {
      if (f.isDefinedAt(tokenStream.currentToken.text))
        f(tokenStream.currentToken.text).parse(tokenStream.advance()).mapLeft(Some(_))
      else
        (None, tokenStream)
    }
  }

  def requiredWord(expectedWord: String): Parser[Unit] = Parser { tokenStream =>
    if (tokenStream.currentToken.text == expectedWord)
      ((), tokenStream.advance())
    else
      throw new Exception(s"Expected '$expectedWord' but found '${tokenStream.currentToken.text}'")
  }

  def singleWordIfAny: Parser[Option[String]] = Parser { tokenStream =>
    if (tokenStream.isEmpty) {
      (None, tokenStream)
    } else {
      (Some(tokenStream.currentToken.text), tokenStream.advance())
    }
  }

  private def allInOpenBrackets[T](parsers: Iterator[Parser[T]], closeBracket: String, separatorOption: Option[String], isInfinite: Boolean): Parser[Seq[T]] = {
    def parseNext(tokenStream: TokenStream, acc: Seq[T] = Nil): (Seq[T], TokenStream) = {
      if (tokenStream.currentToken.text == closeBracket) {
        (acc, tokenStream.advance())
      } else if (!parsers.hasNext) {
        throw new Exception("Expected close bracket at end of list")
      } else {
        val tokenStreamToUse = separatorOption match {
          case Some(separator) if acc.nonEmpty =>
            if (tokenStream.currentToken.text == separator)
              tokenStream.advance()
            else
              throw new Exception(s"Expected separator${if (isInfinite) " or close bracket" else ""} after list item")
          case _ =>
            tokenStream
        }
        val (next, remainingTokenStream) = parsers.next().parse(tokenStreamToUse)
        parseNext(remainingTokenStream, acc :+ next)
      }
    }
    Parser(parseNext(_, Nil))
  }

  private def inBrackets[T](parsers: Iterator[Parser[T]], openBracket: String, closeBracket: String, separatorOption: Option[String], isInfinite: Boolean): Parser[Seq[T]] = {
    for {
      _ <- Parser.requiredWord(openBracket)
      list <- allInOpenBrackets(parsers, closeBracket, separatorOption, isInfinite)
    } yield list
  }

  def int: Parser[Int] = Parser.singleWord.map(_.toInt)

  def allInParens: Parser[String] = Parser(_.untilCloseParen()).inParens
  def wordsInParens: Parser[Seq[String]] = allInParens.map(_.splitByWhitespace())

  implicit class OptionParserOps[T](parser: Option[Parser[T]]) {
    def traverse: Parser[Option[T]] = parser match {
      case Some(p) =>
        p.map(Some(_))
      case None =>
        Parser.constant(None)
    }
  }

  implicit class ParserOptionOps[T](parser: Parser[Option[T]]) {
    def mapMap[S](f: T => S): Parser[Option[S]] = parser.map(_.map(f))
    def flatMapMap[S](f: T => Parser[S]): Parser[Option[S]] = parser.flatMap {
      case Some(t) =>
        f(t).map(Some.apply)
      case None =>
        Parser.constant(None)
    }
    def flatMapFlatMapReverse[S](f: T => Option[Parser[S]]): Parser[Option[S]] = parser.flatMap {
      case Some(t) =>
        f(t).map(_.map(Some.apply)).getOrElse(Parser.constant(None))
      case None =>
        Parser.constant(None)
    }
    def orElse[S >: T](other: => Parser[S]): Parser[S] = Parser { tokenStream =>
      parser.parse(tokenStream) match {
        case (Some(t), nextTokenStream) =>
          (t, nextTokenStream)
        case (None, _) =>
          other.parse(tokenStream)
      }
    }
    def getOrElse[S >: T](other: => S): Parser[S] = Parser { tokenStream =>
      parser.parse(tokenStream) match {
        case (Some(t), nextTokenStream) =>
          (t, nextTokenStream)
        case (None, _) =>
          (other, tokenStream)
      }
    }
    def isDefined: Parser[Boolean] = parser.map(_.isDefined)
    def isUndefined: Parser[Boolean] = parser.map(_.isEmpty)
    def whileDefined: Parser[List[T]] = {
      Parser.whileDefined[T]((_, _) => parser)
    }
  }

  implicit class SeqParserOps[T](parsers: Seq[Parser[T]]) {
    def traverse: Parser[Seq[T]] = Parser { initialTokenStream =>
      parsers.foldLeft((Seq.empty[T], initialTokenStream)) { case ((valuesSoFar, tokenStream), parser) =>
        val (value, newTokenStream) = parser.parse(tokenStream)
        (valuesSoFar :+ value, newTokenStream)
      }
    }
    def inParens(separatorOption: Option[String]): Parser[Seq[T]] = {
      Parser.inBrackets(parsers.iterator, "(", ")", separatorOption, isInfinite = false)
    }
  }

  def optional[T](
    name: String,
    parser: Parser[T],
    default: => T
  ): Parser[T] = {
    Parser.optionalWord(name)
      .flatMapMap(_ => parser)
      .getOrElse(default)
  }

  def optional[T](
    name: String,
    parser: Parser[T]
  ): Parser[Option[T]] = {
    optional(name, parser.map(Some.apply), None)
  }

  def optional[T](f: String => Option[T]): Parser[Option[T]] = Parser { tokenStream =>
    if (tokenStream.isEmpty) {
      (None, tokenStream)
    } else {
      f(tokenStream.currentToken.text) match {
        case Some(t) =>
          (Some(t), tokenStream.advance())
        case None =>
          (None, tokenStream)
      }
    }
  }

  def required[T](
    name: String,
    parser: Parser[T]
  ): Parser[T] = {
    Parser.requiredWord(name)
      .flatMap(_ => parser)
  }

  def whileDefined[T](getParser: (List[T], Int) => Parser[Option[T]]): Parser[List[T]] = Parser { initialTokenStream =>
    def parseRemaining(valuesSoFar: List[T], currentIndex: Int, currentTokenStream: TokenStream): (List[T], TokenStream) = {
      val (newValueOption, newTokenStream) = getParser(valuesSoFar, currentIndex).parse(currentTokenStream)
      newValueOption match {
        case Some(newValue) =>
          parseRemaining(valuesSoFar :+ newValue, currentIndex + 1, newTokenStream)
        case None =>
          (valuesSoFar, currentTokenStream)
      }
    }
    parseRemaining(Nil, 0, initialTokenStream)
  }

  def foldWhileDefined[R](
    initial: R)(
    getParser: R => Parser[Option[R]]
  ): Parser[R] = {
    Parser { initialTokenStream =>
      def parseRemaining(currentAccumulator: R, currentTokenStream: TokenStream): (R, TokenStream) = {
        val (newAccumulatorOption, newTokenStream) = getParser(currentAccumulator).parse(currentTokenStream)
        newAccumulatorOption match {
          case Some(newAccumulator) =>
            parseRemaining(newAccumulator, newTokenStream)
          case None =>
            (currentAccumulator, currentTokenStream)
        }
      }
      parseRemaining(initial, initialTokenStream)
    }
  }

  def mapFoldWhileDefined[T, R](
    initial: R)(
    getParser: (Seq[T], R) => Parser[Option[(T, R)]]
  ): Parser[(Seq[T], R)] = {
    Parser { initialTokenStream =>
      def parseRemaining(currentAccumulator: R, valuesSoFar: Seq[T], currentTokenStream: TokenStream): ((Seq[T], R), TokenStream) = {
        val (newValueAndAccumulatorOption, newTokenStream) = getParser(valuesSoFar, currentAccumulator).parse(currentTokenStream)
        newValueAndAccumulatorOption match {
          case Some((newValue, newAccumulator)) =>
            parseRemaining(newAccumulator, valuesSoFar :+ newValue, newTokenStream)
          case None =>
            ((valuesSoFar, currentAccumulator), currentTokenStream)
        }
      }
      parseRemaining(initial, Nil, initialTokenStream)
    }
  }

  implicit val parserMonad: Monad[Parser] = new Monad[Parser] {
    override def point[A](a: => A): Parser[A] = Parser.constant(a)
    override def bind[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = fa.flatMap(f)
  }
}
