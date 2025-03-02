package net.prover.parsing

import net.prover.model._

case class KnownWordParser[+T](
  matchWord: String => Option[Parser[T]],
  expectedValueDescription: String
) extends Parser[T] {
  override def map[S](f: T => S): KnownWordParser[S] = copy(matchWord = s => matchWord(s).map(_.map(f)))
  override def flatMap[S](f: T => Parser[S]): KnownWordParser[S] = copy(matchWord = s => matchWord(s).map(_.flatMap(f)))
  override def attemptParse(tokenStream: TokenStream): (T, TokenStream) = {
    (attemptParseWordRequired _).tupled.apply(tokenStream.readNext())
  }
  private def attemptParseWordRequired(word: String, tokenStreamAfterWord: TokenStream): (T, TokenStream) = {
    matchWord(word)
      .getOrElse(throw new ParseException(s"Expected $expectedValueDescription, got $word"))
      .parse(tokenStreamAfterWord)
  }
  def attemptParseOption(tokenStream: TokenStream): (Option[T], TokenStream) = {
    tokenStream.peek().flatMap((attemptParseOptionWord _).tupled) match {
      case Some((t, tokenStream)) => (Some(t), tokenStream)
      case None => (None, tokenStream)
    }
  }
  private def attemptParseOptionWord(word: String, tokenStreamAfterWord: TokenStream): Option[(T, TokenStream)] = {
    matchWord(word)
      .map(_.parse(tokenStreamAfterWord))
  }

  def optional: Parser[Option[T]] = Parser(attemptParseOption)

  def whileDefined(): Parser[List[T]] = Parser { tokenStream =>
    def getNext(acc: List[T], tokenStream: TokenStream): (List[T], TokenStream) = {
      attemptParseOption(tokenStream) match {
        case (Some(t), tokenStream) =>
          getNext(acc :+ t, tokenStream)
        case (None, tokenStream) =>
          (acc, tokenStream)
      }
    }
    getNext(Nil, tokenStream)
  }
}

object KnownWordParser {
  def apply[T](word: String)(subsequentParser: => Parser[T]): KnownWordParser[T] = {
    KnownWordParser(Option(_).filter(_ == word).map(_ => subsequentParser), s"'$word''")
  }
  def select[T](parsers: Seq[KnownWordParser[T]]): KnownWordParser[T] = {
    KnownWordParser(word => parsers.findFirst(_.matchWord(word)), parsers.map(_.expectedValueDescription).mkString(" | "))
  }
  def foldWhileDefined[A](acc: A)(f: A => KnownWordParser[A]): Parser[A] = Parser { tokenStream =>
    def getNext(acc: A, tokenStream: TokenStream): (A, TokenStream) = {
      f(acc).attemptParseOption(tokenStream) match {
        case (Some(acc), tokenStream) =>
          getNext(acc, tokenStream)
        case (None, tokenStream) =>
          (acc, tokenStream)
      }
    }
    getNext(acc, tokenStream)
  }
  def mapFoldWhileDefined[A, T](acc: A)(f: A => KnownWordParser[(T, A)]): Parser[(Seq[T], A)] = Parser { tokenStream =>
    def getNext(acc: A, valuesSoFar: Seq[T], tokenStream: TokenStream): ((Seq[T], A), TokenStream) = {
      f(acc).attemptParseOption(tokenStream) match {
        case (Some((value, acc)), tokenStream) =>
          getNext(acc, valuesSoFar :+ value, tokenStream)
        case (None, tokenStream) =>
          ((valuesSoFar, acc), tokenStream)
      }
    }
    getNext(acc, Nil, tokenStream)
  }
}
