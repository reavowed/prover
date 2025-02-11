package net.prover.parsing

import net.prover.model._

case class KnownWordParser[+T](matchWord: String => Option[Parser[T]]) extends Parser[Option[T]] {
  def map[S](f: T => S): KnownWordParser[S] = KnownWordParser(s => matchWord(s).map(_.map(f)))
  private def attemptParseRequired(tokenStream: TokenStream): (T, TokenStream) = {
    (attemptParseWordRequired _).tupled.apply(tokenStream.readNext())
  }
  private def attemptParseWordRequired(word: String, tokenStreamAfterWord: TokenStream): (T, TokenStream) = {
    matchWord(word)
      .getOrElse(throw new ParseException("Invalid word " + word))
      .parse(tokenStreamAfterWord)
  }
  override def attemptParse(tokenStream: TokenStream): (Option[T], TokenStream) = {
    tokenStream.peek().flatMap((attemptParseWord _).tupled) match {
      case Some((t, tokenStream)) => (Some(t), tokenStream)
      case None => (None, tokenStream)
    }
  }
  private def attemptParseWord(word: String, tokenStreamAfterWord: TokenStream): Option[(T, TokenStream)] = {
    matchWord(word)
      .map(_.parse(tokenStreamAfterWord))
  }

  def required: Parser[T] = Parser(attemptParseRequired)

  def whileDefined(): Parser[List[T]] = Parser { tokenStream =>
    def getNext(acc: List[T], tokenStream: TokenStream): (List[T], TokenStream) = {
      attemptParse(tokenStream) match {
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
  def apply[T](word: String)(subsequentParser: Parser[T]): KnownWordParser[T] = {
    KnownWordParser(Option(_).filter(_ == word).map(_ => subsequentParser))
  }
  def select[T](parsers: Seq[KnownWordParser[T]]): KnownWordParser[T] = {
    KnownWordParser(word => parsers.findFirst(_.matchWord(word)))
  }
  def foldWhileDefined[A](acc: A)(f: A => KnownWordParser[A]): Parser[A] = Parser { tokenStream =>
    def getNext(acc: A, tokenStream: TokenStream): (A, TokenStream) = {
      f(acc).attemptParse(tokenStream) match {
        case (Some(acc), tokenStream) =>
          getNext(acc, tokenStream)
        case (None, tokenStream) =>
          (acc, tokenStream)
      }
    }
    getNext(acc, tokenStream)
  }
}
