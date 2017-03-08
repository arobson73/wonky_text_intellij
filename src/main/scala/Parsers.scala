//package fpinscala.parsing

import java.util.regex._

import sun.util.resources.ParallelListResourceBundle
//import com.typesafe.scalalogging.LazyLogging
import scala.util.matching.Regex
//import fpinscala.testing._
//import fpinscala.testing.Prop._
import Gen._
import language.higherKinds
import language.implicitConversions
//not if i wanted to do a class specific log, then just
//put the import in the class itself. then you can have class specifc variations
//e.g macrologgers.{SimpleMacroLogger => ParLogger} for the Par class
//e.g macrologgers.{SimpleMacroLogger => LocLogger} for the Location class
import macrologgers.{SimpleMacroLogger => ParLogger}
//trait Parsers[Parser[+_]] extends LazyLogging { self => // so inner classes may call methods of trait
trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
  def runo[A](p: Parser[A])(input: String, off:Int): Either[ParseError,A]

  implicit def string(s: String): Parser[String] // note commenting this out has no effect
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)// can't comment this out. all those ops need it
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
    ParserOps[String] = ParserOps(f(a))

  def char(c: Char): Parser[Char] =
  {
    ParLogger.info("Parsers:char " + c)
    string(c.toString) map (_.charAt(0))
  }

  /*
   * A default `succeed` implementation in terms of `string` and `map`.
   * We leave `succeed` abstract, since `map` is defined below in terms of
   * `flatMap` and `succeed`, which would be a circular definition! But we include
   * the definition here in case implementations wish to use it
   * (say if they provide a custom implementation of `map`, breaking the cycle)
   */
  def defaultSucceed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def succeed[A](a: A): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]
  //strange one this. if both match (say parser was for char('a')
  // and the input was just "a", then you would think the result would be
  //"Right(List(a,a))"
  def many1[A](p: Parser[A]): Parser[List[A]] = {
    ParLogger.info(("Parsers:many1"))
    map2(p, many(p))(_ :: _)
  }

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n-1, p))(_ :: _)

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String]

  /*
  These can be implemented using a for-comprehension, which delegates to the `flatMap` and `map` implementations we've provided on `ParserOps`,
   or they can be implemented in terms of these functions directly.
  */
  //this is just like a match. if parser is (char('a'),char'a')) then this will return Right((a,a))
  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    flatMap(p)(a => map(p2)(b => (a,b)))
 //this will look like
  //p flatMap {case a => for(b <- p2
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = {
    ParLogger.info("Parsers:map2")
    for {a <- p; b <- p2} yield f(a, b)
  }
  //note if is used here , then it will succeed. if the input was erronous, then
  //f would not get as far as being called.
  def map[A,B](a: Parser[A])(f: A => B): Parser[B] =
    flatMap(a)(f andThen succeed)
  //this is just a label we can attach to the error message (assuming there is a parse error
  def label[A](msg: String)(p: Parser[A]): Parser[A]
  //TODO: come back to this
  def scope[A](msg: String)(p: Parser[A]): Parser[A]
  //just attempts the parser and puts it in uncommited state if failed.
  def attempt[A](p: Parser[A]): Parser[A]

  /** Sequences two parsers, ignoring the result of the first.
    * We wrap the ignored half in slice, since we don't care about its result. */
  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(slice(p), p2)((_,b) => b)

  /** Sequences two parsers, ignoring the result of the second.
    * We wrap the ignored half in slice, since we don't care about its result. */
  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a,b) => a)
//TODO:ditto
  def opt[A](p: Parser[A]): Parser[Option[A]] =
    p.map(Some(_)) or succeed(None)

  /** Parser which consumes zero or more whitespace characters. */
  //TODO: ditto
  def whitespace: Parser[String] = "\\s*".r

  /** Parser which consumes 1 or more digits. */
  //TODO: ditto
  def digits: Parser[String] = "\\d+".r

  /** Parser which consumes reluctantly until it encounters the given string. */
  //TODO: ditto
  def thru(s: String): Parser[String] = (".*?"+Pattern.quote(s)).r

  /** Unescaped string literals, like "foo" or "bar". */
  //TODO: ditto
  def quoted: Parser[String] = string("\"") *> thru("\"").map(_.dropRight(1))

  /** Unescaped or escaped string literals, like "An \n important \"Quotation\"" or "bar". */
  //TODO: ditto
  def escapedQuoted: Parser[String] =
    // rather annoying to write, left as an exercise
    // we'll just use quoted (unescaped literals) for now
    token(quoted label "string literal")

  /** C/Java style floating point literals, e.g .1, -1.0, 1e9, 1E-23, etc.
    * Result is left as a string to keep full precision
    */
  //TODO: ditto
  def doubleString: Parser[String] =
    token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  /** Floating point literals, converted to a `Double`. */
  //TODO: ditto
  def double: Parser[Double] =
    doubleString map (_.toDouble) label "double literal"

  /** Attempts `p` and strips trailing whitespace, usually used for the tokens of a grammar. */
  //ditto <* is skipR , so skipR(attempt(p),whitespace) is how its really called
  //think of this as parsing some input string like "abcd   " and also parsing the white space.
  //the white space is skipped / ignored, but its still taken into account in terms of
  //state update, so that when the next token is asked for it will not try to match whitespace
  //that it has already skipped.
  def token[A](p: Parser[A]): Parser[A] =
    attempt(p) <* whitespace

  /** Zero or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  //TODO: ditto
  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] = // use `Parser[Any]` since don't care about result type of separator
    sep1(p,p2) or succeed(List())

  /** One or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  //TODO: ditto
  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p, many(p2 *> p))(_ :: _)

  /** Parses a sequence of left-associative binary operators with the same precedence. */
  //TODO: ditto
  def opL[A](p: Parser[A])(op: Parser[(A,A) => A]): Parser[A] =
    map2(p, many(op ** p))((h,t) => t.foldLeft(h)((a,b) => b._1(a,b._2)))

  /** Wraps `p` in start/stop delimiters. */
  //TODO: ditto
  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) =
    start *> p <* stop

  /** A parser that succeeds when given empty input. */
  //TODO: ditto
  def eof: Parser[String] =
    regex("\\z".r).label("unexpected trailing characters")

  /** The root of the grammar, expects no further input following `p`. */
  //TODO: ditto
  def root[A](p: Parser[A]): Parser[A] =
    p <* eof

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2) // use `self` to explicitly disambiguate reference to the `or` method on the `trait`
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many = self.many(p)

    def slice: Parser[String] = self.slice(p)

    def **[B](p2: => Parser[B]): Parser[(A,B)] =
      self.product(p,p2)
    def product[B](p2: => Parser[B]): Parser[(A,B)] =
      self.product(p,p2)

    def flatMap[B](f: A => Parser[B]): Parser[B] =
      self.flatMap(p)(f)

    def label(msg: String): Parser[A] = self.label(msg)(p)

    def scope(msg: String): Parser[A] = self.scope(msg)(p)

    def *>[B](p2: => Parser[B]) = self.skipL(p, p2)
    def <*(p2: => Parser[Any]) = self.skipR(p, p2)
    def token = self.token(p)
    def sep(separator: Parser[Any]) = self.sep(p, separator)
    def sep1(separator: Parser[Any]) = self.sep1(p, separator)
    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)
    def opL(op: Parser[(A,A) => A]): Parser[A] = self.opL(p)(op)
  }
  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }
}

//case class Location(input: String, offset: Int = 0) extends LazyLogging  {
  case class Location(input: String, offset: Int = 0)  {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  ParLogger.info("Loc:line " + line)
  lazy val col = input.slice(0,offset+1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }
  ParLogger.info("Loc:col= "  + col )
  ParLogger.info("Loc:str= " + input)
  def toError(msg: String): ParseError = {
    ParseError(List((this, msg)))
  }
  //this return a new copy of Location
  def advanceBy(n: Int) =
  {
    val opn = offset + n
    ParLogger.info("Loc:AdvBy:off= " + opn)
    copy(offset = offset+n)
  }

  /* Returns the line corresponding to this location */
  def currentLine: String =
    //if (input.length > 1) input.lines.drop(line-1).next
    if (input.length > 0) input.lines.drop(line-1).next
    else ""

  def columnCaret = (" " * (col-1)) + "^"
}

case class ParseError(stack: List[(Location,String)] = List()) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc,msg) :: stack)

  def label[A](s: String): ParseError =
    ParseError(latestLoc.map((_,s)).toList)

  def latest: Option[(Location,String)] =
    stack.lastOption

  def latestLoc: Option[Location] =
    latest map (_._1)

  /**
  Display collapsed error stack - any adjacent stack elements with the
  same location are combined on one line. For the bottommost error, we
  display the full line, with a caret pointing to the column of the error.
  Example:

  1.1 file 'companies.json'; array
  5.1 object
  5.2 key-value
  5.10 ':'

  { "MSFT" ; 24,
  */
  override def toString =
    if (stack.isEmpty) "no error message"
    else {
      val collapsed = collapseStack(stack)
      val context =
        collapsed.lastOption.map("\n\n" + _._1.currentLine).getOrElse("") +
        collapsed.lastOption.map("\n" + _._1.columnCaret).getOrElse("")
      collapsed.map { case (loc,msg) => loc.line.toString + "." + loc.col + " " + msg }.mkString("\n") +
      context
    }

  /* Builds a collapsed version of the given error stack -
   * messages at the same location have their messages merged,
   * separated by semicolons */
  def collapseStack(s: List[(Location,String)]): List[(Location,String)] = {
    /*
    for (elem <- s) {
      val in = elem._1.input
      val col = elem._1.col
      val line = elem._1.line
      val off = elem._1.offset
      ParLogger.info(s"CollS input= $in, col=$col,line=$line,off=$off ")
      ParLogger.info(s"CollS: str = ${elem._2}")
    }*/
    s.groupBy(_._1).
      mapValues(_.map(_._2).mkString("; ")).
      toList.sortBy(_._1.offset)
  }
  def formatLoc(l: Location): String = l.line + "." + l.col
}

object Parsers {

}
