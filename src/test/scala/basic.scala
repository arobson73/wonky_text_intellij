import collection.mutable.Stack
import org.scalatest._
import Reference._
import Parsers._
class StackSpec extends FlatSpec {

  "many1(char(a))"should "return Left(1.1 a) for input of just \"b\"" in {
    val m = many1(char('a'))
    val r = Reference.run(m)("b")
    val s = r.toString
    assert(s.contains("1.1"))
    assert(s.contains("a"))
    assert(s.contains("Left"))
  }
  "many(char(a))" should "return Right(List()) for input of just \"b\"" in {
    val m = many(char('a'))
    val r = Reference.run(m)("b")
    val s = r.toString
    assert(s == "Right(List())")
  }
   "many(char(a))" should "return Right(List(a)) for input of just \"a\"" in {
    val m = many(char('a'))
    val r = Reference.run(m)("a")
    val s = r.toString
    assert(s == "Right(List(a))")
  }
  "many1(char(a))" should "return Right(List(a)) for input of just \"a\"" in {
    val m = many1(char('a'))
    val r = Reference.run(m)("a")
    val s = r.toString
    assert(s == "Right(List(a))")
  }
  "many1(char(a))" should "return Left(1.1 'a') for input of just \"\" " in {
    val m = many1(char('a'))
    val r = Reference.run(m)("")
    val s = r.toString
    assert(s.contains("1.1"))
    assert(s.contains("a"))
    assert(s.contains("Left"))
  }
    "many(char(a))" should "return Right(List()) for input of just \"\"" in {
    val m = many(char('a'))
    val r = Reference.run(m)("")
    val s = r.toString
    assert(s == "Right(List())")
  }
   "many(char(a))" should "return Right(List(a, a, a)) for input of just \"aaa\"" in {
    val m = many(char('a'))
    val r = Reference.run(m)("aaa")
    val s = r.toString
    assert(s == "Right(List(a, a, a))")
  }
    "many(char(a))" should "return Right(List(a, a)) for input of just \"aab\"" in {
    val m = many(char('a'))
    val r = Reference.run(m)("aab")
    val s = r.toString
    assert(s == "Right(List(a, a))")
  }
    "many1(char(a))" should "return Right(List(a, a)) for input of just \"aab\"" in {
    val m = many1(char('a'))
    val r = Reference.run(m)("aab")
    val s = r.toString
    assert(s == "Right(List(a, a))")
  }
  "ListOfN(3,char('a')) for input \"a\" " should "return \n Left(1.2 'a' \n\na\n ^)" in {
    val m = listOfN(3,char('a'))
    val r = Reference.run(m)("a")
    assert(r.toString.count(c => c == 'a') == 2)
    assert(r.toString.contains("Left"))
    assert(r.toString.contains("1.2"))
  }
  "ListOfN(3,char('a')) for input \"aa\" " should "return \n Left(1.3 'a' \n\naa\n  ^)" in {
    val m = listOfN(3,char('a'))
    val r = Reference.run(m)("aa")
    assert(r.toString.count(c => c == 'a') == 3)
    assert(r.toString.contains("Left"))
    assert(r.toString.contains("1.3"))
  }
  "ListOfN(3,char('a')) for input \"aaa\" " should "return Right(List(a, a, a))" in {
    val m = listOfN(3,char('a'))
    val r = Reference.run(m)("aaa")
    assert(r.toString == "Right(List(a, a, a))")
  }
  "product(char('a'),char('b')) for input \"ab\" " should "return Right((a,b))" in {
    val p = product(char('a'),char('b'))
    val r = Reference.run(p)("ab")
    assert(r.toString == "Right((a,b))")
  }
   "product(listOfN(4,char('a')),char('b')) for input \"aaaab\" " should "return Right((List(b, b, b, b),a))" in {
    val p = product(listOfN(4,char('a')),char('b'))
    val r = Reference.run(p)("aaaab")
    assert(r.toString == "Right((List(a, a, a, a),b))")
  }
  "map2(string(\"a\"),string(\"b\"))(_ + _) for input \"ab\" " should "return Right(ab)" in {
    val m = map2(string("a"),string("b"))((_ + _))
    val r = Reference.run(m)("ab")
    assert(r.toString == "Right(ab)")
  }
  "map(string(\"12\"))(_.toInt) for input \"12\" " should "return Right(12)" in {
    val p = map(string("12"))(_.toInt)
    val r = Reference.run(p)("12")
    assert(r.toString == "Right(12)" )
  }
  "label(\"hello\")(string(\"abc\")) for input \"abc\" " should "return Right(abc)" in {
    val lab = label("hello")(string("abc"))
    val r = Reference.run(lab)("abc")
    assert(r.toString == "Right(abc)")
  }
   "label(\"hello\")(string(\"abc\")) for input \"ab\" " should "return \nLeft(1.3 hello\n\nab\n  ^" in {
    val lab = label("hello")(string("abc"))
    val r = Reference.run(lab)("ab")
    assert(r.toString.contains("Left(1.3 hello\n\nab\n  ^)"))
  }
  "attempt(\"abc\") for input \"abc\" " should "return Right(abc)" in {
    val a = attempt(string("abc"))
    val r = Reference.run(a)("abc")
    assert(r.toString == "Right(abc)")
  }
  "attempt(\"abc\") for input \"a\" " should "return \nLeft(1.2 'abc'\n\na\n ^)" in {
    val a = attempt(string("abc"))
    val r = Reference.run(a)("a")
    assert(r.toString.contains("Left(1.2 'abc'\n\na\n ^)"))
  }
  "or(string(\"ab\"),string(\"cd\")) for input \"cd\"" should "return Right(cd)" in {
    val a = or(string("ab"),string("cd"))
    val r = Reference.run(a)("cd")
    assert(r.toString == "Right(cd)")
  }
  "or(string(\"ab\"),string(\"ac\")) for input \"ac\"" should "return \nLeft(1.2 'ab'\n\nac\n ^" in {
    val a = or(string("ab"),string("ac"))
    val r = Reference.run(a)("ac")
    assert(r.toString.contains("Left(1.2 'ab'\n\nac\n ^)"))
  }
  "flatMap(string(\"123\"))(pass andThen succeed) for input \"123\"" should "return Right(123123)" in {
    val pass = (s:String) => s+s // just append function
    val fm = flatMap(string("123"))(pass andThen Reference.succeed)
    val r = Reference.run(fm)("123")
    assert(r.toString == "Right(123123)")
  }
  //test slice
  "parser string(\"bcde\") with ParseState(Location(\"abcde\",1))" should "return Right(bcde)" in {
    val s0 = ReferenceTypes.ParseState(Location("abcde",1))
    val p = string("bcde")
    val r = p(s0).extract
    assert(r.toString == "Right(bcde)")
  }
  "slice(string(\"bcde\")) with input \"abcde\",1 " should "return Right(bcde) " in {
    val p = slice(string("bcde"))
    val r = runo(p)("abcde",1)
    assert(r.toString == "Right(bcde)")
  }
  "slice(string(\"bcde\")) with input \"abcdefgh\",1 " should "return Right(bcde) " in {
    val p = slice(string("bcde"))
    val r = runo(p)("abcdefgh",1)
    assert(r.toString == "Right(bcde)")
  }
  "skipR(string(\"abcde\"),string(\"fghijk\")) with input \"abcdefghijk\" " should "return Right(abcde)" in {
    val p = skipR(string("abcde"),string("fghijk"))
    val r = Reference.run(p)("abcdefghijk")
    assert(r.toString == "Right(abcde)")
  }
  "skipL(string(\"abcde\"),string(\"fghijk\")) with input \"abcdefghijk\" " should "return Right(fghijk)" in {
    val p = skipL(string("abcde"),string("fghijk"))
    val r = Reference.run(p)("abcdefghijk")
    assert(r.toString == "Right(fghijk)")
  }
  "skipL(string(\"abcde\"),string(\"fghijk\")) with input \"abcdefghij\" " should "return \nLeft(1.11 'fghijk')\n\nabcdefghij\n          ^" in {
    val p = skipL(string("abcde"),string("fghijk"))
    val r = Reference.run(p)("abcdefghij")
    assert(r.toString.contains("Left(1.11 'fghijk'\n\nabcdefghij\n          ^)"))
  }
  "skipR(string(\"abcde\"),string(\"fghijk\")) with input \"abcdefghij\" " should "return \nLeft(1.11 'fghijk')\n\nabcdefghij\n          ^" in {
    val p = skipR(string("abcde"),string("fghijk"))
    val r = Reference.run(p)("abcdefghij")
    assert(r.toString.contains("Left(1.11 'fghijk'\n\nabcdefghij\n          ^)"))
  }
  "token(string(\"abc\")) with input \"abc   \"" should "return Right(abc)" in {
    val p = token(string("abc"))
    val r = Reference.run(p)("abc   ")
    assert(r.toString == "Right(abc)")
  }
  "many(string(\"abc\")) with input \"abcabcabc\" " should "return Right(List(abc, abc, abc))" in {
    val p = many(string("abc"))
    val r = Reference.run(p)("abcabcabc")
    assert(r.toString == "Right(List(abc, abc, abc))")
  }
  "many(string(\"abc\")) with input \"ab\" " should "return Left(1.3 'abc'\n\nab\n  ^)" in {
    val p = many(string("abc"))
    val r = Reference.run(p)("ab")
    assert(r.toString.contains("Left(1.3 'abc'\n\nab\n  ^)"))
  }
  //this matches zero or more repetitions of p separated by p2 whose results are ignored
  "sep(string(\"abc\"),string(\"def\")) with input \"zzz\" " should "return Right(List())" in {
    val p = sep(string("abc"),string("def"))
    val r = Reference.run(p)("zzz")
    assert(r.toString =="Right(List())")
  }
   "sep(string(\"abc\"),string(\"def\")) with input \"abc\" " should "return Right(List(abc))" in {
    val p = sep(string("abc"),string("def"))
    val r = Reference.run(p)("abc")
    assert(r.toString =="Right(List(abc))")
  }
   "sep(string(\"abc\"),string(\"def\")) with input \"abcdef\" " should "return \nLeft(1.7 'abc'\n\nabcdef\n      ^" in {
    val p = sep(string("abc"),string("def"))
    val r = Reference.run(p)("abcdef")
    assert(r.toString.contains("Left(1.7 'abc'\n\nabcdef\n      ^)"))
  }
}