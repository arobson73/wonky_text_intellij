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
}
