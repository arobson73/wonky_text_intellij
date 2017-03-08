import Reference._
import Parsers._

object TestApp {
  def main(args: Array[String]): Unit = {
    val a  = char('a')
    println(a)
    val b = many(a)
    var c = Reference.run(b)("a")
    println(c) //Right(List(a)
    //try the default Succeed
    val d = defaultSucceed[Char]('a') //notice this would fail to compile if i returned it to var
    println(d) //not much to see since it returns a function (ParserState => Result)
    //here we get a list of 3 if the input is 3 or more. Less than 3 then Left
    //will report the error.
    val e = listOfN(3,char('a'))

  }
}

