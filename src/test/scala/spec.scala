package bff

import org.specs._

object Spec extends Specification {
  "@#^*" should {
    "run hello world" in {
       val b = new scala.collection.mutable.ListBuffer[Char]
       @#^*.emptyin(c => b.append(c.toChar))(
         "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
       )
       b.mkString("") must_== "Hello World!\n"
    }
  }
}
