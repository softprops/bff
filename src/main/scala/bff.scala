package bff

object @#^* {
  def emptyin[T](out: Int => T) = apply(0, out)_

  def apply[T](in: => Int, out: Int => T)(src: String) =
    src.toArray match {
      case cmds =>
        @annotation.tailrec def next(cells: List[Int], ls: List[(Int, Int)], dp: Int, ip: Int): Unit =
          if(ip < cmds.size) {
            cmds(ip) match {
              case '<' => next(cells, ls, dp - 1, ip + 1)
              case '>' => next(cells, ls, dp + 1, ip + 1)
              case '+' => next(cells.updated(dp, cells(dp) + 1), ls, dp, ip + 1)
              case '-' => next(cells.updated(dp, cells(dp) - 1), ls, dp, ip + 1)
              case '.' => out(cells(dp)); next(cells, ls, dp, ip + 1)
              case ',' => next(cells.updated(dp, in), ls, dp, ip + 1)
              case '[' =>
                if(cells(dp) == 0) next(cells, ls.init, dp, ls.last._2 + 1)
                else next(cells, ls :+ (ip, 0), dp, ip + 1)
              case ']' =>
                if(cells(dp) != 0) next(cells, ls.updated(ls.size - 1, (ls.last._1, ip)), dp, ls.last._1 + 1)
                else next(cells, ls, dp, ip + 1)
              case _ => next(cells, ls, dp, ip)
            }
          }
        next(List.fill(30000)(0), Nil, 0, 0)
    }
}
