package bff

object @#^* {
  /** @param in - a string of instructions */
  def apply[T](in: String)(out: Int => T) =
    (List.fill(in.length)(0), in.toArray) match {
      case (cells, cmds) =>
        @annotation.tailrec def next(state: List[Int], ls: List[(Int, Int)], dp: Int, ip: Int): Unit =
          if(ip < cmds.size) {
            cmds(ip) match {
              case '<' => next(state, ls, dp - 1, ip + 1)
              case '>' => next(state, ls, dp + 1, ip + 1)
              case '+' => next(state.updated(dp, state(dp) + 1), ls, dp, ip + 1)
              case '-' => next(state.updated(dp, state(dp) - 1), ls, dp, ip + 1)
              case '.' => out(state(dp)); next(state, ls, dp, ip + 1)
              //case ',' => next()
              case '[' =>
                if(state(dp) == 0) next(state, ls.init, dp, ls.last._2 + 1)
                else next(state, ls :+ (ip, 0), dp, ip + 1)
              case ']' =>
                if(state(dp) != 0) next(state, ls.updated(ls.size - 1, (ls.last._1, ip)), dp, ls.last._1 + 1)
                else next(state, ls, dp, ip + 1)
              case _ => next(state, ls, dp, ip)
            }
          }
      next(cells, Nil, 0, 0)
    }
}
