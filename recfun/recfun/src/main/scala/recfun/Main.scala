package recfun

object Main extends App{{
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = if (r < c || c < 0 || r < 0) 0
    else if (c == 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def updateState(c: Char, state: Int) =
      if (c == '(') state+1
      else if (c == ')') state-1 else state

    def myBalance(chars: List[Char], count: Int): Boolean = {
      if (chars.isEmpty) count == 0
      else { if (count < 0) false
      else myBalance(chars.tail, updateState(chars.head, count))
      }
    }
    myBalance(chars, 0)
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)

}