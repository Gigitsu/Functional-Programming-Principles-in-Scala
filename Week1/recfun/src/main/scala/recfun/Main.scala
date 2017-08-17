package recfun

object Main {
  def main(args: Array[String]) {
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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def innerBalance(chars: List[Char], open: Int): Boolean = {
      if (open < 0) false
      else if (chars.isEmpty) open == 0
      else if (chars.head == '(') innerBalance(chars.tail, open + 1)
      else if (chars.head == ')') innerBalance(chars.tail, open - 1)
      else innerBalance(chars.tail, open)
    }

    innerBalance(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def innerCountChange(money: Int, coins: List[Int]): Int = {
      if (money == 0 || coins.isEmpty || money - coins.head < 0) 0
      else if(money - coins.head == 0) 1
      else innerCountChange(money - coins.head, coins) + innerCountChange(money, coins.tail)
    }

    innerCountChange(money, coins.sorted)
  }
}
