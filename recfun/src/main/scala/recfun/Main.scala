package recfun

object Main {
  def main(args: Array[String]) {
    println(countChange(0, List(1, 2)))
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
    if(c > r) 0
    else {
      if(c == 0 || r == 0) 1
      else pascal(c, r-1) + pascal(c-1, r-1)
    }
  }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
    var left = 0; var right = 0;
    def balanceRecursive(chars : List[Char]): Boolean = {
      if(chars.isEmpty) left == right
      else {
        if(chars.head == '(') left += 1
        if(chars.head == ')') right += 1
        if(right > left) false
        else balanceRecursive(chars.tail)
      }
    }
    balanceRecursive(chars)
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeRecursive(money: Int, coins: List[Int]): Int = {
      if(money == 0) return 1
      else if(money < 0) throw new IllegalArgumentException("money must be larger than zero")
      else {
        coins.takeWhile(_ <= money).zipWithIndex.foldLeft(0) { case (accu, (coin, index)) =>
          accu +  countChangeRecursive(money - coin, coins.slice(index, coins.length))
        }
      }
    }
    def countChange2(money: Int, coins: List[Int]): Int = {
      if(money == 0) return 1
      else if(money < 0) return 0
      else {
        coins match {
          case Nil => 0
          case h::tail => countChange2(money - h, coins) + countChange2(money, tail)
        }
      }
    }
    if(money == 0) 0
    else countChange2(money, coins.sorted)

  }
  }
