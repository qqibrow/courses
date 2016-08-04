package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var left = 0; var right = 0;
    def balanceRecursive(chars : Array[Char]): Boolean = {
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

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(start: Int, end: Int) /*: ???*/ = {
      var min = 0;
      var total = 0;
      for(index <- start until end) {
        if(chars(index) == '(') total += 1
        if(chars(index) == ')') {
          total -= 1
          min = Math.min(total, min)
        }
      }
      (total, min)
    }

    def reduce(from: Int, until: Int):(Int, Int) = {
      if(until - from <= threshold) {
        traverse(from, until)
      } else {
        val mid = from + (until -from) / 2
        val (res1, res2) = parallel(reduce(from, mid), reduce(mid, until))
        (res1._1 + res2._1, Math.min(res1._2, res1._1 + res2._2))
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
