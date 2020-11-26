package reductions

import scala.annotation._
import org.scalameter._

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
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    @tailrec
    def testBalance(cs:Array[Char], n: Int): Boolean = {
      if(n < 0) false
      else if(cs.isEmpty) n == 0
      else if(cs.head == '(') testBalance(cs.tail, n + 1)
      else if(cs.head == ')') testBalance(cs.tail, n - 1)
      else testBalance(cs.tail, n)
    }
    testBalance(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) : (Int, Int) = {
      var from = idx
      var acc = arg1
      var min = arg2
      while(from < until) {
        if(chars(idx) == '(') {
          acc = acc + 1
        } else if (chars(idx) == ')') {
          acc = acc - 1
          if(acc < min) min = acc
        }
        from = from + 1
      }
      (acc, min)
    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      if(until - from <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid = (from + until) / 2
        val ((acc1, min1), (acc2, min2)) = parallel(reduce(from, mid), reduce(mid, until))
        (acc1 + acc2, Math.min(min1, acc1 + min2))
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
