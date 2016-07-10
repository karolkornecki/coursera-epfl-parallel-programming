package reductions

import common._
import org.scalameter._

import scala.collection.mutable

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

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
    def balance(chars: Array[Char], o: Int): Boolean = {
      if (chars.isEmpty) o == 0
      else if (o < 0) false
      else if (chars.head == '(') balance(chars.tail, o + 1)
      else if (chars.head == ')') balance(chars.tail, o - 1)
      else balance(chars.tail, o)
    }

    balance(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int): (Int, Int) = {
      val stack = mutable.Stack[Int]()
      for (i <- idx to until - 1) {
        if (chars(i) == '(') stack.push(1)
        else if (chars(i) == ')') {
          if (stack.nonEmpty && stack.top == 1) stack.pop()
          else stack.push(-1)
        }
      }
      val left = stack count (e => e == 1)
      val right = stack.size - left
      (left, right * -1)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) traverse(from, until)
      else {
        val mid = from + (until - from) / 2
        val (p1, p2) = parallel(reduce(from, mid), reduce(mid, until))
        (p1._1 + p2._1, p2._1 + p2._2)
      }
    }
    val (left, right) = reduce(0, chars.length)
    if (left < 0) false
    else left + right == 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
