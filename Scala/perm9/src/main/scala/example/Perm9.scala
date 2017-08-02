import org.rogach.scallop._
import java.io._
import scala.collection.mutable.ListBuffer

class Pair(val n: Int, val y: Pair) {
  var hd: Int = n
  var tl: Pair = y

  def car() : Int = hd
  def cdr() : Pair = tl

  def length(): Int = {
    var n = 1 // account for self

    var x2 : Pair = tl
    while (x2 != null) {
      n = n + 1
      val y : Pair = x2.cdr()
      x2 = y
    }
    n
  }
}

class Perm(val n: Pair, val y: Perm) {
  var hd: Pair = n
  var tl: Perm = y

  def car() : Pair = hd
  def cdr() : Perm = tl
}

object Perm9 {
  // not mt safe
  private var perms: Perm = _

  private var x: Pair = _

  def tail(l: Pair, n: Int) : Pair = {
    var x = l
    var l2 = l
    while (x != null) {
      l2 = x
      x = l2.cdr()
    }
    return l2
  }

  def revloop(xx: Pair,  nn: Int,  yy: Pair): Pair = {
    var n  = nn
    var x2 = xx
    var y2 = yy

    while (n > -1) {
      println(s"     revloop: nn= ${nn} n= ${n}")
      //println(s"     revloop: x2 ${x2.hd} y2 ${y2.hd}")
      y2 = new Pair(x2.car(), y2)
      var jj = x2.cdr()
      x2 = jj
      n -= 1
    }

    return y2
  }

  def F(n: Int) {
    println(s" F(${n})")
    //println(s"  tail is ${x2.hd}")
    //println(s"   x before revloop ${x.hd}")
    x = revloop(x, n, tail(x, n))
    printints(x)
    //println(s"  x after ${x.hd}")
    perms = new Perm(x, perms)
  }

  def P(n: Int) {
    println(s"P(${n})")
    if (n > 1) {
      var j = n - 1
      while (j != 0) {
        P(n - 1)
        F(n)
        j -= 1
      }
      P(n - 1)
    }
  }

  def permutations(the_x: Pair): Perm = {
    x = the_x
    perms = new Perm(the_x, null)
    P(the_x.length())
    println("Perm done")
    return perms
  }

  def sumperms(x: Perm): Long = {
    var tempX = x
    var y: Pair = null
    var sum = 0
    while (tempX != null) {
      y = tempX.car()
      while (y != null) {
        sum = sum + y.car()
        y = y.cdr()
      }
      tempX = tempX.cdr()
    }
    sum
  }

  def factorial(n: Int): Long = {
    var tempN = n
    var f = 1
    while (n > 0) {
      f = tempN * f
      tempN = tempN - 1
    }
    f
  }

  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val computeThreads = opt[Int](default = Some(1), short = 't')
    val computeDepth = opt[Int](default = Some(6), short = 'd')
    val iterations = opt[Int](default = Some(1100), short = 'i')
    val computeSleep = opt[Int](default = Some(1), short = 's')
    val gcThreads = opt[Int](default = Some(1), short = 'g')
    val gcSleep = opt[Int](default = Some(1), short = 'S')
    val gcDelay = opt[Int](default = Some(1), short = 'J')
    val treeDepth = opt[Int](default = Some(15), short = 'G')
    val help = opt[Boolean]()

    version("Scala Perm9 0.1.0")
    banner("""Usage: Perm9 [OPTION]
             |Options:
             |""".stripMargin)
    footer("\nhttps://ubmltongroup.github.io/")

    verify()
  }

  def main(args: Array[String]) {
    var n: Int = 4
    var m: Perm = null
    var m2: Perm = null
    var sum: Long = 0l
    var k: Int = 0

    var one_to_n: Pair = null
    var nn: Int = n

    println("Create list of [1,2,...,n]: ")
    while (nn > 0) {
      one_to_n = new Pair(nn, one_to_n)
      nn = nn - 1
    }

    printints(one_to_n)

    println("Generate permutations of that list:")


    m = Perm9.permutations(one_to_n)
    println("Permutations >>>")
    printperms(m)
    println("<<< Permutations")

    k = 3
    while (k > 0) {
      println(k)
      val m2 = Perm9.permutations(one_to_n)
      m = m2
      k = k - 1
    }
    println("sumperms")
    sum = Perm9.sumperms(m)
    if (sum != (n * (n + 1) * Perm9.factorial(n)) / 2) println("*** wrong result ***")
  }

  /** ****************************************************************************
    *
    * Miscellaneous.
    *
    * *****************************************************************************/

  def printints(l: Pair) {
    var tempL = l
    print("(")
    while (tempL != null) {
      print(tempL.car())
      tempL = tempL.cdr()
      if (tempL != null) print(" ")
    }
    println(")")
  }

  def printperms(perms: Perm) {
    var temPerms = perms
    while (temPerms != null) {
      printints(temPerms.car())
      temPerms = temPerms.cdr()
    }
    println("")
  }

}
