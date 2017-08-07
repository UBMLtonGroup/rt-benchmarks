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

  def tail(l: Pair, n_ : Int) : Pair = {
    var l2 = l
    var n = n_

    while(n > 0) {
      n = n - 1
      l2 = l2.cdr()
    }

    return l2
  }

  def revloop(xx: Pair,  nn: Int,  yy: Pair): Pair = {
    var n  = nn
    var x2 = xx
    var y2 = yy

    while (n > 0) {
      y2 = new Pair(x2.car(), y2)
      x2 = x2.cdr()
      n -= 1
    }

    return y2
  }

  def F(n: Int) {
    x = revloop(x, n, tail(x, n))
    perms = new Perm(x, perms)
  }

  def P(n: Int) {
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
    while (tempN > 0) {
      f = tempN * f
      tempN = tempN - 1
    }
    f
  }

  def do_perm9(depth: Int, iterations: Int) {
    var n: Int = depth
    var m: Perm = null
    var m2: Perm = null
    var sum: Long = 0l
    var k: Int = 0

    var one_to_n: Pair = null
    var nn: Int = n

    while (nn > 0) {
      one_to_n = new Pair(nn, one_to_n)
      nn = nn - 1
    }

    m = Perm9.permutations(one_to_n)

    k = iterations
    while (k > 0) {
      val m2 = Perm9.permutations(one_to_n)
      m = m2
      k = k - 1
    }
    sum = Perm9.sumperms(m)
    if (sum != (n * (n + 1) * Perm9.factorial(n)) / 2)
      println("*** wrong result ***")
  }


  def start_gc_threads(num_threads : Int, p9iters: Int, p9depth: Int, iterations: Int, gcsleep: Int, debug: Boolean) {
    for(i <- 1 until num_threads+1) {
      if (debug) println(s"starting gc thread ${i}")

      val t = new Thread(new Runnable {
        def run() {
          var listofTimeStamps = gc_func(p9iters, p9depth, i, iterations, gcsleep, debug)
          for (timeStamp <- listofTimeStamps.toList){
            println(timeStamp)
          }
        }
      })
      t.setDaemon(false)
      t.start()
    }
  }

  def gc_func(p9iters: Int, p9depth: Int, id: Int, iterations: Int, comp_sleep: Int, debug: Boolean): ListBuffer[String] = {
    var listOfTimeStamps = new ListBuffer[String]()
    for(i <- 1 until iterations+1) {
      if (debug) println(s"gc iter ${i}")
      val tStart = System.currentTimeMillis()
      val runtime3 = Runtime.getRuntime
      listOfTimeStamps += ("gc:start:"+ id +":" + i + ":" +tStart  + ":" + (runtime3.totalMemory - runtime3.freeMemory))
      do_perm9(p9depth, p9iters)
      val tStop = System.currentTimeMillis()
      val runtime4 = Runtime.getRuntime
      listOfTimeStamps += ("gc:stop:"+ id +":" + i + ":" +tStop  + ":" + (runtime4.totalMemory - runtime4.freeMemory))
      Thread.sleep(comp_sleep * 1000)
    }
    listOfTimeStamps
  }



  def start_comp_threads(num_threads : Int, depth: Int, iterations: Int, comp_sleep: Int, debug: Boolean) {
    for(i <- 1 until num_threads+1) {
      if (debug) println(s"starting comp thread ${i}")
      val t = new Thread(new Runnable {
        def run() {
          var listofTimeStamps = comp_func(depth, i, iterations, comp_sleep, debug)
          for (timeStamp <- listofTimeStamps.toList){
            println(timeStamp)
          }
        }
      })
      t.setDaemon(false)
      t.start()
    }
  }

  def fibonacci(num: Int): Int = {
    if(num < 2){
      return 1
    } else {
      return fibonacci(num-2) + fibonacci(num-1)
    }
  }

  def comp_func(depth: Int, id: Int, iterations: Int, comp_sleep: Int, debug: Boolean): ListBuffer[String] = {
    var listOfTimeStamps = new ListBuffer[String]()
    for(i <- 1 until iterations+1) {
      if (debug) println(s"comp iter ${i}")
      val tStart = System.currentTimeMillis()
      val runtime3 = Runtime.getRuntime
      listOfTimeStamps += ("compute:start:"+ id +":" + i + ":" +tStart  + ":" + (runtime3.totalMemory - runtime3.freeMemory))
      fibonacci(depth)
      val tStop = System.currentTimeMillis()
      val runtime4 = Runtime.getRuntime
      listOfTimeStamps += ("compute:stop:"+ id +":" + i + ":" +tStop  + ":" + (runtime4.totalMemory - runtime4.freeMemory))
      Thread.sleep(comp_sleep * 1000)
    }
    listOfTimeStamps
  }



  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val computeThreads = opt[Int](default = Some(1), short = 't')
    val computeDepth = opt[Int](default = Some(37), short = 'd')
    val iterations = opt[Int](default = Some(1100), short = 'i')
    val computeSleep = opt[Int](default = Some(1), short = 's')

    val gcThreads = opt[Int](default = Some(1), short = 'g')
    val gcSleep = opt[Int](default = Some(1), short = 'S')
    val gcDelay = opt[Int](default = Some(60), short = 'J')
    val p9iters = opt[Int](default = Some(15), short = 'G')
    var p9depth = opt[Int](default = Some(9), short='p')

    var debug = opt[Boolean](default = Some(false), short='D')
    val help = opt[Boolean]()

    version("Scala Perm9 0.1.0")
    banner("""Usage: Perm9 [OPTION]
             |Options:
             |""".stripMargin)
    footer("\nhttps://ubmltongroup.github.io/")

    verify()
  }


  def main(args: Array[String]) {
    val conf = new Conf(args)
    if (conf.help()) {
      conf.printHelp();
    }
    else {
      if (conf.debug()) println("Start compute threads")
      start_comp_threads(conf.computeThreads(), conf.computeDepth(), conf.iterations(), conf.computeSleep(), conf.debug())
      Thread.sleep(conf.gcDelay() * 1000)
      if (conf.debug()) println("Start GC threads")
      start_gc_threads(conf.gcThreads(), conf.p9iters(), conf.p9depth(), conf.iterations(), conf.gcSleep(), conf.debug())
    }
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
