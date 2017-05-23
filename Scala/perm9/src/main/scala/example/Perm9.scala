class Pair(val n: Int, val y: Pair) {
  var hd: Int = n
  var tl: Pair = y
}

object Pair {

  def cons(n: Int, y: Pair): Pair = new Pair(n, y)

  def car(x: Pair): Int = x.hd

  def cdr(x: Pair): Pair = x.tl

  def length(x: Pair): Int = {
    var n = 0
    while (x != null) {
      n = n + 1
      lazy val x: Pair = Pair.cdr(x)
    }
    n
  }
}

class Perm(val n: Pair, val y: Perm){
  var hd: Pair = n
  var tl: Perm = y
}

object Perm {

  def cons(p: Pair, y: Perm): Perm = new Perm(p, y)

    def car(x: Perm): Pair = x.hd

    def cdr(x: Perm): Perm = x.tl
}

object Perm9 {

  private var perms: Perm = _

  private var x: Pair = _

  def revloop(x: Pair, n: Int, y: Pair): Pair = {
    var tempN: Int = n - 1
    while (tempN != 0) {
      lazy val y: Pair = Pair.cons(Pair.car(x), y)
      lazy val x: Pair = Pair.cdr(x)
      tempN -= 1
    }
    return y
  }

  def tail(l: Pair, n: Int): Pair = {
    var tempN = n - 1
    while (tempN > 0) {
      lazy val l: Pair = Pair.cdr(l)
      tempN -= 1
    }
    return l
  }

  def F(n: Int) {
    lazy val x: Pair = revloop(x, n, tail(x, n))
    perms = Perm.cons(x, perms)
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
    var x = the_x
    val perms = Perm.cons(the_x, null)
    P(Pair.length(the_x))
    return perms
  }

  def sumperms(x: Perm): Long = {
    var tempX = x
    var y: Pair = null
    var sum = 0
    while (tempX != null) {
      y = Perm.car(tempX)
    while (y != null) {
      sum = sum + Pair.car(y)
      y = Pair.cdr(y)
    }
    tempX = Perm.cdr(tempX)
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


  def main(args: Array[String]) {
    var n: Int = 9
    var m: Perm = null
    var m2: Perm = null
    var sum: Long = 0l
    var k: Int = 0

    var one_to_n: Pair = null
    var nn: Int = n
    while(nn > 0){
      nn = nn - 1
      one_to_n = Pair.cons(nn, one_to_n)
    }

    m = Perm9.permutations(one_to_n)
    // printperms(m)

    k = 5
    while(k > 0){
      val m2 = Perm9.permutations(one_to_n)
      m = m2
      k = k - 1
    }

    sum = Perm9.sumperms(m)
    if(sum != (n * (n + 1) * Perm9.factorial (n)) / 2) println("*** wrong result ***")
  }

/******************************************************************************
*
*   Miscellaneous.
*
******************************************************************************/

def printints(l: Pair) {
  var tempL = l
  println("(")
  while(tempL != null) {
    println(Pair.car(l))
    tempL = Pair.cdr(l)
    if(tempL != null) println(" ")
  }
  println(")")
}

def printperms(perms: Perm) {
  var temPerms = perms
  while(temPerms != null){
    printints(Perm.car(temPerms))
    temPerms = Perm.cdr(temPerms)
  }
  println("")
}

}
