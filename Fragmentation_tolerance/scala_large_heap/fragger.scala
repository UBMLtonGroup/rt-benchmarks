/**
use following options to run
scala -J-Xms1M -J-Xmx2097153 -J-XX:-UseGCOverheadLimit fragger.scala
 for i in 1033895940 1033995940 1034095940 1034195940 1034295940 1034395940 1034495940 1034595940 1034695940 1034795940 1034895940 1034995940;do taskset 0x1 scala -J-Xms$i -J-Xmx$i -J-Xloggc:gc -J-XX:+PrintGCDetails -J-XshowSettings:vm -J-XX:+PrintGCTimeStamps -J-XX:+PrintCommandLineFlags -J-XX:-UseGCOverheadLimit fragger.scala ;done
**/

object fragger{
class X(var x: Int)

def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block    // call-by-name
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0)/100000 + "ms")
    result
}

def allocateArray(kArraySize: Int) : Array[X] =
{
    var arr = new Array[X](kArraySize)

    for (i <- 0 to (arr.length - 1))
        {
            arr(i) = new X(i)
        }
    return arr

  /**  for (x <- arr)
        {
            println(x)
        }
    **/

}

def traverseArray(arr :Array[X])
{
    for (i <- 0 to (arr.length - 1))
        {
            arr(i).x = i+1
        }
}


def fragmentArray(arr: Array[X])
{
    for( i<-0 until (arr.length -1 ) by 2)
        {
            arr(i) = null
        }
    
}

def allocateArray2(kArraySize :Int): Array[X] =
{
var startTime =System.nanoTime /**System.currentTimeMillis()**/
    var arr = new Array[X](kArraySize)
var endTime   = System.nanoTime /**System.currentTimeMillis()**/
var totalTime = endTime - startTime
println(totalTime)
    for (i <- 0 to (arr.length - 1))
        {
             arr(i) = new X(i + (kArraySize *5))
        }
    return arr
}

def main(args: Array[String]) {

var startTime = System.currentTimeMillis()
var arraySize = 50000000
/**println("allocating large array")**/
var arr = allocateArray(arraySize)

traverseArray(arr)
fragmentArray(arr)

/**println("allocating new array")**/
var arr2 = allocateArray2(400000) 
traverseArray(arr2)
var endTime   = System.currentTimeMillis()
var totalTime = endTime - startTime
/**println("Total time= "+totalTime+" ms")**/
}

}

