/**
use following options to run
scala -J-Xms1M -J-Xmx22529k fragger.scala
**/

object fragger{

def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
}

def allocateAndFragmentArray(kArraySize: Int)
{
    var arr = new Array[Option[Int]](kArraySize)

    for (i <- 0 to (arr.length - 1))
        {
            arr(i) = Some(2)
        }

    fragmentArray(arr)

  /**  for (x <- arr)
        {
            println(x)
        }
    **/

}


def fragmentArray(arr: Array[Option[Int]])
{
    for( i<-0 until (arr.length -1 ) by 2)
        {
            arr(i) = None
        }
    
}

def allocateArray(kArraySize :Int)
{
    println("allocating new array")
    var arr =time{ new Array[Option[Int]](kArraySize)}

    for (i <- 0 to (arr.length - 1))
        {
            arr(i) = Some(2)
        }
}

def main(args: Array[String]) {

var arraySize = 900000
allocateAndFragmentArray(arraySize)
allocateArray(arraySize/2)

}

}

