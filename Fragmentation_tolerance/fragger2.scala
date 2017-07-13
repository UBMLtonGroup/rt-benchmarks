import scala.collection.mutable.ArrayBuffer
import java.lang.OutOfMemoryError
import util.control.Breaks._

object fragger2 {

class X(var x: Long)

def allocateArray(i :Long) : Array[X] =
{
    var arr = new Array[X](1)
    arr(0) = new X(i)
    return arr
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

def fragmentArray(arrlist : ArrayBuffer[Array[X]])
{
   
    for(i<-0 to arrlist.length -1)
        {
            if(i %2 == 0)
                arrlist(i) = null
            
           // println(Runtime.getRuntime().freeMemory())
        }
    
}

def main(args: Array[String])
{
    var arrlist = new ArrayBuffer[Array[X]]()
    var i =0
  breakable{
        try
        {
            while(true)
            {
                arrlist += allocateArray(i)
                i = i+1
                //println(Runtime.getRuntime().freeMemory())
                println(i)
            }
        }
        catch
        {
            case ex: java.lang.OutOfMemoryError=>{
           // println("Out of memory")
            break
            }
    
        }
    }

    println("out of loop")


   /* //524287
    for(j<-0 to 16777215)
    {
        arrlist += allocateArray(i)
        
    }
    

    //println("out of loop")
    fragmentArray(arrlist)
    allocateArray2((16777216/2.5).toInt)
*/
}

}
