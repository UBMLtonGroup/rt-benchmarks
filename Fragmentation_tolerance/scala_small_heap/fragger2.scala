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
    var free = Runtime.getRuntime().freeMemory()
    var startTime =System.nanoTime /**System.currentTimeMillis()**/
    var arr = new Array[X](kArraySize)
    
    
    
    var endTime   = System.nanoTime /**System.currentTimeMillis()**/
    var totalTime = endTime - startTime
    println(totalTime+","+ free)
    return arr
}

def fragmentArray2(arrlist : ArrayBuffer[Array[X]])
{
   
    for(i<-0 to arrlist.length -1)
        {
                arrlist(i) = null
            
           // println(Runtime.getRuntime().freeMemory())
        }
    
}
def fragmentArray(arrlist : ArrayBuffer[Array[X]])
{
   
    for(i<-0 to arrlist.length -1)
        {
            if(i %2 == 0)
            {
              //var tmp= arrlist(i) 
              //tmp(0)= null
            arrlist(i) = null
            }
            
           // println(Runtime.getRuntime().freeMemory())
        }
    
}

def main(args: Array[String])
{
    var arrlist = new ArrayBuffer[Array[X]]()
    var i =0
/*  breakable{
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
 */

   //524287
   //16777215
    for(j<-0 to 524287)
    {
        arrlist += allocateArray(j)
        
    }
    
    //println("out of loop")
    fragmentArray(arrlist)
//    Thread.sleep(1000)
    //System.gc() 
    //var free = Runtime.getRuntime().freeMemory()
    //print("free space "+free)
    var arr2 = allocateArray2((524288*3.78).toInt)
    
    var refval = arrlist(10000)

    //var free = Runtime.getRuntime().freeMemory()
    //print("free space "+free)
    
}

}
