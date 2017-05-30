import java.io._
import scala.collection.mutable.ListBuffer

object GCBench {

    class Node(var left: Node, var right: Node) {
        def this() {
            this(null, null)
        }
    }

    val kStretchTreeDepth: Int = 18
    val kLongLivedTreeDepth: Int  = 16
    val kArraySize: Int  = 500000
    val kMinTreeDepth: Int = 4
    val kMaxTreeDepth: Int = 16
    
    // Nodes used by a tree of a given size
    def TreeSize(i : Int) : Int = {
        return ((1 << (i + 1)) - 1)
    }

    // Number of iterations to use for a given tree depth
    def NumIters(i : Int) : Int = {
        return 2 * TreeSize(kStretchTreeDepth) / TreeSize(i)
    }

    // Build tree top down, assigning to older objects.
    def Populate(iDepth : Int, thisNode : Node) {
    // var temp : Int = iDepth
        if (iDepth <= 0){
            return
        } else {
        //	temp -= 1
            thisNode.left = new Node()
            thisNode.right = new Node()
            Populate(iDepth - 1 , thisNode.left)
            Populate(iDepth - 1, thisNode.right)
        }
    }

    // Build tree bottom-up
    def MakeTree(iDepth : Int) : Node = {
        if(iDepth <= 0){
            return new Node()
        } else {
            return new Node(MakeTree(iDepth-1),MakeTree(iDepth-1))
        }
    }

    def PrintDiagnostics() {
        val lFreeMemory = Runtime.getRuntime().freeMemory()
        val lTotalMemory = Runtime.getRuntime().totalMemory()

        println(" Total memory available="
                + lTotalMemory + " bytes")
        println("  Free memory=" + lFreeMemory + " bytes")
    }

    def TimeConstruction(depth : Int){
        val root : Node = null
        var tStart: Long = 0l
        var tFinish: Long = 0l
        val iNumIters : Int = NumIters(depth)
        var tempTree : Node = null
        //println("Creating " + iNumIters +" trees of depth " + depth)
        tStart = System.currentTimeMillis()
        for( i <- 0 until iNumIters - 1){
            tempTree = new Node()
            Populate(depth, tempTree)
            tempTree = null;
        }
        tFinish = System.currentTimeMillis()
        //println("\tTop down construction took "+ (tFinish - tStart) + "msecs");
        tStart = System.currentTimeMillis()
        for( i <- 0 until iNumIters - 1){
            tempTree = MakeTree(depth)
            tempTree = null
        }
        tFinish = System.currentTimeMillis()
        //println("\tBottom up construction took "+ (tFinish - tStart) + "msecs");

    }

    def fibonacci(num: Int): Int = {
        if(num < 2){
            return 1
        } else {
            return fibonacci(num-2) + fibonacci(num-1)
        }
    }
    def start_gc_thread(num_threads : Int, tree_depth: Int, iterations: Int) {
        for(i <- 1 until num_threads+1){
            val t = new Thread(new Runnable {
                def run() {
                    var listofTimeStamps = gc_func(tree_depth, i, iterations)
                    for (timeStamp <- listofTimeStamps.toList){
                        println(timeStamp)
                    }
                }
            })
            t.setDaemon(false)
            t.start()
        }
    }

    def gc_func(tree_depth: Int, id: Int, iterations: Int): ListBuffer[String] = {
        var listOfTimeStamps = new ListBuffer[String]()
        for(i <- 1 until iterations+1){
            val start = System.currentTimeMillis()
            val runtime1 = Runtime.getRuntime
            listOfTimeStamps += "gc:start:"+ id +":" + i+ ":" +start + ":" + (runtime1.totalMemory - runtime1.freeMemory)
            val root: Node = null
            var longLivedTree: Node = null
            var tempTree: Node = null
            var tStart: Long = 0l
            var tFinish: Long = 0l
            var tElapsed: Long = 0l

            //println("Garbage Collector Test")
            //println(" Stretching memory with a binary tree of depth "+ kStretchTreeDepth)
            //PrintDiagnostics()
            tStart = System.currentTimeMillis()

            // Stretch the memory space quickly
            tempTree = MakeTree(kStretchTreeDepth)
            tempTree = null

            // Create a long lived object
            //println(" Creating a long-lived binary tree of depth " +kLongLivedTreeDepth)
            longLivedTree = new Node()
            Populate(kLongLivedTreeDepth, longLivedTree)

            // Create long-lived array, filling half of it
            //println(" Creating a long-lived array of "+ kArraySize + " doubles")
            var array = new Array[Double](kArraySize)
            for ( i <- 0 until kArraySize/2) {
                array(i) = 1.0/i
            }
            //PrintDiagnostics()

            var d : Int = kMinTreeDepth
            while(d <= kMaxTreeDepth){
                TimeConstruction(d)
                d += 2
            }

            if (longLivedTree == null || array(1000) != 1.0 / 1000) println("Failed")

            val runtime2 = Runtime.getRuntime
            tFinish = System.currentTimeMillis()
            tElapsed = tFinish-tStart
            //PrintDiagnostics()
            //println("Completed in " + tElapsed + "ms.")
            val stop = System.currentTimeMillis()
            listOfTimeStamps += "gc:stop:"+ id +":" + i + ":" + stop + ":" + (runtime2.totalMemory - runtime2.freeMemory)
        }
        listOfTimeStamps
    }

    def start_comp_threads(num_threads : Int, depth: Int, iterations: Int, comp_sleep: Int){
        for(i <- 1 until num_threads+1){
            val t = new Thread(new Runnable {
                def run() {
                    var listofTimeStamps = comp_func(depth, i, iterations, comp_sleep)
                    for (timeStamp <- listofTimeStamps.toList){
                        println(timeStamp)
                    }
                }
            })
            t.setDaemon(false)
            t.start()
        }
    }

    def comp_func(depth: Int, id: Int, iterations: Int, comp_sleep: Int): ListBuffer[String] = {
        var listOfTimeStamps = new ListBuffer[String]()
        for(i <- 1 until iterations+1){
            val tStart = System.currentTimeMillis()
            val runtime3 = Runtime.getRuntime
            listOfTimeStamps += ("compute:start:"+ id +":" + i + ":" +tStart  + ":" + (runtime3.totalMemory - runtime3.freeMemory))
            fibonacci(depth)
            val tStop = System.currentTimeMillis()
            val runtime4 = Runtime.getRuntime
            listOfTimeStamps += ("compute:stop:"+ id +":" + i + ":" +tStop  + ":" + (runtime4.totalMemory - runtime4.freeMemory))
            Thread.sleep(comp_sleep)
        }
        listOfTimeStamps
    }

    def main(args: Array[String]) {

        var computeThreads: Int = 1
        var computeDepth: Int = 37
        var iterations: Int = 1100
        var computeSleep: Int = 1000
        var gcThreads: Int = 1
        var treeDepth: Int = 15

        if(args.size >= 1){
            computeThreads = args(0).toInt
            computeDepth = args(1).toInt
            iterations = args(2).toInt
            computeSleep = args(3).toInt
            gcThreads = args(4).toInt
            treeDepth = args(5).toInt
        }
        start_comp_threads(computeThreads, computeDepth, iterations, computeSleep)
        Thread.sleep(30000)
        start_gc_thread(gcThreads, treeDepth, iterations)
    }
}
