import java.io._
import scala.collection.mutable.ListBuffer
import org.rogach.scallop._

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

    def simpleloop(num: Int): Int = {
        var accum = 0
        for (i <- 1 until num) {
            accum = i
        }
        accum
    }

    def fibonacci(num: Int): Int = {
        if(num < 2){
            return 1
        } else {
            return fibonacci(num-2) + fibonacci(num-1)
        }
    }

    def start_gc_thread(num_threads : Int, tree_depth: Int, iterations: Int, gcsleep: Int, debug: Boolean) {
        for (i <- 1 until num_threads+1) {
            if (debug) println(s"starting gc thread ${i}")

            val t = new Thread(new Runnable {
                def run() {
                    var listofTimeStamps = gc_func(tree_depth, i, iterations, gcsleep, debug)
                    for (timeStamp <- listofTimeStamps.toList){
                        println(timeStamp)
                    }
                }
            })
            t.setDaemon(false)
            t.start()
        }
    }

    def gc_func(tree_depth: Int, id: Int, iterations: Int, gcsleep: Int, debug: Boolean): ListBuffer[String] = {
        var listOfTimeStamps = new ListBuffer[String]()
        for (i <- 1 until iterations+1) {
            if (debug) println(s"gc iter ${i}")

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
            tempTree = MakeTree(tree_depth) //kStretchTreeDepth)
            tempTree = null

            // Create a long lived object
            //println(" Creating a long-lived binary tree of depth " +kLongLivedTreeDepth)
            longLivedTree = new Node()
            Populate(tree_depth, longLivedTree)

            // Create long-lived array, filling half of it
            //println(" Creating a long-lived array of "+ kArraySize + " doubles")
            var array = new Array[Double](kArraySize)
            for ( i <- 0 until kArraySize/2) {
                array(i) = 1.0/i
            }
            //PrintDiagnostics()

            var d : Int = kMinTreeDepth
            while(d <= tree_depth){
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

            Thread.sleep(gcsleep * 1000)
        }
        listOfTimeStamps
    }

    def start_comp_threads(num_threads : Int, depth: Int, iterations: Int, comp_sleep: Int, debug: Boolean){
        for (i <- 1 until num_threads+1) {
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

    def comp_func(depth: Int, id: Int, iterations: Int, comp_sleep: Int, debug: Boolean): ListBuffer[String] = {
        var listOfTimeStamps = new ListBuffer[String]()
        for (i <- 1 until iterations+1) {
            if (debug) println(s"comp iter ${i}")

            val tStart = System.currentTimeMillis()
            val runtime3 = Runtime.getRuntime
            listOfTimeStamps += ("compute:start:"+ id +":" + i + ":" +tStart  + ":" + (runtime3.totalMemory - runtime3.freeMemory))
            //fibonacci(depth)
            simpleloop(depth)
            val tStop = System.currentTimeMillis()
            val runtime4 = Runtime.getRuntime
            listOfTimeStamps += ("compute:stop:"+ id +":" + i + ":" +tStop  + ":" + (runtime4.totalMemory - runtime4.freeMemory))
            Thread.sleep(comp_sleep * 1000)
        }
        listOfTimeStamps
    }

    class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
        val computeThreads = opt[Int](default = Some(1), short = 't')
        val computeDepth = opt[Int](default = Some(40), short = 'd')
        val iterations = opt[Int](default = Some(1100), short = 'i')
        val computeSleep = opt[Int](default = Some(1), short = 's')
        val gcThreads = opt[Int](default = Some(1), short = 'g')
        val gcSleep = opt[Int](default = Some(1), short = 'S')
        val gcDelay = opt[Int](default = Some(60), short = 'J')
        val treeDepth = opt[Int](default = Some(15), short = 'G')

        var debug = opt[Boolean](default = Some(false), short='D')
        val help = opt[Boolean]()

        version("Scala GCBench 0.1.0")
        banner("""Usage: GCBench [OPTION]
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
            start_comp_threads(conf.computeThreads(), conf.computeDepth(), conf.iterations(),
                               conf.computeSleep(), conf.debug())

            Thread.sleep(conf.gcDelay() * 1000)

            if (conf.debug()) println("Start GC threads")
            start_gc_thread(conf.gcThreads(), conf.treeDepth(), conf.iterations(),
                            conf.gcSleep(), conf.debug())
        }
    }
}
