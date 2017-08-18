(* This benchmark aims to check the fragmentation tolerance of the GC.
* It first allocates a HUGE array of array options. 
* It then traverses the entire array to make sure that the array isn't optimized
* away by the compiler. 
* It then deallocates every alternate element in the aray. 
* Next it tries to allocate another array of exactly half the size of initial
* array and measures the cost of this second allocation.
* What we are interested in measuring is:
* 1. The amount of min heap required to complete this operation. 
* 2. The amount of time taken for the allocation as the size of the heap is
* gradually increased by a factor of 0.2.
* *)



fun timeIt f x = let
  val t0 = Time.now ()
  val result = f x
  val t1 = Time.now ()
in
  print(concat["It took ",Time.toString(Time.-(t1,t0))," seconds\n"]);
  result
end

fun timeIt3 f x = let
  val tmr = Timer.startRealTimer ()
  val result = f x
in
  print(concat["It took ",Time.toString(Timer.checkRealTimer tmr)," s\n"]);
  result
end

fun timeIt4 f x = let
  val tmr = Timer.startRealTimer ()
  val result = f x
in
  (concat["\nIt took ",Time.toString(Timer.checkRealTimer tmr)," s\n"],result)
end


fun timeIt2 f x = let 
  val tmr = Timer.startCPUTimer ()
  fun pr (msg, {usr,sys}) = print (concat [ msg,Time.toString
    usr,"u+",Time.toString sys,"s\n"]);
  val result = f x
  val {nongc,gc} = Timer.checkCPUTimes tmr
in 
  pr( "non-gc time = ", nongc);
  pr("gc time = ",gc);
  result
end

fun allocateNewArray kArraySize = 
let open Array
    val (allocTime,arr2) = timeIt4 array (kArraySize,SOME(array(1,0.0)))
(* Traverse array to keep it from being optimized away *)
   fun traverseArray j = 
     if j < (kArraySize)
     then (update (arr2,j, SOME(array(1,1.0)));
           (if (j mod 1000) =0 
           then print (Real.toString(sub(Option.valOf(sub(arr2,j)),0)) )
           else ());
     traverseArray (j+1))
     else ()
in
  traverseArray 0;
  print allocTime
end


fun doit kArraySize =
  let open Array
              val arr = array (kArraySize, SOME(array(1,0.0)))
              fun fragmentArray i =
                if i < (kArraySize div 2)
                  then (update (arr, i, NONE);
                        fragmentArray (i + 2))
                  else ()
              fun traverseWholeArray j = 
                if j < kArraySize
                    then (update (arr,j, SOME(array(1,1.0)));
                          (if (j mod 100000) =0 then 
                            print
                            (Real.toString(sub(Option.valOf(sub(arr,j)),0)) )
                            else ());
                          traverseWholeArray (j+1))
                else ()
          in
            traverseWholeArray 0;
            fragmentArray 0;
            allocateNewArray (kArraySize div 2)
          end


val _ = doit 90000000
