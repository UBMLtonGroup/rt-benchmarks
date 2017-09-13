(* this benchmark allocates single element array options till heap is full and
* deallocates half of them *)
fun timeIt4 f x = let
  val tmr = Timer.startRealTimer ()
  val result = f x
in
  (Real.toString((Time.toReal(Timer.checkRealTimer tmr)) *1000.0)^"\n",result)
end


fun heapstats () =
  let
    open MLton.GC
  in
    IntInf.toString(Statistics.bytesAllocated ())
  end   


fun doit kArraySize =
  let open Array
            fun allocateArrays i ls kArraySize  =
                if i < kArraySize
                then 
                    let
                        val arr  = SOME(array(10,i))
                    in
                        (*print (Int.toString(i)^ "\n");*)
                        update(ls,i, arr);
                        allocateArrays (i+1) ls kArraySize
                    end
                else ls 
             
            fun allocateNewArray size = 
                let
                    val (alloctime,arr2) = timeIt4 array(size,10)
                    fun traverseArray j = 
                            if j < (size)
                            then ((if (j mod 1000) =0 
                                    then update(arr2, j, 11)
                                    else ());
                                   traverseArray (j+1))
                            else ()
                in
                    traverseArray 0;
                    print alloctime;
                    arr2
                end

            
            fun fillHeap kArraySize =
              let
                val largeArray : int array option array = array(kArraySize,NONE)
              in
                allocateArrays 0 largeArray kArraySize 
              end


            val ls = fillHeap kArraySize

            fun fragmentHeap arr i =
                if i < kArraySize 
                  then (update (arr, i, NONE);
                        fragmentHeap arr (i + 2))
                  else ()

            fun traverseArray ls i = 
              if (i < (length ls))
                then (sub (ls,i);
                     traverseArray ls (i+1))
                else ()
                              
  in
    let open MLton.GC
    in
      (*print ("done\n");*)
       fragmentHeap ls 0 ;
        print(heapstats () ^"\n");
       (* print(Int.toString(length ls)^ "\n");*)
        allocateNewArray (990000);
        print(Int.toString(sub(Option.valOf(sub(ls,10001)),0)));
        traverseArray ls 0
        (* traverseList ls*)
    end
  end

(* 433259 elements*)
val _ = doit 500000 (*(Option.valOf(Int.fromString(hd (CommandLine.arguments()))))*)
