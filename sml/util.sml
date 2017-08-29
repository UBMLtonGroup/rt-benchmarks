

fun nanotime () = IntInf.toString (Time.toMilliseconds (Time.now ()))

fun heaputil () = Int.toString(12345) (* XXX TODO *)

fun stoptime (tid, iter) =
    print ("comp:stop:" ^ Int.toString(tid) ^ ":" ^ Int.toString(iter) ^ ":" ^ (nanotime()) ^ ":" ^ (heaputil()) ^ "\n");

fun starttime (tid, iter) =
    print ("comp:start:" ^ Int.toString(tid) ^ ":" ^ Int.toString(iter) ^ ":" ^ (nanotime()) ^ ":" ^ (heaputil()) ^ "\n");

