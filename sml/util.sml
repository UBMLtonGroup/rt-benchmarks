

fun nanotime () = IntInf.toString (Time.toMilliseconds (Time.now ()))

fun heaputil () = Int.toString(12345) (* XXX TODO *)

fun stoptime (typ, tid, iter) =
    print (typ ^ ":stop :" ^ Int.toString(tid) ^ ":" ^ Int.toString(iter) ^ ":" ^ (nanotime()) ^ ":" ^ (heaputil()) ^ "\n");

fun starttime (typ, tid, iter) =
    print (typ ^ ":start:" ^ Int.toString(tid) ^ ":" ^ Int.toString(iter) ^ ":" ^ (nanotime()) ^ ":" ^ (heaputil()) ^ "\n");

