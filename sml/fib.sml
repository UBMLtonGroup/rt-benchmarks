
fun computation (numThr, compDepth, iterations, compSleep, debug, gcDelay, gcf) =
let
    fun dbg x = (if (debug) then print (x^"\n") else ());

    val xx = ref 0

    val cc = ref 0

    fun fib n =
        let
            fun fib' (0,a,b) = a
              | fib' (n,a,b) = fib' (n-1,a+b,a)
        in
            fib' (n,0,1)
        end

    val rec delay = (* note: only one thread for now *)
       fn 0 => ()
        | n => (xx := n ; delay (n - 1))

    fun comp_func2 (tnum, i) = (
        dbg(" comp-iteration #" ^ Int.toString(i));
        starttime("compute", tnum, i);
        (* xx := fib(compDepth);*)
        delay(compSleep);
        stoptime("compute", tnum, i);
        cc := !cc + 1;
        if gcDelay = !cc then gcf() else ();
        (*delay(compSleep * 10000);*)
        (*NPThread.yield();*)
    ())

    val rec comp_func =
        fn (_   , 0 ) => ()
         | (tnum, i') => (comp_func2 (tnum, i'); comp_func (tnum, (i'-1)))

    val rec start_comp_threads =
        fn 0 => ()
         | n => (dbg("comp-spawn #"^Int.toString(n)^"\n");
                 PThread.spawn (fn () => comp_func (n, iterations));
                 start_comp_threads (n-1))
in
    dbg("computeDepth is " ^ Int.toString(compDepth));
    dbg("computeSleep is " ^ Int.toString(compSleep));
    start_comp_threads(numThr)
end
