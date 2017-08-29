
fun computation (numThr, compDepth, iterations, compSleep, debug) =
let
    fun dbg x = (if (debug) then print (x^"\n") else ());

    val xx = ref 0

    fun fib n =
        let
            fun fib' (0,a,b) = a
              | fib' (n,a,b) = fib' (n-1,a+b,a)
        in
            fib' (n,0,1)
        end

    val rec delay =
       fn 0 => ()
        | n => (xx := n ; delay (n - 1))

    fun comp_func2 (tnum, i) = (
        dbg(" comp-iteration #" ^ Int.toString(i));
        starttime("comp", tnum, i);
        (* xx := fib(compDepth);*)
        delay(compSleep * 50);

        stoptime("comp", tnum, i);
        (*delay(compSleep * 10000);*)
        NPThread.yield();
    ())

    val rec comp_func =
        fn (_   , 0 ) => ()
         | (tnum, i') => (comp_func2 (tnum, i'); comp_func (tnum, (i'-1)))

    val rec start_comp_threads =
        fn 0 => ()
         | n => (dbg("comp-spawn #"^Int.toString(n)^"\n");
                 NPThread.spawn (fn () => comp_func (n, iterations));
                 start_comp_threads (n-1))
in
    dbg("computeDepth is " ^ Int.toString(compDepth));
    dbg("computeSleep is " ^ Int.toString(compSleep));
    start_comp_threads(numThr)
end