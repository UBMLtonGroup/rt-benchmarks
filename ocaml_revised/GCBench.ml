(*
Sun -
Two problems:
1. Tried to write up the code as similar as possible
   from GCBench.sml, but for some reason the building-up trees
   takes too much time
2. printing memory available (explained below )

*)


type tree = Empty
          | Node of node
            and node = { left: tree ref; right: tree ref; i: int; j: int}

let make_empty_node () =
  Node { left = ref Empty; right= ref Empty; i = 0 ; j = 0 }

let make_node (l, r) =
  Node { left  = ref l; right = ref r; i = 0 ; j = 0 }


(* Source:
http://caml.inria.fr/pub/docs/manual-ocaml/libref/Gc.html   *)
(* Sun: I wasn't able to find a method to represent
 "total memory available" by OCaml (Doesn't seem to have one in OCaml)
 Instead I found a way to represent  "total memory allocated"    *)
let printDiagnostics () = (
  let lTotalMemory =  8. *. ( (Gc.stat()).Gc.minor_words +. (Gc.stat()).Gc.major_words -.
                             (Gc.stat()).Gc.promoted_words )
  in
  (
      print_string(" Total memory allocated=" ^ string_of_float(lTotalMemory) ^ " bytes\n");
  )
)

let gcBench kStretchTreeDepth =

  let rec expt m n =
    if n = 0 then 1 else m * (expt m (n - 1)) in
  let treeSize i =
    (expt 2 (i + 1)) - 1 in
  let numIters i =
    (2 * (treeSize kStretchTreeDepth)) / (treeSize i) in

  let kLongLivedTreeDepth = kStretchTreeDepth - 2 in
  let kArraySize          = 4 * (treeSize kLongLivedTreeDepth) in
  let kMinTreeDepth       = 4 in
  let kMaxTreeDepth       = kLongLivedTreeDepth in

  let rec populate iDepth (Node {left = lr ; right = rr; i ; j}) =
    if iDepth <= 0 then false
    else let iDepth = iDepth - 1
         in
         (
              lr := make_empty_node();
              rr := make_empty_node();
			  (populate iDepth (!lr));
			  (populate iDepth (!rr))
		 )  in

  let rec makeTree iDepth =
     if iDepth <= 0
         then make_empty_node()
		 else make_node ( (makeTree (iDepth - 1)) ,
                          (makeTree (iDepth - 1)) )  in

  let timeConstruction depth =
    let iNumIters = numIters depth
    in (
	     print_string("Creating " ^ string_of_int(iNumIters) ^
					  " trees of depth " ^string_of_int(depth) ^ "\n");

	     let tStart = Sys.time() in
           let rec loop i =
               if i < iNumIters
                  then (  ( populate depth (make_empty_node()) ) ; loop (i + 1) )
			      else ()
		   in ( loop 0 );
           (
             let tElapsed = (Sys.time() -. tStart) in
		     print_string(  "\tTop down construction took "
			 	       	    ^ string_of_float( tElapsed *. 1000. )
					        ^ "msecs\n" )
           );
         let tStart = Sys.time() in
           let rec loop i =
               if i < iNumIters
                  then ( (makeTree depth) ; loop (i + 1) )
           in ( loop 0 );
           (
             let tElapsed = (Sys.time() -. tStart) in
			 print_string(  "\tBottom up construction took "
			     		    ^ string_of_float( tElapsed *. 1000. )
				            ^ "msecs\n" )
           )
	    ) in
  let main () = (
    print_string("Garbage Collector Test\n");
    print_string(" Stretching memory with a binary tree of depth " ^
			     string_of_int(kStretchTreeDepth) ^"\n" );
    printDiagnostics();
    let tStart = Sys.time()
    in (
          (makeTree kStretchTreeDepth);
          print_string(" Creating a long-lived binary tree of depth " ^
          string_of_int(kLongLivedTreeDepth) ^"\n");
          let longLivedTree = (make_empty_node())
          in (
              populate kLongLivedTreeDepth longLivedTree

             );
          print_string(" Creating a long-lived array of " ^
          string_of_int(kArraySize) ^ " floats\n");

          let arr = Array.make kArraySize 0.0
          in (
               print_float (Array.get arr 0); print_string("\n");
               for i = 0 to kArraySize/2 - 1 do ( Array.set arr i (1.0 /. float_of_int(i)) ) done
             );

          printDiagnostics();

          for i= kMinTreeDepth/2 to kMaxTreeDepth/2 do timeConstruction (i * 2) done;

          if longLivedTree = Empty || (Array.get arr 1000) <> 1.0 /. 1000. then
              print_string("Failed\n")
          else ();
          let tElapsed = Sys.time() -. tStart
          in (
               printDiagnostics();
               print_string( "Completed in " ^
                             string_of_float( tElapsed *. 1000.) ^
                             "ms.\n")

             )
       )
  ) in ( main () )


let () = gcBench 18






(**)
