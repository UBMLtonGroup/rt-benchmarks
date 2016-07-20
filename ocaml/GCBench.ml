
type tree = Empty 
          | Node of node
and node = { left: tree ref; right: tree ref; i: int ; j: int }

let kStretchTreeDepth = 18;;
let kLongLivedTreeDepth = 16;;
let kArraySize          = 500000;;
let kMinTreeDepth       = 4;;
let kMaxTreeDepth       = 16;;


let rec treeHelper i = if i = 0 then 1 else 2 * treeHelper(i-1);;

let treeSize i = treeHelper(i+1) - 1;;


let numIters i = (2 * (treeSize kStretchTreeDepth)) / (treeSize i);;

let make_empty_node() = Node { left= ref Empty; right= ref Empty; i= 0; j= 0 }
let make_node (l, r) = Node { left= ref l; right= ref r; i= 0; j= 0 }

(*   
(* needs revision on this populate function *)
let rec populate iDepth node {left=lr; right=rr; i;j}  = 
   if iDepth <= 0 then (make_empty_node())
   else let iDepth = iDepth - 1 
		 in (
             lr := make_empty_node();
             rr := make_empty_node();
			 populate iDepth lr;
			 populate iDepth rr;
		  );;
*)

(* maybe also needs revision on this makeTree function *)
let rec makeTree iDepth =  if iDepth <= 0 then make_empty_node()
					   else make_node ( makeTree (iDepth - 1) , makeTree (iDepth - 1) )



let printDiagnostics () = ( 
	let lTotalMemory, 
		lFreeMemory = 8. *. ( (Gc.stat()).Gc.minor_words +. (Gc.stat()).Gc.major_words ), 
					  8. *. ( (Gc.stat()).Gc.minor_words +. (Gc.stat()).Gc.major_words -. (Gc.stat()).Gc.promoted_words )
	in
	(print_string(" Total memory available="^string_of_float(lTotalMemory)^" bytes");
	 print_string("  Free memory="^string_of_float(lFreeMemory)^" bytes\n") )

(*The total amount of memory allocated by the program since it was started is (in words) minor_words + major_words - promoted_words. Multiply by the word size (4 on a 32-bit machine, 8 on a 64-bit machine) to get the number of bytes.*)
(* Source:
http://caml.inria.fr/pub/docs/manual-ocaml/libref/Gc.html
*)

)


let timeConstruction depth = (
  let iNumIters = numIters depth
  in (
	  print_string("Creating " ^ string_of_int(iNumIters) ^ 
					" trees of depth " ^string_of_int(depth) ^ "\n");
	  let tStart = Sys.time()
(* needs revision on populate....
		   let rec loop i =
				if i < iNumIters then (  populate (depth, make_empty_node); loop (i+1) )
				else ()
			in (loop 0) *)
	  in ( print_string(  "\tTop down construction took "
				     	   ^ string_of_float( (Sys.time() -. tStart) *. 1000. )
					       ^ "msecs\n" )
	     );

      let tStart = Sys.time()

	  in (
			let rec loop i =
				if i < iNumIters then (makeTree depth; loop (i+1) )
				else ()
			in (
			  loop 0;
			 print_string(  "\tBottom up construction took "
			     		  ^ string_of_float( (Sys.time() -. tStart) *. 1000. )
				          ^ "msecs\n" )
			
			   )
		 )
     )
)


let main() = (
  print_string("Garbage Collector Test\n");

  print_string(" Stretching memory with a binary tree of depth " ^
			   string_of_int(kStretchTreeDepth) ^"\n" );
  printDiagnostics();

  let tStart = Sys.time()
  in ( 
		makeTree kStretchTreeDepth;
		print_string(" Creating a long-lived binary tree of depth " ^
		string_of_int(kLongLivedTreeDepth) ^"\n");
		let longLivedTree = make_empty_node()
		in (
      	   (* populate kLongLivedTreeDepth, longLivedTree;  *)

		   );


		print_string(" Creating a long-lived array of " ^
				  	   string_of_int(kArraySize) ^
					   " floats\n");

		let arr = Array.make 500000 0.0
		in ( 
		     print_float (Array.get arr 0); print_string("\n"); 
			 for i=0 to 500000/2 - 1 do ( Array.set arr i (1.0 /. float_of_int(i)) ) done 
		   );

		printDiagnostics();

		for i= kMinTreeDepth/2 to kMaxTreeDepth/2 do timeConstruction (i*2) done;

		if longLivedTree = Empty or (Array.get arr 1000) <> 1.0 /. 1000. then
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

)


let () = main()



