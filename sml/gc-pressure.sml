(* GC Pressure Test *)
val cmd = CommandLine.name ()
val args = CommandLine.arguments ()

fun intFromString(s) = case Int.fromString(s) of SOME x => x | NONE => 0;

open Array

fun test_max(s) = (array (s, 2) ; true) handle Size => false

fun make_element(elt) = 
let
   val x : int ref = ref elt
   val y : int = !x
in
	print (Int.toString(elt) ^ "\n");
  y
end

fun doit (b : int) = 
  case b of 
       0 => print "heap > 0 plz\n"
     | _ => if test_max(b) = true then (
               print("making array of size " ^ Int.toString(b) ^ "\n");
               tabulate(b, make_element);
               print("done\n")
            ) else 
               print("nope\n")
  

   
val () =
  if List.length args = 0
  then let
       in
          print cmd ; print " [heap bytes]\n" ;
          ()
       end
  else doit (intFromString(hd(args)))

