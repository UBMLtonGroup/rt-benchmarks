(* GC Pressure Test *)
val cmd = CommandLine.name ()
val args = CommandLine.arguments ()

val () =
  if List.length args = 0
  then let
       in
          print cmd ; print " [heap bytes]\n" ;
          ()
       end
  else
       print "ok\n"
