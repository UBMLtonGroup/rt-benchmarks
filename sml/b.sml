val a = 1;

val b = "2";

val _ = print (Int.toString(a));
print "\n";


fun intFromString(s) = case Int.fromString(s) of SOME x => x | NONE => 0;

val c = intFromString(b);
val _ = print (Int.toString(c));
print "\n";
