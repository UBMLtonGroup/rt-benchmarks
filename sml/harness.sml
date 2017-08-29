
fun usage (name, msg) =
let
    val help' = " -t  computeThreads  1\n" ^
                " -d  computeDepth    40\n" ^
                " -i  iterations      1100\n" ^
                " -s  computeSleep    1\n" ^
                " -g  gcThreads       1\n" ^
                " -S  gcSleep         1\n" ^
                " -J  gcDelay         60\n" ^
                " -G  treeDepth       18\n" ^
                " -D  debug           false\n" ^
                " -h  help            false\n\n" ^ msg ^ "\n\n"
in
   (TextIO.output (TextIO.stdErr,
                   concat ["usage: ", OS.Path.file name, "\n", help']);
    OS.Process.exit OS.Process.failure;
    ())
end

fun harness (name, arguments) =
let
    open GetOpt
    open Posix

    (*
     -t  computeThreads  1
     -d  computeDepth    40
     -i  iterations      1100
     -s  computeSleep    1
     -g  gcThreads       1
     -S  gcSleep         1
     -J  gcDelay         60
     -G  treeDepth       18
     -D  debug           false
     -h  help            false

      *)

    val computeThreads = ref 1
    val computeDepth   = ref 40
    val iterations     = ref 1100
    val computeSleep   = ref 1
    val gcThreads      = ref 1
    val gcSleep        = ref 1
    val gcDelay        = ref 60
    val treeDepth      = ref 18
    val debug          = ref false
    val help           = ref false

    val opts1 = [IntOpt #"t", IntOpt #"d", IntOpt #"i", IntOpt #"s",
                 IntOpt #"g", IntOpt #"S", IntOpt #"J", IntOpt #"G",
                 FlagOpt #"D", FlagOpt #"h"
                ]

    fun assignopt (Flag #"h")      = usage(CommandLine.name(), "help requested")
      | assignopt (Flag #"D")      = debug := true
      | assignopt (Flag x)         = usage(CommandLine.name(), "unknown flag opt matched")

      | assignopt (Int (#"t", i')) = computeThreads := i'
      | assignopt (Int (#"d", i')) = computeDepth := i'
      | assignopt (Int (#"i", i')) = iterations := i'
      | assignopt (Int (#"s", i')) = computeSleep := i'
      | assignopt (Int (#"g", i')) = gcThreads := i'
      | assignopt (Int (#"S", i')) = gcSleep := i'
      | assignopt (Int (#"J", i')) = gcDelay := i'
      | assignopt (Int (#"G", i')) = treeDepth := i'
      | assignopt (Int (_   , i')) = usage(CommandLine.name(), "unknown int opt matched")

      | assignopt (Str (_   , i')) = usage(CommandLine.name(), "string opt matched")

    val assignopt2 = fn (x : value, ks : value list) => ((assignopt x); x::ks)

    (*
     :: of 'a * 'a list

     ([Str (#"o", "arg"), Flag #"a"], ["path", "path"]))

    val args = getopt opts1 (List.::) [] arguments
    *)

    val args2 = getopt opts1 assignopt2 [] arguments

    fun dbg x = (if (!debug) then print (x^"\n") else ());


in (
    if !iterations < 1 then
        (usage (CommandLine.name(), "iterations < 1") ; OS.Process.failure)
    else (
        dbg "Start compute threads";
        computation(!computeThreads, !computeDepth, !iterations, !computeSleep, !debug);
        Posix.Process.sleep(Time.fromSeconds(IntInf.fromInt(!gcDelay)));
        dbg "Start GC threads";
        OS.Process.success
    )
) end

handle UnknownOption => (usage (CommandLine.name(), "unknown option raised") ; OS.Process.failure)


val _ = OS.Process.exit( harness(CommandLine.name(), CommandLine.arguments()) )
