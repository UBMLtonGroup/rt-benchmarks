

let permutations x0 =
    let x = ref x0 in
    let perms = ref ([x0]) in
    let rec p n =
        if n > 1 then let rec loop j =
                        if j = 0 then p (n - 1)
                                   else ( p ( n - 1);
                                          f n;
                                          loop (j - 1) )
                        in loop (n - 1)
        else ()
    and f n =
      ( x := (revloop !x n (list_tail !x n ));
        perms := ((!x) :: !perms)
      )
    and revloop x n y =
        if n = 0
          then y
          else (  revloop (List.tl(x)) (n - 1) ((List.hd(x))::y)  )
    and list_tail x n =
        if n = 0
          then x
        else list_tail (List.tl(x)) (n - 1)
    in (   p (List.length(!x))   ;  !perms  ) ;;


let sumlists x =
    let rec loop1 x sum =
        if x = []
            then sum
        else
            let rec loop2 y sum =
                if y = []
                    then sum
                    else loop2 (List.tl(y)) ( sum + List.hd(y))
            in loop1 (List.tl(x)) (loop2 (List.hd(x)) sum )
    in loop1 x 0 ;;


let perms : int list list ref = ref [] ;;


let one2n n =
    let rec loop n p =
        if n = 0 then p
        else ( loop (n - 1) (n::p) )
    in loop n [] ;;



let perm9_benchmark m (n : int) =
    let rec factorial n =
        if n = 1
            then 1
            else n * factorial (n - 1)
    in (
            (String.concat "" [ string_of_int(5); "perm" ; string_of_int(9) ]) , 1 ,
                    ( fun () ->
                       (   perms := permutations (one2n 1) ;
    					   let rec loop m =
    					 	  if m = 0
                                 then !perms
    					 	  else ( perms := permutations ( List.hd( !perms ) );
    								 loop (m - 1) )
    					   in loop m
        		       )
                    ) ,
                    ( fun (result) ->
                         (sumlists result)
							= (n * (n + 1) * (factorial n)) / 2
                    )
        ) ;;


let main () = perm9_benchmark 5 9 ;;


module Main =
struct
    let testit out =
        let (_,_,f,t) = main ()
        in output_string out (if t (f () ) then "OK\n" else "Fail\n")

	let doit () =
        let (_,n,f,_) = main () in ( f() )

        (*
        let rec loop n =
            if n = 0 then ()
            else ( f() ; loop (n - 1) )  (* produced warning 10 type unit *)
        in loop n
        *)

end
;;


Main.doit();;

print_string("Done\n");;





(**)
