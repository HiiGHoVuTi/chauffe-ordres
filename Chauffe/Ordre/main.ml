
(* ocamlopt -g -o test.exe main.ml *)
let () = Printexc.record_backtrace true ;;


let u0 = 5

let rec un =
  let m = Hashtbl.create 5000 in
  fun n ->
    if n < 0 then failwith "invalid un" else
    if n = 0 then u0 else
    match Hashtbl.find_opt m n with
      | Some k -> k
      | None -> 
        let r = (19999991 * un (n-1)) mod 19999999 in
        Hashtbl.replace m n r;
        r

type graph = int list array

let vertices = Array.mapi (fun i _ -> i)

let make_g n m =
  let g = Array.make n [] in
  for x = 0 to n - 1 do
    for y = 0 to n - 1 do
      if x <> y 
      && ((un (n+m+7*x+11*y)) mod 1000) < m
      then
        g.(x) <- y :: g.(x)   
    done
  done;
  g

let edge_count g =
  Array.fold_left (+) 0
  @@ Array.map List.length g

let make_c n m p =
  (make_g n m, fun v -> (un (5 * v)) mod p)

let control_sum (g, c) =
  edge_count g
  + Array.fold_left (+) 0
  (Array.mapi (fun v _ -> c v) g)

module IntSet = Set.Make(Int)

type partition = IntSet.t list

let initial_partition g =
  [IntSet.of_list
  @@ Array.to_list
  @@ vertices g]

let rec refine part n_out = match part with
  | [] -> []
  | ialpha :: rest ->
    let j = IntSet.inter n_out ialpha in
    let ialpha' = IntSet.diff ialpha j in
    if IntSet.is_empty ialpha'
      then j :: refine rest n_out
    else if IntSet.is_empty j
      then ialpha' :: refine rest n_out
      else j :: ialpha' :: refine rest n_out
  
let calc_lr g n m r =
  let part = ref (initial_partition g) in
  for i = 0 to r do
    let vi = (un (n + m + i)) mod n in
    part := refine !part 
      (IntSet.of_list g.(vi))
  done;
  !part


let calc_lr' n m r =
  List.length (calc_lr (make_g n m) n m r)

let partition_by_colour (g,c) p =
  let parr = Array.make p [] in 
  Array.iteri 
    (fun v _ -> parr.(c v) <- v :: parr.(c v)) g;
  Array.to_list (Array.map IntSet.of_list parr)

let equ_step (g,c) p part =
  List.iter begin fun vs ->
    let s = IntSet.fold 
            (fun v m -> IntSet.union g.(v) m)
            vs IntSet.empty in
    part := refine !part s
  end !part

let mirror_sets g =
  let g' = Array.make (Array.length g) [] in
  Array.iteri (fun x ys -> List.iter (fun y ->
    g'.(y) <- x :: g'.(y)
  ) ys) g;
  Array.map IntSet.of_list g'
  
let equ (g, c) p =
  let g' = mirror_sets g in
  let part = ref (partition_by_colour (g,c) p) in
  let ok = ref false in
  while not !ok do
    let part' = !part in
    equ_step (g', c) p part;
    ok := part' = !part
  done;
  !part

let show_results = 
  print_endline "--- TP ENS";
  Printf.printf "u0 = %d\n" u0;
  print_newline ();
  
  print_endline "--- 1 ---";
  print_int (un   50 mod 1000);
  print_newline ();
  print_int (un  100 mod 1000);
  print_newline ();
  print_int (un 1000 mod 1000);
  print_newline ();
  print_int (un 5000 mod 1000);
  print_newline ();
  
  print_endline "--- 2 ---";
  print_int (edge_count (make_g 100 100));
  print_newline ();
  print_int (edge_count (make_g 100 500));
  print_newline ();
  print_int (edge_count (make_g 1000 100));
  print_newline ();
  print_int (edge_count (make_g 1000 500));
  print_newline ();

  print_endline "--- 3 ---";
  print_int (control_sum (make_c 777 222 5));
  print_newline ();
  print_int (control_sum (make_c 777 500 25));
  print_newline ();
  print_int (control_sum (make_c 1234 222 25));
  print_newline ();
  print_int (control_sum (make_c 1234 500 75));
  print_newline ();

  print_endline "--- 4 ---";
  print_int (calc_lr' 111 222 5);
  print_newline ();
  print_int (calc_lr' 111 500 100);
  print_newline ();
  print_int (calc_lr' 1234 222 5);
  print_newline ();
  print_int (calc_lr' 1234 500 100);
  print_newline ();

  print_endline "--- 5 ---";
  let five n m p =
      List.length (equ (make_c n m p) p)
  in
  print_int (five 3000 1 4);
  print_newline ();
  print_int (five 3001 2 4);
  print_newline ();
  print_int (five 3002 900 200);
  print_newline ();
  print_int (five 3002 900 224);
  print_newline ();

  
  print_newline ();


