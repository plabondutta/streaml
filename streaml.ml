(* Streaml — an OCaml port of the STREAM benchmark.
   Original C version: Copyright 1991-2013 John D. McCalpin.
   See LICENSE for full terms. *)
let stream_array_size =
  match Sys.getenv_opt "STREAM_N" with
  | Some s -> int_of_string s
  | None -> 60_000_000

let offset = 0
let ntimes = 10
let hline = "-------------------------------------------------------------\n"

let far_mask =
  match Sys.getenv_opt "STREAM_FAR" with Some s -> int_of_string s | None -> 0

let is_far bit = far_mask land (1 lsl bit) <> 0
let mk bit = Array.make ~far:(is_far bit) (stream_array_size + offset) 0.0
let a = mk 0
let b = mk 1
let c = mk 2
let avgtime = Array.make 4 0.0
let maxtime = Array.make 4 0.0
let mintime = Array.make 4 Float.max_float
let label = [| "Copy:      "; "Scale:     "; "Add:       "; "Triad:     " |]

let bytes =
  [|
    2.0 *. 8.0 *. float_of_int stream_array_size;
    2.0 *. 8.0 *. float_of_int stream_array_size;
    3.0 *. 8.0 *. float_of_int stream_array_size;
    3.0 *. 8.0 *. float_of_int stream_array_size;
  |]

let m = 20
let mysecond () = Unix.gettimeofday ()

let checktick () =
  let timesfound = Array.make m 0.0 in

  for i = 0 to m - 1 do
    let t1 = mysecond () in
    let t2 = ref t1 in
    while !t2 -. t1 < 1.0e-6 do
      t2 := mysecond ()
    done;
    timesfound.(i) <- !t2
  done;

  let delta = ref 0 in
  let minDelta = ref 1_000_000 in
  for i = 1 to m - 1 do
    delta := int_of_float (1.0e6 *. (timesfound.(i) -. timesfound.(i - 1)));
    minDelta := min !minDelta (max !delta 0)
  done;
  !minDelta

let checkSTREAMresults () =
  let aj = ref 1.0 in
  let bj = ref 2.0 in
  let cj = ref 0.0 in
  let scalar = 3.0 in
  aj := 2.0 *. !aj;

  for _ = 0 to ntimes - 1 do
    cj := !aj;
    bj := scalar *. !cj;
    cj := !aj +. !bj;
    aj := !bj +. (scalar *. !cj)
  done;

  let aSumErr = ref 0.0 in
  let bSumErr = ref 0.0 in
  let cSumErr = ref 0.0 in
  for j = 0 to stream_array_size - 1 do
    aSumErr := !aSumErr +. abs_float (a.(j) -. !aj);
    bSumErr := !bSumErr +. abs_float (b.(j) -. !bj);
    cSumErr := !cSumErr +. abs_float (c.(j) -. !cj)
  done;

  let aAvgErr = !aSumErr /. float_of_int stream_array_size in
  let bAvgErr = !bSumErr /. float_of_int stream_array_size in
  let cAvgErr = !cSumErr /. float_of_int stream_array_size in
  let epsilon = 1.3e-13 in

  let err = ref 0 in
  let ierr = ref 0 in
  if abs_float (aAvgErr /. !aj) > epsilon then begin
    err := !err + 1;
    Printf.printf
      "Failed Validation on array a[], AvgRelAbsErr > epsilon (%e)\n" epsilon;
    Printf.printf "     Expected Value: %e, AvgAbsErr: %e, AvgRelAbsErr: %e\n"
      !aj aAvgErr
      (abs_float aAvgErr /. !aj);
    Printf.printf "     AvgRelAbsErr > Epsilon (%e)\n" epsilon;
    ierr := 0;

    for j = 0 to stream_array_size - 1 do
      if abs_float ((a.(j) /. !aj) -. 1.0) > epsilon then ierr := !ierr + 1
    done;

    Printf.printf "     For array a[], %d errors were found.\n" !ierr
  end;

  if abs_float (bAvgErr /. !bj) > epsilon then begin
    err := !err + 1;
    Printf.printf
      "Failed Validation on array b[], AvgRelAbsErr > epsilon (%e)\n" epsilon;
    Printf.printf "     Expected Value: %e, AvgAbsErr: %e, AvgRelAbsErr: %e\n"
      !bj bAvgErr
      (abs_float bAvgErr /. !bj);
    Printf.printf "     AvgRelAbsErr > Epsilon (%e)\n" epsilon;
    ierr := 0;

    for j = 0 to stream_array_size - 1 do
      if abs_float ((b.(j) /. !bj) -. 1.0) > epsilon then ierr := !ierr + 1
    done;

    Printf.printf "     For array b[], %d errors were found.\n" !ierr
  end;

  if abs_float (cAvgErr /. !cj) > epsilon then begin
    err := !err + 1;
    Printf.printf
      "Failed Validation on array c[], AvgRelAbsErr > epsilon (%e)\n" epsilon;
    Printf.printf "     Expected Value: %e, AvgAbsErr: %e, AvgRelAbsErr: %e\n"
      !cj cAvgErr
      (abs_float cAvgErr /. !cj);
    Printf.printf "     AvgRelAbsErr > Epsilon (%e)\n" epsilon;
    ierr := 0;

    for j = 0 to stream_array_size - 1 do
      if abs_float ((c.(j) /. !cj) -. 1.0) > epsilon then ierr := !ierr + 1
    done;

    Printf.printf "     For array c[], %d errors were found.\n" !ierr
  end;

  if !err = 0 then
    Printf.printf
      "Solution Validates: avg error less than %e on all three arrays\n" epsilon;
  ()

let main () =
  Printf.printf "%s" hline;
  Printf.printf "STREAML version 1.0\n";

  let bytes_per_word = 8 in
  (* OCaml floats are guaranteed to be 8 bytes *)
  let times = Array.make_matrix 4 ntimes 0.0 in

  Printf.printf "This system uses %d bytes per array element.\n" bytes_per_word;
  Printf.printf "%s" hline;
  Printf.printf "Array size = %d (elements)\n" stream_array_size;
  let memory_per_array_mb =
    float_of_int bytes_per_word
    *. (float_of_int stream_array_size /. (1024.0 *. 1024.0))
  in
  let memory_per_array_gb =
    float_of_int bytes_per_word
    *. (float_of_int stream_array_size /. (1024.0 *. 1024.0 *. 1024.0))
  in
  Printf.printf "Memory per array = %.1f MiB (= %.1f GiB).\n"
    memory_per_array_mb memory_per_array_gb;
  Printf.printf "Each kernel will be executed %d times.\n" ntimes;
  Printf.printf
    " The *best* time for each kernel (excluding the first iteration)\n";
  Printf.printf " will be used to compute the reported bandwidth.\n";

  (* Get initial value for system clock. *)
  for j = 0 to stream_array_size - 1 do
    Array.unsafe_set a j 1.0;
    Array.unsafe_set b j 2.0;
    Array.unsafe_set c j 0.0
  done;

  Printf.printf "%s" hline;

  let quantum = ref (checktick ()) in
  if !quantum >= 1 then
    Printf.printf
      "Your clock granularity/precision appears to be %d microseconds.\n"
      !quantum
  else begin
    Printf.printf
      "Your clock granularity appears to be less than one microsecond.\n";
    quantum := 1
  end;

  let t = ref (mysecond ()) in

  for j = 0 to stream_array_size - 1 do
    Array.unsafe_set a j (Array.unsafe_get a j *. 2.0)
  done;
  t := 1.0e6 *. (mysecond () -. !t);

  Printf.printf
    "Each test below will take in the on the order of %d microseconds.\n"
    (int_of_float !t);
  Printf.printf "   (= %d clock ticks)\n"
    (int_of_float (!t /. float_of_int !quantum));
  Printf.printf "Increase the size of the arrays if this shows that\n";
  Printf.printf "you are not getting at least 20 clock ticks per test.\n";

  Printf.printf "%s" hline;

  Printf.printf "WARNING -- The above is only a rough guideline.\n";
  Printf.printf "For best results, please be sure you know the\n";
  Printf.printf "precision of your system timer.\n";
  Printf.printf "%s" hline;

  (* --- MAIN LOOP --- repeat test cases NTIMES times --- *)
  let scalar = 3.0 in
  for k = 0 to ntimes - 1 do
    Array.unsafe_set (Array.unsafe_get times 0) k (mysecond ());
    for j = 0 to stream_array_size - 1 do
      Array.unsafe_set c j (Array.unsafe_get a j)
    done;
    Array.unsafe_set (Array.unsafe_get times 0) k
      (mysecond () -. Array.unsafe_get (Array.unsafe_get times 0) k);

    Array.unsafe_set (Array.unsafe_get times 1) k (mysecond ());
    for j = 0 to stream_array_size - 1 do
      Array.unsafe_set b j (Array.unsafe_get c j *. scalar)
    done;
    Array.unsafe_set (Array.unsafe_get times 1) k
      (mysecond () -. Array.unsafe_get (Array.unsafe_get times 1) k);

    Array.unsafe_set (Array.unsafe_get times 2) k (mysecond ());
    for j = 0 to stream_array_size - 1 do
      Array.unsafe_set c j (Array.unsafe_get a j +. Array.unsafe_get b j)
    done;
    Array.unsafe_set (Array.unsafe_get times 2) k
      (mysecond () -. Array.unsafe_get (Array.unsafe_get times 2) k);

    Array.unsafe_set (Array.unsafe_get times 3) k (mysecond ());
    for j = 0 to stream_array_size - 1 do
      Array.unsafe_set a j
        (Array.unsafe_get b j +. (scalar *. Array.unsafe_get c j))
    done;
    Array.unsafe_set (Array.unsafe_get times 3) k
      (mysecond () -. Array.unsafe_get (Array.unsafe_get times 3) k)
  done;

  (*	--- SUMMARY --- *)
  for k = 1 to ntimes - 1 do
    for j = 0 to 3 do
      avgtime.(j) <- avgtime.(j) +. times.(j).(k);
      mintime.(j) <- min mintime.(j) times.(j).(k);
      maxtime.(j) <- max maxtime.(j) times.(j).(k)
    done
  done;

  Printf.printf
    "Function    Best Rate MB/s  Avg time     Min time     Max time\n";
  for j = 0 to 3 do
    avgtime.(j) <- avgtime.(j) /. float_of_int (ntimes - 1);

    Printf.printf "CSV,%d,%d,%s,%.1f,%.6f,%.6f,%.6f,%.6f\n" far_mask
      stream_array_size
      [| "copy"; "scale"; "add"; "triad" |].(j)
      (1.0e-06 *. bytes.(j) /. mintime.(j))
      avgtime.(j) mintime.(j) maxtime.(j)
      times.(j).(0);

    Printf.printf "%s%12.1f  %11.6f  %11.6f  %11.6f\n" label.(j)
      (1.0e-06 *. bytes.(j) /. mintime.(j))
      avgtime.(j) mintime.(j) maxtime.(j)
  done;

  checkSTREAMresults ();
  Printf.printf "%s\n" hline;
  ()

let () = main ()
