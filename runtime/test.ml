let () =
  Random.self_init();
  let nItems = ref 10_000_000 in
  let argSpecs = [
    "-n", Arg.Set_int(nItems), "How many elements to find the square root of."
    ]
  in
  let () = Arg.parse argSpecs print_endline "test [-n 1000000000]" in
  let arr = Array.init !nItems (fun n -> float_of_int (n + 1)) in
  let arr1 = Array.copy arr in
  let arr2 = Array.copy arr in
  let kSrc =
    {cucamlcl|
    (fun abs ((float x))
    (if
      (< x 0.0)
      (- 0.0 x)
      x))

  (fun kernel ((float x))
    (float x2)
    (set x2 x)
    (while (> 0.001 (abs (- 1.0 x2)))
      (set x2 (/ (+ x2 1.0) 2.0)))
    x2)
    |cucamlcl}
  in
  let kSrc =
    {cucamlcl|
      (fun square ((float x)) (* x x))

      (fun kernel ((float x))
        (int i)
        (set i 1)
        (while (< i 3)
          (set i (+ 1 i))
          (set x (square x)))
        x)
    |cucamlcl}
  in
  let kSrc =
    {cucamlcl|
    (fun abs ((float x))
      (if (< x 0.0)
        (- 0.0 x)
        x))
    
    (fun square ((float x))
      (* x x))

    (fun kernel ((float x))
      (float guess)  (float step)
      (set guess x)
      (int i)
      (set i 0)
      (set step (/ x 2.0))
      (while (< 0.001 (abs (- x (square guess))))
        (set i (+ 1 i))
        (if (> (square guess) x)
          (set guess (- guess step))
          (set guess (+ guess step)))
        (set step (/ step 2.0)))
      guess)
    |cucamlcl}
  in

  let arr' = [| 1.0; 2.0; 3.0 |] in
  let _ =
    CudaArray.mapFloatArrayInPlace
      kSrc
      arr'
  in
  let cuStart = Unix.gettimeofday() in
  let () =
    CudaArray.mapFloatArrayInPlace
      kSrc
      arr1
  in
  let cuEnd = Unix.gettimeofday() in
  let cuTime = cuEnd -. cuStart in
  let () = Printf.printf "CUDA time: %f\n" cuTime in
  let () = flush_all() in
  let seStart = Unix.gettimeofday() in
  let () =
    for i = 0 to Array.length arr - 1 do
      let guess = ref (arr2.(i)) in
      let step = ref (!guess /. 2.0) in
      while abs_float(arr2.(i) -. (!guess *. !guess)) > 0.001 do
        (*arr.(i) <- sqrt ((4.0 *. 3.14159265358979 *. x *. x) /. ((4.0 /. 3.0) *. 3.14159265358979 *. x *. x *. x))*)
        if !guess *. !guess > arr2.(i)
        then begin
          guess := !guess -. !step;
          step := !step /. 2.0;
        end
        else begin
          guess := !guess +. !step;
          step := !step /. 2.0;
        end
      done;
      arr2.(i) <- !guess;
    done
  in
  let seEnd = Unix.gettimeofday() in
  let seTime = seEnd -. seStart in
  Printf.printf "Serial time: %f\n" seTime;
  for i = 0 to Array.length arr - 1 do
    let diff = abs_float(arr1.(i) -. arr2.(i)) in
    if diff > 0.01 then raise(Failure (Printf.sprintf "Arrays differ on element %d: %f, %f\n" i arr1.(i) arr2.(i)));
  done;
  (*Printf.printf "CUDA-modded array: \n";
  CudaArray.printFloatArray arr1;
  Printf.printf "Serially-modded array: \n";
  CudaArray.printFloatArray arr2;*)
  Printf.printf "\nArrays %s" "match!\n"