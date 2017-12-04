let () =
  Random.self_init();
  let arr = Array.init 100_000_000 (fun _ -> Random.float 10.0) in
  let arr1 = Array.copy arr in
  let arr2 = Array.copy arr in
  let kSrc =
    CudaArray.generateKernel
      {| (sqrt (/
            ( * 4.0 ( * 3.14159265358979 ( * x x)))
            ( * (/ 4.0 3.0) ( * 3.14159265358979 ( * x ( * x x))))))
      |}
  in
  let arr' = [| 1.0; 2.0; 3.0 |] in
  let _ =
    CudaArray.mapFloatArrayInPlace
      kSrc
      arr'
  in
  Printf.printf "kSrc:\n%s\n" kSrc;
  let cuStart = Unix.gettimeofday() in
  let () =
    CudaArray.mapFloatArrayInPlace
      kSrc
      arr1
  in
  let cuEnd = Unix.gettimeofday() in
  let cuTime = cuEnd -. cuStart in
  let seStart = Unix.gettimeofday() in
  let () =
    for i = 0 to Array.length arr - 1 do
      let x = arr2.(i) in
      arr.(i) <- sqrt ((4.0 *. 3.14159265358979 *. x *. x) /. ((4.0 /. 3.0) *. 3.14159265358979 *. x *. x *. x))
    done
  in
  let seEnd = Unix.gettimeofday() in
  let seTime = seEnd -. seStart in
  Printf.printf "CUDA time: %f\nSerial time: %f\n" cuTime seTime;
  for i = 0 to Array.length arr - 1 do
    let diff = abs_float(arr1.(i) -. arr2.(i)) in
    if diff > 0.000001 then raise(Failure (Printf.sprintf "Arrays differ on element %d: %f, %f\n" i arr1.(i) arr2.(i)));
  done
  (*let _ = CudaArray.printFloatArray arr in ()*)