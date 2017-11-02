let () =
  Random.self_init();
  let arr = Array.init 10_000_000 (fun _ -> Random.float 100.0) in
  let arr1 = Array.copy arr in
  let arr2 = Array.copy arr in
  let kSrc = CudaArray.generateKernel "(if (>= x 1.99999) (* x x) (+ x 1.0))" in
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
    Array.iteri
      (fun i x -> if x >= 1.99999 then arr.(i) <- x *. x else arr.(i) <- x +. 1.0)
      arr2
  in
  let seEnd = Unix.gettimeofday() in
  let seTime = seEnd -. seStart in
  Printf.printf "CUDA time: %f\nSerial time: %f\n" cuTime seTime;
  for i = 0 to Array.length arr - 1 do
    let diff = abs_float(arr1.(i) -. arr2.(i)) in
    if diff > 0.000001 then raise(Failure (Printf.sprintf "Arrays differ on element %d: %f, %f\n" i arr1.(i) arr2.(i)));
  done
  (*let _ = CudaArray.printFloatArray arr in ()*)