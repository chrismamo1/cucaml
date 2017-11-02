let () =
  Random.self_init();
  let arr = [| 1.0; 2.0; 3.0 |] in
  let arr = Array.init 100_000 (fun _ -> Random.float 100.0) in
  let kSrc = CudaArray.generateKernel "(if (>= x 1.99999) (* x x) (+ x 1.0))" in
  Printf.printf "kSrc:\n%s\n" kSrc;
  let cuStart = Unix.gettimeofday() in
  let () =
    CudaArray.mapFloatArrayInPlace
      kSrc
      arr
  in
  let cuEnd = Unix.gettimeofday() in
  let cuTime = cuEnd -. cuStart in
  let seStart = Unix.gettimeofday() in
  let () =
    Array.iteri
      (fun i x -> if x >= 1.99999 then arr.(i) <- x *. x else arr.(i) <- x +. 1.0)
      arr
  in
  let seEnd = Unix.gettimeofday() in
  let seTime = seEnd -. seStart in
  Printf.printf "CUDA time: %f\nSerial time: %f\n" cuTime seTime
  (*let _ = CudaArray.printFloatArray arr in ()*)