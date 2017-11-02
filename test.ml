let () =
  let arr = [| 1.0; 2.0; 3.0 |] in
  let kSrc = CudaArray.generateKernel "(+ x (if (= 1.0 x) (+ 2.0 0.0) (+ x 0.0)))" in
  Printf.printf "kSrc:\n%s\n" kSrc;
  let () =
    CudaArray.mapFloatArrayInPlace
      kSrc
      arr
  in
  let _ = CudaArray.printFloatArray arr in
  ()