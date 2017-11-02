let () =
  let arr = [| 1.0; 2.0; 3.0 |] in
  let kSrc = CudaArray.generateKernelSource "(* 12.0 x)" in
  Printf.printf "kSrc:\n%s\n" kSrc;
  let () =
    CudaArray.mapFloatArrayInPlace
      kSrc
      arr
  in
  let _ = CudaArray.printFloatArray arr in
  ()