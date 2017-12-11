let () =
  let ptxCode =
    {cucamlcl|
    (fun abs ((float x))
      (if (< x 0.0)
        (- 0.0 x)
        x))
    
    (fun square ((float x))
      (* x x))
  
    (fun kernel ((float x))
      (float guess)  (float step)
      (set step (set guess (/ x 2.0)))
      (while (> 0.001 (abs (- x (square guess))))
        (if (> (square guess) x)
          (set guess (- guess (set step (/ step 2.0))))
          (set guess (+ guess (set step (/ step 2.0))))))
      guess)
    |cucamlcl}
  in
  let ptxCode =
    {cucamlcl|
      (fun kernel ((float x))
      (int i)
      (float x2)
      (set x2 x)
      (while (< 10 i)
        (set i (+ 1 i))
        (set x2 (+ 1.0 x2)))
      x2)
    |cucamlcl}
  in
  Printf.printf "%s\n" ptxCode;