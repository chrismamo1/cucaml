(*external printFloatArray : (float array -> int -> float) = "printFloatArray"*)
external eMapFloatArrayInPlace : (float array -> int -> string -> float) = "mapFloatArrayInPlace"

(*let map(f, arr) = {

};*)

let generateKernel s =
  let rval = AsmGen.compileProgram s [] in
  rval

let kernelSource = ".version\t5.0\n.target\tsm_20\n.address_size\t64\n.visible .entry\tmyTestKernel(\n\t.param\t.u64\tparamX,\n\t.param\t.u64\tsz\n)\n{\n\t.reg\t.f64\t%fd0;\n\t.reg\t.u32\t%r0;\n\t.reg\t.u32\t%r1;\n\t.reg\t.u32\t%r2;\n\t.reg\t.u32\t%r3;\n\t.reg\t.u64\t%rd0;\n\t.reg\t.u64\t%rd1;\n\t.reg\t.u64\t%rd2;\n\t.reg\t.u64\t%rd3;\n\tld.param.u64\t%rd0,[paramX];\n\tmov.u32\t%r0,%ntid.x;\n\tmov.u32\t%r1,%ctaid.x;\n\tmov.u32\t%r2,%tid.x;\n\tmad.lo.u32\t%r3,%r0,%r1,%r2;\n\tmul.wide.u32\t%rd3,%r3,8;\n\tcvta.to.global.u64\t%rd1,%rd0;\n\tadd.u64\t%rd2,%rd1,%rd3;\n\tcvt.rz.f64.u32\t%fd0,%r3;\n\tst.global.f64\t[%rd2],69.000000000;\n}"

let printFloatArray arr =
  Array.iteri (fun i x -> Printf.printf "arr[%d] = %f\n" i x) arr

let mapFloatArrayInPlace code arr =
  let st = eMapFloatArrayInPlace arr (Array.length arr) code in
  let _ = Printf.printf "Return value: %f\n" st in
  ()

(*let () =
  let arr = [| 5.0; 4.0; 3.0 |] in
  let x = eMapFloatArrayInPlace arr 3 kernelSource in
  print_float x;
  printFloatArray arr;
  print_newline()*)