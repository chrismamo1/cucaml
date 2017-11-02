test.native:
	rebuild -Is TinyLisp -use-menhir -use-ocamlfind test.native -lflag ../lib/camlCuda.o -lflag -cclib=-lcuda

a.out:
	ocamlc -c camlCuda.c
	ocamlopt CudaArray.ml camlCuda.o -cclib -lcuda

_bld/ptx.ml: ptx.re
	refmt --parse=re -p ml ptx.re > $@

_bld/AsmGen.ml: AsmGen.re
	refmt --parse=re -p ml AsmGen.re > $@

_bld/CudaArray.ml: CudaArray.ml
	cp CudaArray.ml _bld/CudaArray.ml

_bld/CudaArray.mli: CudaArray.mli
	cp CudaArray.mli _bld/CudaArray.mli

_bld/test.ml: test.ml
	cp test.ml _bld/test.ml

main: _bld/*.ml lib/camlCuda.o
	ocamlopt