cucaml.cmxa:
	rebuild -use-ocamlfind -Is TinyLisp -use-menhir -Is TinyLisp cucaml.cmxa -lflag ../lib/camlCuda.o
	cp _build/$@ ./$@

cucaml.cmx:
	rebuild -use-ocamlfind -Is TinyLisp -use-menhir -Is TinyLisp cucaml.cmx -lflag ../lib/camlCuda.o
	cp _build/$@ ./$@

cucaml.cma:
	rebuild -use-ocamlfind -Is TinyLisp -use-menhir -Is TinyLisp cucaml.cma
	cp _build/$@ ./$@

native-code: cucaml.cmxa
byte-code: cucaml.cma

install: native-code
	ocamlfind install cucaml META cucaml.cmxa

#test.native:
#	rebuild -lflag ../lib/camlCuda.o -use-ocamlfind -use-menhir -Is TinyLisp test.native

test.native:
	rebuild -use-ocamlfind -Is TinyLisp -use-menhir test.native -lflag ../lib/camlCuda.o -lflag -cclib=-lcuda -lflag -I /usr/lib/nvidia-384/ -verbose 1 || true
	cd _build && ocamlfind ocamlopt unix.cmxa ../lib/camlCuda.o '-cclib=-lcuda' -linkpkg -I TinyLisp TinyLisp/BasicSExp.cmx TinyLisp/tinyLispParser.cmx TinyLisp/tinyLispLexer.cmx TinyLisp/TinyLisp.cmx ptx.cmx AsmGen.cmx CudaArray.cmx test.cmx -o test.native -ccopt '-L/usr/lib/nvidia-384' -verbose -ccopt '-l:libnvidia-fatbinaryloader.so.384.90' && cp test.native ../ && cd ..
clean:
	rebuild -clean

#native-code:
#	rebuild -Is TinyLisp -use-menhir -use-ocamlfind cudaArray.cmxa -lflag ../lib/camlCuda.o -lflag -cclib=-lcuda

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