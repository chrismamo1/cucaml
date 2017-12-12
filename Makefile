runtime/_build/camlCuda.o: runtime/camlCuda.c
	cd runtime && ocamlbuild -use-ocamlfind camlCuda.o -cflag '-ccopt=-I/usr/local/cuda/include/' && cd ..

runtime/_build/CudaArray.cmxa: runtime/_build/camlCuda.o runtime/CudaArray.ml
	cd runtime && ocamlbuild -use-ocamlfind CudaArray.cmxa && cd _build && ocamlmklib -failsafe -o CudaArray -ccopt -L/usr/lib/nvidia-384/ -ccopt -l:libnvidia-fatbinaryloader.so.384.90 camlCuda.o CudaArray.cmx -lcuda || cd .. && cd ..

ppx_cucaml.native:
	rebuild -use-ocamlfind -use-menhir -Is TinyLisp ppx_cucaml.native

_build/cucaml.cmxa:
	rebuild -use-ocamlfind -use-menhir -Is TinyLisp cucaml.cmxa

cucaml.cmx:
	rebuild -use-ocamlfind -use-menhir -I TinyLisp cucaml.cmx -lflag ../camlCuda.o
	cp _build/$@ ./$@

_build/cucaml.cmxs:
	rebuild -use-ocamlfind -use-menhir -I TinyLisp cucaml.cmxs # -lflag ../camlCuda.o

_build/cucaml.cma:
	rebuild -use-ocamlfind -Is TinyLisp -use-menhir -Is TinyLisp cucaml.cma

cucaml.a:
	rebuild -use-ocamlfind -Is TinyLisp cucaml.a

native-code: _build/cucaml.cmxa _build/cucaml.cmxs runtime/_build/CudaArray.cmxa
byte-code: _build/cucaml.cma

install: ppx_cucaml.native native-code # byte-code
	ocamlfind install cucaml `find ./_build -maxdepth 2 -type f` `find ./runtime/_build -maxdepth 2 -type f` ppx_cucaml.native META

#test.native:
#	rebuild -lflag ../lib/camlCuda.o -use-ocamlfind -use-menhir -Is TinyLisp test.native

test.native:
	rebuild -use-ocamlfind -Is TinyLisp -use-menhir test.native -lflag -cclib=-lcuda -lflag -I /usr/lib/nvidia-384/ -verbose 1 || true
	# cd _build && ocamlfind ocamlopt unix.cmxa CudaArray.o '-cclib=-lcuda' -linkpkg -I TinyLisp TinyLisp/BasicSExp.cmx TinyLisp/tinyLispParser.cmx TinyLisp/tinyLispLexer.cmx TinyLisp/TinyLisp.cmx ptx.cmx AsmGen.cmx CudaArray.cmx test.cmx -o test.native -ccopt '-L/usr/lib/nvidia-384' -verbose -ccopt '-l:libnvidia-fatbinaryloader.so.384.90' && cp test.native ../ && cd ..

clean:
	rm -f test.native cucaml.cmxa cucaml.a cucaml.cma
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