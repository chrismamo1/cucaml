a.out:
	ocamlc -c camlCuda.c
	ocamlopt CudaArray.ml camlCuda.o -cclib -lcuda