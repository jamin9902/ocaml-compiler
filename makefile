all: miniml evaluation

miniml: miniml.ml
	ocamlbuild -use-ocamlfind miniml.byte

evaluation: evaluation.ml
	ocamlbuild -use-ocamlfind evaluation.byte

expr: expr.ml
	ocamlbuild -use-ocamlfind expr.byte

bignum: bignum.ml
	ocamlbuild -use-ocamlfind bignum.byte
