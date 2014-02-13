CAMLP4=
PACK=protobuf
OCAMLDEP_OPTS=-package core,bitstring,bitstring.syntax -syntax camlp4o
OCAMLC_OPTS=$(OCAMLDEP_OPTS)
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

LIB_MODULES=fixed32.ml fixed64.ml \
	sint32.ml sint64.ml \
	varint.ml protocol.ml \
	builder.ml parser.ml

BYTE_TARGETS=protobuf.cma
NATIVE_TARGETS=protobuf.cmxa

.PHONY: all install test

all: native-code byte-code

install: all
	ocamlfind install protobuf META protobuf.cmxa protobuf.cma protobuf.a \
	protobuf.cmi $(native_cmx) $(native_cmi) $(sources_mli)

test:

# Dependencies
include ../Ocamlrules.mk.in
