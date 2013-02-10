CAMLP4=
OCAMLC_OPTS=-package core,bitstring,bitstring.syntax -syntax camlp4o
OCAMLOPT_OPTS=-for-pack Protobuf $(OCAMLC_OPTS)

cources=varint.ml protocol.ml

sources=$(cources)

sources_mli=$(cources:%.ml=%.mli)

native_cmx=$(sources:%.ml=%.cmx)

byte_cmo=$(sources:%.ml=%.cmo)

native_cmi=$(sources_mli:%.mli=%.cmi)

.PHONY: all native byte native-code byte-code test clean

all: native byte

native-code: native

byte-code: byte

test:

native: protobuf.cmxa

protobuf.cmxa: protobuf.cmx

protobuf.cmx: $(native_cmx)
	ocamlfind ocamlopt -pack -o $@ $^

byte: protobuf.cma

protobuf.cma: protobuf.cmo

protobuf.cmo: $(byte_cmo)
	ocamlfind ocamlc -pack -o $@ $^

# Dependencies
include ../Ocamlrules.mk.in
