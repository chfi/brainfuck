# - The -I flag introduces sub-directories
# - -use-ocamlfind is required to find packages (from Opam)
# - _tags file introduces packages, bin_annot flag for tool chain

OCB_FLAGS = -use-ocamlfind -I src -I lib
OCB = 		corebuild $(OCB_FLAGS)
SOURCES = main

all: 		native # profile debug

clean:
			$(OCB) -clean

native: 	#sanity
			$(OCB) $(SOURCES:=.native)

byte:		sanity
			$(OCB) *.byte

profile: 	sanity
			$(OCB) -tag profile *.native

debug: 		sanity
			$(OCB) -tag debug *.byte

sanity:
# check that packages can be found
			ocamlfind query core

# test: 		native
# 			echo '[1, 2, "three", {"four": 4}]' | ./main.native

.PHONY: 	all clean byte native profile debug sanity test
