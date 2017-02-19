CC=ocamlopt
EXEC=Huffman
SRC=$(wildcard *.ml)


$(EXEC): $(SRC)
	ocamlopt bitio.ml Compression.ml Decompression.ml Huffman.ml -o Huffman
	rm *.cmx
	rm *.o
	rm *.cmi