let main ()=
	if((Array.length Sys.argv)==3 && Sys.argv.(1)="-c") then (Compression.compression (Sys.argv.(2))) else if((Array.length Sys.argv)==3 && Sys.argv.(1)="-d") then (Decompression.decompression (Sys.argv.(2))) else (print_string "Usage: ./Huffman [-c][-d] fichier";print_newline ());;

main();;	