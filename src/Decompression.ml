type treed = Node of treed * treed | Feuille of int 

let verifE arbre = ((Bitio.input_bit_byte arbre) = 135 && (Bitio.input_bit_byte arbre)=74 && (Bitio.input_bit_byte arbre)=31 && (Bitio.input_bit_byte arbre)=72)
let aligne arbre =Bitio.seek_in_bit arbre (Bitio.input_bit_byte arbre + 5);;
let rec ftotree arbre=
        let n=(Bitio.input_bit arbre) in
        if(n=0) then let r = (Bitio.input_bit_byte arbre) in if(r=255) then let s = (Bitio.input_bit arbre) in if(s=1) then (Feuille(0)) else (Feuille(255)) else (Feuille(r)) else (let t=(ftotree arbre) and u = (ftotree arbre) in Node(t,u));;

let decompression file=
	let inn = Bitio.open_in_bit file and out = Bitio.open_out_bit (String.sub file 0 ((String.length file)-3))
	in let r = (if(verifE inn) then (aligne inn; Some(ftotree inn)) else (None)) in 
	let decode inp out =
  let decodaux arbre = 
    try
    let abr = ref arbre 
	in
    while(true) do
    match !abr with
    | Feuille(c)-> if(c=0) then raise End_of_file else ((Bitio.output_bit_byte out c); abr:=arbre);
    | Node(g,d)->let n = (Bitio.input_bit inp) in if (n=0) then (abr:=g) else (abr:=d);
  done
  with End_of_file->();
  in (if(r!=None) then (let a = match r with None -> Feuille(-1) | Some(s)->s in (decodaux a);(Bitio.close_out_bit out)) else (Sys.remove (String.sub file 0 ((String.length file)-3)) ;print_string "ERREUR DE FICHIER");)
  in decode inn out;;
