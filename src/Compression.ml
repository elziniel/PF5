type tree =
    | Feuille of int * int
    | Node of int * tree * tree;;

let rec clnup2 n l=match l with
  	[]->[]
  	|p::q-> if (p=n) then clnup2 n q else p::clnup2 n q;;

let clnup n l =
	let rec clnupaux r acc = match r with
		| [] -> acc
		| q::p -> if (q!=n) then clnupaux p (q::acc) else clnupaux p acc
	in clnupaux l [];; 	

let hist l = 
	let rec histaux	m acc= match m with 
    	[] -> List.rev acc  
    	| n::p ->  
       	   let filtre r = r=n  
        in histaux (clnup n p) ((n,(List.length (List.filter filtre l)))::acc)
    in histaux l [];;    


 let sort t1 t2 =
    let c1,v1 = t1 and c2,v2=t2 in
      if v1=v2 then 0 else if v1>v2 then 1 else -1;; 


let rec tltofl l = match l with
  | [] -> []
  | n::p -> let c,v=n in Feuille(v,c)::tltofl p;;


let min l=
  let rec minaux n l= match n,l with
    | tree,[]->tree
    | Feuille(c,v),Feuille(c1,v1)::p->if c<c1 then minaux (Feuille(c,v)) p else minaux (Feuille(c1,v1)) p 
    | Node(no,g,d),Feuille(c,v)::p->if no<c then minaux (Node(no,g,d)) p else minaux (Feuille(c,v)) p
    | Feuille(c,v),Node(no,g,d)::p->if no<c then minaux (Node(no,g,d)) p else minaux (Feuille(c,v)) p
    | Node(no,g,d),Node(no1,g1,d1)::p -> if no<no1 then minaux (Node(no,g,d)) p else minaux (Node(no1,g1,d1)) p
  in minaux (Feuille(100000000,0)) l;;

let ltoarbre4 n m = match n,m with 
  | Feuille(c,v),Feuille(c1,v1)->Node(c+c1,Feuille(c,v),Feuille(c1,v1))
  | Feuille(c,v),Node(no,g,d)->Node(c+no,Feuille(c,v),Node(no,g,d))
  | Node(no,g,d),Feuille(c,v)->Node(no+c,Node(no,g,d),Feuille(c,v))
  | Node(no,g,d),Node(no1,g1,d1)->Node(no1+no,Node(no,g,d),Node(no1,g1,d1))

let ltoarbre3 l=
  let rec lotaux n = match n with
    |[]->Feuille(0,0)
    | r::[]->r
    | r::p-> let a1 = min (r::p) in let a2=min (clnup2 a1 (r::p)) in lotaux ((ltoarbre4 a1 a2)::(clnup2 a1 (clnup2 a2 (r::p))))
  in lotaux (tltofl (List.sort sort (((-1,1))::l)));;

let rec puissance b n =
    if n = 0 then 1 else if n=1 then b else   b*puissance b (n-1);;  

let binaire n=
    let rec binaux b p = 
      if b>=0 && p>=puissance 2 b then "1"^binaux (b-1) (p-(puissance 2 b)) else if b>=0 && p<puissance 2 b then "0"^binaux (b-1) p else ""
  in binaux 7 n;;

let rec parcour a = match a with
  | Feuille(c,v)-> if (v<=254 && v!=(-1)) then "0"^binaire v else if v=(-1) then "0111111111" else "0111111110"
  | Node(no,g,d)-> "1"^(parcour g)^(parcour d);;


let tb a=
    let rec tbaux b acc acc2 = match b with
      Feuille(c,v)-> ((v,acc))::[]
      | Node(no,g,d)-> let r=tbaux g (acc^"0") acc2 and s=tbaux d (acc^"1") acc2 in r@s@acc2
    in tbaux a "" [];;

   let rec search c l = match l with 
          |[]->"0"
          |n::p-> let r,s=n in if r=c then s else search c p;; 


let compression file=
  let abc = Bitio.open_in_bit file in 
  let v ()= 
        let v_aux () =
          try     
          Some(Bitio.input_bit_byte abc)
        with End_of_file -> None
      in let rec v_fin l =
        match v_aux () with
          | Some(s)->v_fin(s::l)
          | None -> List.rev l
      in v_fin []
  in let c = v() in let d = hist c in let e = ltoarbre3 d in let f = tb e in Bitio.seek_in_bit abc 0; let encode ()=  
        let rec encodaux acc =  
      try
        acc^(encodaux (search (Bitio.input_bit_byte abc) f))
      with End_of_file -> acc^(search (-1) f)
    in encodaux "" in let str = encode() and fgh = Bitio.open_out_bit (file^".hf") and pe = parcour e 
    in let wrt s=
      let entetes n = 
          Bitio.output_bit_byte fgh 135;
          Bitio.output_bit_byte fgh 74;
          Bitio.output_bit_byte fgh 31;
          Bitio.output_bit_byte fgh 72;
          Bitio.output_bit_byte fgh n;   
      in
let rec fin n =
  if (n=0) then () else (Bitio.output_bit fgh 0; (fin (n-1)));
in
  let ios l = 
    try
      int_of_string l
    with
    | int_of_string -> Char.code (String.get l 0)
  in
  let rec wrtaux s =
      Bitio.output_bit fgh (ios (String.sub s 0 1));
      if (String.length s)>1 then wrtaux (String.sub s 1 ((String.length s)-1)) else ()   
  in entetes 0; wrtaux pe; wrtaux s; fin (8-((5+(String.length pe)+(String.length str)) mod 8));
   in wrt str; Bitio.output_bit_byte fgh 0;Bitio.close_out_bit fgh;;