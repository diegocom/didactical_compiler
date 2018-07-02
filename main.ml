
open Functions
let r = ref 0;;
(*FUNZIONI DI PARSING*)
(*Funzione che permette di parserizzare una riga*)
let parserRiga x =  let lexbuf = (Lexing.from_string(x^"\n")) in 
      Parser.main Lexer.token lexbuf;;
(*Funzione che permette di parserizzare una lista di comandi e la esegue passandola alla semt*)
let rec parserLista x  = match x with
  |[] -> ""
  | h::t -> r := !r + 1;print_string("\nParsing istruzione: ");print_int(!r);(semt (parserRiga (h ^ ";;")))^parserLista t
and semt l = match l with
	|Lstringa(s) -> s^(!sep)
	|Lparse(s) -> print_string("\nScomposizione reflect.. ");reflect s
and reflect stringa =
   let elencoComandi = (Str.split (Str.regexp "|") stringa) in
  	let stringa = parserLista elencoComandi in stringa;;




(*-----------------------INIZIO MAIN FILE CALC------------------------------------*)
(*DICHIARAZIONE DELLE ECCEZIONI*)
exception Parametri of string;;

(*LEGGO IL FILE PASSATO COME PARAMETRO*)
if(Array.length Sys.argv < 2) then (raise (Parametri "Passa il nome del programma da eseguire!"));;
let testo = ref "";;
let lista = ref [] ;;

let in_channel = open_in Sys.argv.(1) in
(try
  while true do
    let line = input_line in_channel in
    lista := line :: !lista;
  done
with End_of_file ->
  close_in in_channel);;
  
let rec unisci x = match x with
   | [] -> ""
  | h::t -> h^(unisci t);;



(*UNISCO TUTTO IL TESTO LETTO DAL FILE*)
testo := unisci (List.rev !lista);;



print_string("\n\nESEGUO..\n\n");;


(*ESEGUO L'ELENCO DI COMANDI LETTO DAL FILE*)
let nome = ref ("compiled_"^Sys.argv.(1));;
if(Array.length Sys.argv == 3) then
	nome := Sys.argv.(2);;

let elencoComandi = Str.split (Str.regexp ";;") !testo in 
	let output = (parserLista elencoComandi) in
		let oc = open_out !nome in
			  Printf.fprintf oc "%s" output; close_out oc;;

print_string ("\nFile "^(!nome)^" correttamente generato.\n");;

(*-----------------------FINE MAIN FILE CALC------------------------------------*)



