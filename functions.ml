let ideprova = ref "";;
let empty() = ref [];;
let lista_var = ref ([]: string list);;
let aggiungi x  = (lista_var := (x::!lista_var));;

let rec offusca(x) = match x with
 |[] -> lista_var := ([]: string list);""
 | h::t -> ("\ntemp = (0,U);\nwhile("^(h)^"!= 0) do (temp = temp + 1; "^(h)^" = "^(h)^" - 1);\n"^(h)^" = temp;\n")^offusca(t);;

type parser = 
	| Lstringa of string 
	| Lparse of string;;

let sep = ref ";;\n";;
