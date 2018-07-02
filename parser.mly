 /* File parser.mly */
%{

	open Functions

%}
	%token <string> STRING
        %token <int> INT 
	%token <string> BOOL
	%token <string> ID
	%token <string> TAINT
	%token <string> COMMENTO
	%token <string> DIRETTIVA
	%token CONST
	%token EINT ESTRING EBOOL SUM PROD DIFF MINUS OR AND ISZERO NOT NEWLOC IFTHENELSE LET DEN FUN APPL REC NOT_EQ PRINT RUNF EQUAL IF THEN ELSE PROC SUBSTR CONCAT LEN IN VAL EQ
        %token CALL BLOCK RUNP CIF ASSIGN CIFTHENELSE WHILE DO
	%token REFLECT 
        %token LPAREN RPAREN COMMA LQPAREN RQPAREN SEMIC RBRACE LBRACE 
        %token EOL
        %start main             /* the entry point */
   	%type <Functions.parser> main
	%type <string> ideval
	%type <string> base_type
	%type <string> dic_o
	%left CONST
	%left EQ
	%left VAL
        %%


	/*ENTRY POINT*/
        main:
	 | dic_o EOL		{ print_string("\nOffuscamento INT in corso.. ");Lstringa ($1^(!sep)^"eint temp = (0,U)"^(!sep)^
"while("^(!ideprova)^"!= 0) do (temp = temp + 1; "^(!ideprova)^" = "^(!ideprova)^" - 1)"^(!sep)^(!ideprova)^" = temp") }
	 
	 | dic_b EOL		{  print_string("\nOffuscamento BOOL in corso.. ");Lstringa ($1^(!sep)^"ebool tempt = (true,U)"^(!sep)^"ebool tempf = (false, U)"^(!sep)^"cif("^(!ideprova)^") then ("^(!ideprova)^" = tempt) else ("^(!ideprova)^" = tempf)")}
	 | expr EOL		{  Lstringa ($1) }
	 | dic_n EOL		{ Lstringa($1) }
         | reflect EOL		{  Lparse ($1) } 
	 | comm EOL		{ Lstringa ($1) }
	 | COMMENTO EOL		{ Lstringa ($1) }
	 | DIRETTIVA EOL	{ Lstringa ($1) }
	 | EOL 		     	{ Lstringa ""}
        ;

	dic_n:
	  | ESTRING ideval EQUAL base_type { "estring "^$2^" = "^$4}
	 | CONST ESTRING ideval EQUAL base_type { "const estring "^$3^" = "^$5}	
	 ;
	
	dic_b:
	  | EBOOL ideval EQUAL ideval 	{ "ebool "^$2^" = "^$4}	
	 | EBOOL ideval EQUAL base_type 	{ "ebool "^$2^" = "^$4}	
	;

	/*TIPI BASE*/
	base_type:
         | LPAREN INT COMMA TAINT RPAREN         { "("^string_of_int($2)^","^$4^")" }
	 | LPAREN BOOL COMMA TAINT RPAREN	{  "("^$2^","^$4^")" }
	 | LPAREN STRING COMMA TAINT RPAREN	{  "("^$2^","^$4^")" }
	 | LPAREN base_type RPAREN 	{ "("^$2^")" }
	 | BOOL				{ $1 }
	 | STRING			{ "\""^$1^"\"" }
	 | INT          		{ string_of_int $1 }
	;

	idevall:
	| ideval { $1}
	| ideval SEMIC idevall { $1^";"^$3 }
	| LQPAREN idevall RQPAREN { "["^$2^"]" } 
	;

	ideval:
	 | ID 				{ ideprova := $1; $1 }
	 | LPAREN ideval RPAREN 		{ $2 }
	;
	
	dic_o:
	 | EINT ideval EQUAL ideval 	{ "eint "^$2^" = "^$4}	
	 | EINT ideval EQUAL base_type 	{ "eint "^$2^" = "^$4}	
	;


        expr:
        | base_type { $1 }
	| ideval { $1 }
        | LPAREN expr RPAREN      { "("^$2^")" }

	/*TEST SU INT*/


	| ISZERO expr { "iszero "^$2}
	| expr EQ expr {$1^" == "^$3}
	| expr NOT_EQ expr	{ $1^" != "^$3}

	/*COSTANTI*/

	| EINT base_type { "eint "^$2 }
	| EBOOL base_type { "ebool "^$2 }
	| ESTRING base_type { "estring "^$2 }
	| CONST ESTRING base_type { "const estring "^$3 }
   	
	
	/*OPERAZIONI SU BOOL*/

	| NOT expr 	{"!"^$2}
	| expr OR expr 	{$1^" or "^$3}
	| expr AND expr {$1^" and "^$3}

	
	/*OPERAZIONI SU INT*/

	| MINUS expr {"-"^$2}
	| expr SUM expr 	{$1^" + "^$3}
	| expr MINUS expr 	{$1^" - "^$3}
	| expr PROD expr 	{$1^" * "^$3}
	

	/*ESPRESSIONI COMPOSTE*/

	| IF LPAREN expr RPAREN THEN expr ELSE expr { "if( "^$3^") then "^$6^" else "^$8 }
	| IF LPAREN expr RPAREN THEN expr { "if( "^$3^") then "^$6 }
	| LET FUN ideval LPAREN idevall RPAREN EQUAL expr IN expr {"let function "^$3^"("^$5^") = " ^$8^" in "^$10 }
	| LET REC FUN ideval LPAREN idevall RPAREN EQUAL expr IN expr {"let rec function "^$4^"("^$6^") = " ^$9^" in "^$11 }
	| REC FUN ideval LPAREN idevall RPAREN EQUAL expr {"rec function "^$3^"("^$5^") = " ^$8 }
	| FUN ideval LPAREN idevall RPAREN EQUAL expr {"function "^$2^"("^$4^") = " ^$7 }
	
	/*FUNZIONI*/
	/*| PROC idevall LBRACE block RBRACE {Proc($2,$4)}*/
	| RUNF ideval LPAREN expr RPAREN { "runf "^$2^"("^$4^")" }
	
	
	/*OPERAZIONI SU STRINGHE*/
        | PRINT expr {"print("^$2^")"}
        | LEN expr {"len"^$2}
	| SUBSTR LPAREN expr COMMA expr COMMA expr RPAREN { "substr("^$3^","^$5^","^$7^")" }
	| expr CONCAT expr {$1^"."^$3}
	;

	
	reflect:
	| REFLECT LPAREN STRING RPAREN { $3 }
	;

	comm:
	| PROC ideval LPAREN idevall RPAREN EQUAL block { "procedure "^$2^"("^$4^") = " ^$7 }
	| ideval EQUAL expr	{ $1^" = "^$3 }
	| ideval EQUAL ideval	{ $1^" = "^$3 }
	| WHILE expr DO comml { "while"^$2^" do "^$4 }
	| CIF expr THEN comml ELSE comml { "cif( "^$2^") then "^$4^" else "^$6  }
	| LBRACE block RBRACE { "{\n"^$2^"\n}" }
	| RUNP ideval LPAREN expr RPAREN { "runp "^$2^"("^$4^")" }
	;

	comml:
	| LPAREN comml RPAREN { "("^$2^")" }
	| comm SEMIC comml { $1^";"^$3 }
	| comm  { $1 }
	| LQPAREN RQPAREN	{"[]"}
	;

	/*BLOCCHI*/
	block:
	| LPAREN block RPAREN	{ "("^$2^")" }
	| LQPAREN RQPAREN COMMA LQPAREN RQPAREN COMMA LQPAREN RQPAREN  { "[],[],[]" }
	| dicl COMMA LQPAREN RQPAREN COMMA LQPAREN RQPAREN  { $1^",\n[]"^","^offusca(!lista_var) }
	| dicl COMMA dic COMMA LQPAREN RQPAREN  { $1^",\n"^$3^","^offusca(!lista_var)}
	| LQPAREN RQPAREN COMMA dic COMMA LQPAREN RQPAREN  { "[],\n"^$4^",\n[]" }
	| dicl COMMA LQPAREN RQPAREN COMMA comml  { $1^",[]"^","^offusca(!lista_var)^$6 }
	| LQPAREN RQPAREN COMMA LQPAREN RQPAREN COMMA comml  { "[],[],\n"^$7 }
	| LQPAREN RQPAREN COMMA dic COMMA comml RPAREN { "[],"^$4^","^$6 }
	| LPAREN dicl COMMA dic COMMA comml RPAREN { "("^$2^",\n"^$4^","^offusca(!lista_var)^$6^")" }
	| dicl COMMA dic COMMA comml { $1^",\n"^$3^","^offusca(!lista_var)^$5 }
	| comml { $1 }
	| dicl COMMA comml { $1^","^offusca(!lista_var)^$3 }
	| dicl	{ $1^","^offusca(!lista_var) }
	| LBRACE block RBRACE { "{\n"^$2^"\n}" }
	;
	
 	dicl:
	| dic { $1}
	| dic SEMIC dicl { $1^";"^$3 }
	| LQPAREN dicl RQPAREN { "["^$2^"]" } 
	;

	dic:
	 | EINT ideval EQUAL expr 	{ aggiungi($2);"eint temp = (0,U);\neint "^$2^" = "^$4}	
	 | ESTRING ideval EQUAL expr { "estring "^$2^" = "^$4}
	 | CONST ESTRING ideval EQUAL expr { "const estring "^$3^" = "^$5}	
	 | EBOOL ideval EQUAL expr { "ebool "^$2^" = "^$4}	
	;

