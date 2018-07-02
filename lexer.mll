 (* File lexer.mll *)
        {
        open Parser        (* Tipo dei token definito nel file parser.mly *)
        exception Eof
        }
        rule token = parse
          [' ' '\t' '\n']     { token lexbuf }     (* salto gli spazi bianchi *)
        | ";;"          	{ EOL }
        | ['0'-'9']+ as lxm 	{ INT(int_of_string lxm) }
	| "T"			{TAINT("T")}
	| "U"			{TAINT("U")}
	| "false"	 	{ BOOL("false")}
	| "true"		{ BOOL("true")}
	| ['e' 'E']"int"        { EINT}
	| ['e' 'E']"string"	{ ESTRING }
	| ['e' 'E']"bool"	{ EBOOL }
	| "Sum" 		{ SUM } 
	| ['r' 'R']"eflect"	{REFLECT}
	| "Prod" 		{ PROD }
	| "Diff"		{ DIFF }
	| "Minus"		{ MINUS }
	| "Eq"			{ EQ }
	| ['o' 'O']"r"		{ OR }
	| ['a' 'A']"nd" 	{ AND }
	| ['i' 'I']"szero"	{ ISZERO }
     	| "Not"			{ NOT }
	| "Val"			{ VAL }
	| "Newloc"		{ NEWLOC }
	| "Ifthenelse"		{ IFTHENELSE }
	| "Fun"			{ FUN }
	| "in"			{ IN }
	| ['l' 'L']"et"		{ LET }
	| "Den"			{ DEN }
	| "Appl"		{ APPL }
	| "Block"		{ BLOCK }
	| "Call"		{ CALL }
	| "Proc"		{ PROC }
	| ['r' 'R']"ec"		{ REC }
        | '('            	{LPAREN }
        | ')'            	{ RPAREN }
	| ','			{ COMMA }
	| '['			{LQPAREN}
	| ']'			{RQPAREN}
	| ';'			{SEMIC}
	| "if"			{IF}
	| "then"		{THEN}
	| "else"		{ELSE}
	| "cif"			{CIF}
	| "Assign"		{ASSIGN}
	| "Cifthenelse" 	{CIFTHENELSE}
	| ['w' 'W']"hile" 	{WHILE}
	| "=="			{EQ}
	| "="			{EQUAL}
	| "+"			{SUM}
	| "-"			{MINUS}
	| "*"			{PROD}
	| "!"			{NOT}
	| "{"			{LBRACE}
	| "}"			{RBRACE}
	| "!="			{NOT_EQ}
	| "procedure"		{PROC}
	| "function"		{FUN}
	| "do"			{DO}
	| "runf"		{RUNF}
	| "runp"		{RUNP}
	| ['p' 'P']"rint"	{PRINT}
	| "len"			{LEN}
	| "substr"		{SUBSTR}
	| "concat"		{CONCAT}
	| "const"		{CONST}
	| '\\'	{ token lexbuf }
	| '.'		{CONCAT}
	| ("--")['A'-'Z' ' ']* as direttiva {DIRETTIVA(direttiva)}
	| ("@@")['A'-'Z' 'a'-'z' '0'-'9' '!'-'/' ':' '<'-'@' '['-'?' '_' ' ' '|' ]* as commento{COMMENTO(commento)}
	| ['A'-'Z' 'a'-'z']* as id {ID(id)}
	| '"'(['A'-'Z' 'a'-'z' '0'-'9' '!'-'/' ':' '<'-'@' '['-'?' '_' ' ' '|' '\\'] + as lxm)'"' {STRING(lxm)}
        | eof            { raise Eof }
