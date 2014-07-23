type token =
  | TOK_case
  | TOK_false
  | TOK_for
  | TOK_switch
  | TOK_true
  | SYMB1
  | SYMB2
  | SYMB3
  | SYMB4
  | SYMB5
  | SYMB6
  | SYMB7
  | SYMB8
  | SYMB9
  | SYMB10
  | SYMB11
  | SYMB12
  | SYMB13
  | SYMB14
  | SYMB15
  | SYMB16
  | SYMB17
  | SYMB18
  | TOK_EOF
  | TOK_Ident of (string)
  | TOK_String of (string)
  | TOK_Integer of (int)
  | TOK_Double of (float)
  | TOK_Char of (char)
  | TOK_UIdent of (string)
  | TOK_LIdent of (string)
  | TOK_Wild of (string)

val pAgent :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Absdavinci.agent
val pGuardedAgent :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Absdavinci.guardedAgent
val pAbstraction :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Absdavinci.abstraction
val pConcretion :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Absdavinci.concretion
val pBinding :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Absdavinci.binding
val pPattern :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Absdavinci.pattern
val pChannel :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Absdavinci.channel
val pSymbol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Absdavinci.symbol
val pVariation :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Absdavinci.variation
val pInformation :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Absdavinci.information
val pLyst :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Absdavinci.lyst
val pValue :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Absdavinci.value
val pDuality :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Absdavinci.duality
val pAgent_list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Absdavinci.agent list
val pGuardedAgent_list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Absdavinci.guardedAgent list
val pPattern_list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Absdavinci.pattern list
val pBinding_list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Absdavinci.binding list
