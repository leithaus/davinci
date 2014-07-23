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

open Parsing;;
let _ = parse_error;;
# 3 "src/main/ocaml/repl/Pardavinci.mly"
open Absdavinci
open Lexing
# 41 "src/main/ocaml/repl/Pardavinci.ml"
let yytransl_const = [|
  257 (* TOK_case *);
  258 (* TOK_false *);
  259 (* TOK_for *);
  260 (* TOK_switch *);
  261 (* TOK_true *);
  262 (* SYMB1 *);
  263 (* SYMB2 *);
  264 (* SYMB3 *);
  265 (* SYMB4 *);
  266 (* SYMB5 *);
  267 (* SYMB6 *);
  268 (* SYMB7 *);
  269 (* SYMB8 *);
  270 (* SYMB9 *);
  271 (* SYMB10 *);
  272 (* SYMB11 *);
  273 (* SYMB12 *);
  274 (* SYMB13 *);
  275 (* SYMB14 *);
  276 (* SYMB15 *);
  277 (* SYMB16 *);
  278 (* SYMB17 *);
  279 (* SYMB18 *);
  280 (* TOK_EOF *);
    0|]

let yytransl_block = [|
  281 (* TOK_Ident *);
  282 (* TOK_String *);
  283 (* TOK_Integer *);
  284 (* TOK_Double *);
  285 (* TOK_Char *);
  286 (* TOK_UIdent *);
  287 (* TOK_LIdent *);
  288 (* TOK_Wild *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\004\000\004\000\
\005\000\005\000\006\000\006\000\007\000\007\000\008\000\008\000\
\009\000\009\000\010\000\010\000\011\000\011\000\012\000\012\000\
\013\000\013\000\014\000\014\000\015\000\015\000\016\000\016\000\
\017\000\017\000\018\000\018\000\018\000\018\000\018\000\018\000\
\018\000\018\000\019\000\020\000\021\000\022\000\022\000\023\000\
\023\000\023\000\023\000\024\000\024\000\024\000\025\000\026\000\
\027\000\027\000\028\000\028\000\028\000\028\000\029\000\029\000\
\029\000\029\000\029\000\030\000\030\000\031\000\031\000\031\000\
\032\000\032\000\032\000\033\000\033\000\034\000\034\000\034\000\
\037\000\038\000\039\000\036\000\035\000\040\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\001\000\002\000\001\000\002\000\001\000\
\002\000\001\000\002\000\001\000\002\000\001\000\002\000\001\000\
\002\000\001\000\002\000\001\000\002\000\001\000\002\000\001\000\
\002\000\001\000\002\000\001\000\002\000\001\000\002\000\001\000\
\002\000\001\000\003\000\004\000\002\000\005\000\007\000\005\000\
\005\000\001\000\004\000\004\000\003\000\007\000\007\000\004\000\
\001\000\001\000\001\000\001\000\001\000\004\000\001\000\001\000\
\001\000\001\000\002\000\003\000\005\000\005\000\001\000\001\000\
\001\000\001\000\004\000\001\000\001\000\000\000\001\000\003\000\
\000\000\001\000\003\000\001\000\003\000\000\000\001\000\003\000\
\001\000\001\000\001\000\001\000\001\000\001\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\069\000\000\000\000\000\068\000\000\000\
\000\000\000\000\081\000\082\000\083\000\084\000\087\000\000\000\
\000\000\042\000\063\000\056\000\064\000\065\000\066\000\004\000\
\000\000\088\000\000\000\006\000\000\000\089\000\000\000\008\000\
\000\000\090\000\000\000\010\000\000\000\085\000\091\000\000\000\
\000\000\000\000\049\000\051\000\050\000\055\000\012\000\092\000\
\000\000\014\000\000\000\093\000\000\000\053\000\052\000\016\000\
\094\000\000\000\018\000\095\000\000\000\020\000\096\000\058\000\
\000\000\000\000\022\000\097\000\000\000\024\000\098\000\000\000\
\026\000\099\000\000\000\028\000\100\000\000\000\000\000\030\000\
\101\000\000\000\000\000\032\000\102\000\000\000\000\000\034\000\
\103\000\000\000\000\000\000\000\000\000\000\000\037\000\000\000\
\001\000\000\000\000\000\000\000\003\000\000\000\005\000\000\000\
\007\000\059\000\000\000\009\000\000\000\000\000\011\000\000\000\
\013\000\015\000\017\000\019\000\021\000\023\000\025\000\000\000\
\027\000\000\000\029\000\000\000\031\000\000\000\033\000\000\000\
\000\000\035\000\000\000\000\000\000\000\000\000\000\000\045\000\
\000\000\060\000\000\000\000\000\000\000\072\000\075\000\077\000\
\080\000\000\000\000\000\036\000\067\000\000\000\000\000\043\000\
\044\000\000\000\000\000\000\000\000\000\048\000\054\000\038\000\
\000\000\040\000\041\000\062\000\061\000\000\000\000\000\000\000\
\000\000\000\000\039\000\046\000\047\000"

let yydgoto = "\018\000\
\031\000\042\000\046\000\050\000\055\000\064\000\068\000\073\000\
\076\000\079\000\084\000\087\000\090\000\093\000\097\000\101\000\
\105\000\094\000\098\000\047\000\051\000\106\000\102\000\069\000\
\058\000\059\000\082\000\060\000\061\000\035\000\095\000\099\000\
\103\000\107\000\062\000\036\000\037\000\038\000\039\000\000\000"

let yysindex = "\087\001\
\213\255\102\255\079\255\089\255\034\255\066\255\012\255\015\255\
\017\255\242\255\029\255\073\255\072\255\249\255\108\255\174\255\
\207\255\000\000\000\000\000\000\018\255\053\255\000\000\057\000\
\099\255\048\255\000\000\000\000\000\000\000\000\000\000\061\255\
\107\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\027\000\000\000\082\255\000\000\080\255\000\000\088\255\000\000\
\057\000\000\000\104\255\000\000\013\000\000\000\000\000\112\255\
\116\255\132\255\000\000\000\000\000\000\000\000\000\000\000\000\
\118\255\000\000\127\255\000\000\123\255\000\000\000\000\000\000\
\000\000\124\255\000\000\000\000\129\255\000\000\000\000\000\000\
\107\255\134\255\000\000\000\000\138\255\000\000\000\000\139\255\
\000\000\000\000\140\255\000\000\000\000\143\255\144\255\000\000\
\000\000\145\255\146\255\000\000\000\000\133\255\148\255\000\000\
\000\000\147\255\149\255\027\000\150\255\168\255\000\000\057\000\
\000\000\169\255\171\255\163\255\000\000\172\255\000\000\175\255\
\000\000\000\000\014\255\000\000\099\255\027\000\000\000\057\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\057\000\
\000\000\150\255\000\000\027\000\000\000\027\000\000\000\115\255\
\177\255\000\000\162\255\178\255\178\255\057\000\057\000\000\000\
\011\255\000\000\117\255\176\255\170\255\000\000\000\000\000\000\
\000\000\057\000\027\000\000\000\000\000\180\255\181\255\000\000\
\000\000\182\255\185\255\186\255\187\255\000\000\000\000\000\000\
\189\255\000\000\000\000\000\000\000\000\027\000\027\000\057\000\
\198\255\200\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\190\255\196\255\000\000\
\199\255\000\000\000\000\000\000\000\000\000\000\000\000\204\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\059\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\026\255\000\000\000\000\
\000\000\033\255\000\000\000\000\000\000\094\255\000\000\000\000\
\000\000\027\255\000\000\128\255\215\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\056\255\
\000\000\063\255\000\000\000\000\000\000\071\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\009\000\222\000\000\000\252\255\221\000\015\000\238\255\
\220\000\255\255\180\000\246\255\004\000\217\000\234\255\173\255\
\206\255\159\255\253\255\000\000\000\000\000\000\000\000\000\000"

let yytablesize = 360
let yytable = "\033\000\
\085\000\110\000\123\000\071\000\034\000\070\000\111\000\077\000\
\081\000\032\000\144\000\066\000\033\000\034\000\072\000\088\000\
\075\000\034\000\080\000\057\000\065\000\071\000\033\000\070\000\
\153\000\145\000\108\000\034\000\083\000\067\000\053\000\057\000\
\071\000\052\000\154\000\020\000\079\000\079\000\023\000\074\000\
\030\000\030\000\054\000\118\000\161\000\054\000\030\000\081\000\
\053\000\071\000\079\000\026\000\034\000\053\000\159\000\116\000\
\074\000\080\000\109\000\027\000\028\000\029\000\070\000\030\000\
\054\000\063\000\112\000\020\000\057\000\073\000\023\000\089\000\
\086\000\020\000\020\000\156\000\023\000\023\000\044\000\070\000\
\078\000\078\000\057\000\026\000\113\000\053\000\073\000\045\000\
\048\000\160\000\026\000\027\000\028\000\029\000\078\000\030\000\
\054\000\049\000\027\000\028\000\029\000\040\000\041\000\076\000\
\076\000\117\000\155\000\096\000\041\000\030\000\033\000\119\000\
\177\000\158\000\076\000\034\000\067\000\076\000\114\000\115\000\
\147\000\071\000\057\000\070\000\162\000\163\000\033\000\121\000\
\030\000\054\000\125\000\034\000\172\000\173\000\033\000\124\000\
\157\000\078\000\078\000\034\000\126\000\127\000\171\000\166\000\
\167\000\128\000\129\000\130\000\033\000\033\000\041\000\170\000\
\131\000\034\000\034\000\140\000\057\000\132\000\168\000\169\000\
\033\000\133\000\134\000\135\000\136\000\034\000\138\000\137\000\
\142\000\139\000\176\000\141\000\143\000\100\000\146\000\020\000\
\150\000\148\000\023\000\149\000\165\000\151\000\033\000\164\000\
\152\000\174\000\049\000\034\000\175\000\178\000\179\000\026\000\
\187\000\053\000\182\000\183\000\185\000\186\000\184\000\027\000\
\028\000\029\000\180\000\030\000\054\000\181\000\104\000\188\000\
\020\000\189\000\070\000\023\000\019\000\070\000\020\000\021\000\
\022\000\023\000\024\000\073\000\025\000\073\000\078\000\043\000\
\026\000\056\000\053\000\074\000\120\000\091\000\026\000\000\000\
\027\000\028\000\029\000\000\000\030\000\054\000\027\000\028\000\
\029\000\078\000\030\000\020\000\021\000\022\000\023\000\024\000\
\092\000\025\000\020\000\021\000\022\000\023\000\024\000\000\000\
\025\000\000\000\000\000\026\000\000\000\000\000\000\000\000\000\
\000\000\000\000\026\000\027\000\028\000\029\000\020\000\030\000\
\000\000\023\000\027\000\028\000\029\000\000\000\030\000\000\000\
\000\000\000\000\000\000\000\000\020\000\000\000\026\000\023\000\
\053\000\122\000\000\000\000\000\000\000\000\000\027\000\028\000\
\029\000\000\000\030\000\054\000\026\000\000\000\053\000\000\000\
\000\000\000\000\000\000\000\000\027\000\028\000\029\000\000\000\
\030\000\054\000\020\000\021\000\022\000\023\000\024\000\000\000\
\025\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\026\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\027\000\028\000\029\000\000\000\030\000\001\000\
\002\000\003\000\004\000\005\000\006\000\007\000\008\000\009\000\
\010\000\011\000\012\000\013\000\014\000\015\000\016\000\017\000"

let yycheck = "\001\000\
\011\000\024\000\053\000\007\000\001\000\007\000\025\000\009\000\
\010\000\001\000\108\000\000\001\014\000\010\000\000\001\012\000\
\000\001\014\000\010\000\005\000\006\000\025\000\024\000\025\000\
\011\001\109\000\009\001\024\000\000\001\018\001\020\001\017\000\
\007\001\000\001\021\001\002\001\010\001\011\001\005\001\007\001\
\030\001\030\001\031\001\045\000\142\000\031\001\030\001\049\000\
\020\001\024\001\024\001\018\001\049\000\020\001\138\000\041\000\
\024\001\049\000\006\001\026\001\027\001\028\001\007\001\030\001\
\031\001\000\001\019\001\002\001\010\001\007\001\005\001\000\001\
\000\001\002\001\002\001\126\000\005\001\005\001\000\001\024\001\
\010\001\011\001\024\001\018\001\024\001\020\001\024\001\009\001\
\000\001\140\000\018\001\026\001\027\001\028\001\024\001\030\001\
\031\001\009\001\026\001\027\001\028\001\000\001\001\001\010\001\
\011\001\024\001\125\000\000\001\001\001\030\001\112\000\024\001\
\163\000\136\000\021\001\112\000\018\001\024\001\012\001\013\001\
\112\000\125\000\108\000\125\000\010\001\011\001\128\000\024\001\
\030\001\031\001\015\001\128\000\016\001\017\001\136\000\024\001\
\128\000\010\001\011\001\136\000\009\001\024\001\153\000\148\000\
\149\000\019\001\024\001\024\001\150\000\151\000\001\001\153\000\
\024\001\150\000\151\000\023\001\142\000\024\001\150\000\151\000\
\162\000\024\001\024\001\024\001\022\001\162\000\022\001\024\001\
\022\001\024\001\162\000\024\001\024\001\000\001\007\001\002\001\
\014\001\009\001\005\001\009\001\019\001\010\001\184\000\007\001\
\010\001\010\001\009\001\184\000\019\001\010\001\010\001\018\001\
\184\000\020\001\009\001\009\001\182\000\183\000\010\001\026\001\
\027\001\028\001\021\001\030\001\031\001\021\001\000\001\010\001\
\002\001\010\001\007\001\005\001\000\001\024\001\002\001\003\001\
\004\001\005\001\006\001\024\001\008\001\007\001\024\001\002\000\
\018\001\005\000\020\001\008\000\049\000\013\000\018\001\255\255\
\026\001\027\001\028\001\255\255\030\001\031\001\026\001\027\001\
\028\001\000\001\030\001\002\001\003\001\004\001\005\001\006\001\
\000\001\008\001\002\001\003\001\004\001\005\001\006\001\255\255\
\008\001\255\255\255\255\018\001\255\255\255\255\255\255\255\255\
\255\255\255\255\018\001\026\001\027\001\028\001\002\001\030\001\
\255\255\005\001\026\001\027\001\028\001\255\255\030\001\255\255\
\255\255\255\255\255\255\255\255\002\001\255\255\018\001\005\001\
\020\001\021\001\255\255\255\255\255\255\255\255\026\001\027\001\
\028\001\255\255\030\001\031\001\018\001\255\255\020\001\255\255\
\255\255\255\255\255\255\255\255\026\001\027\001\028\001\255\255\
\030\001\031\001\002\001\003\001\004\001\005\001\006\001\255\255\
\008\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\018\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\026\001\027\001\028\001\255\255\030\001\001\000\
\002\000\003\000\004\000\005\000\006\000\007\000\008\000\009\000\
\010\000\011\000\012\000\013\000\014\000\015\000\016\000\017\000"

let yynames_const = "\
  TOK_case\000\
  TOK_false\000\
  TOK_for\000\
  TOK_switch\000\
  TOK_true\000\
  SYMB1\000\
  SYMB2\000\
  SYMB3\000\
  SYMB4\000\
  SYMB5\000\
  SYMB6\000\
  SYMB7\000\
  SYMB8\000\
  SYMB9\000\
  SYMB10\000\
  SYMB11\000\
  SYMB12\000\
  SYMB13\000\
  SYMB14\000\
  SYMB15\000\
  SYMB16\000\
  SYMB17\000\
  SYMB18\000\
  TOK_EOF\000\
  "

let yynames_block = "\
  TOK_Ident\000\
  TOK_String\000\
  TOK_Integer\000\
  TOK_Double\000\
  TOK_Char\000\
  TOK_UIdent\000\
  TOK_LIdent\000\
  TOK_Wild\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'agent) in
    Obj.repr(
# 59 "src/main/ocaml/repl/Pardavinci.mly"
                       ( _1 )
# 342 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.agent))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "src/main/ocaml/repl/Pardavinci.mly"
          ( raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 348 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.agent))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'guardedAgent) in
    Obj.repr(
# 62 "src/main/ocaml/repl/Pardavinci.mly"
                                     ( _1 )
# 355 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.guardedAgent))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "src/main/ocaml/repl/Pardavinci.mly"
          ( raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 361 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.guardedAgent))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'abstraction) in
    Obj.repr(
# 65 "src/main/ocaml/repl/Pardavinci.mly"
                                   ( _1 )
# 368 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.abstraction))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "src/main/ocaml/repl/Pardavinci.mly"
          ( raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 374 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.abstraction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'concretion) in
    Obj.repr(
# 68 "src/main/ocaml/repl/Pardavinci.mly"
                                 ( _1 )
# 381 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.concretion))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "src/main/ocaml/repl/Pardavinci.mly"
          ( raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 387 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.concretion))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'binding) in
    Obj.repr(
# 71 "src/main/ocaml/repl/Pardavinci.mly"
                           ( _1 )
# 394 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.binding))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "src/main/ocaml/repl/Pardavinci.mly"
          ( raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 400 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.binding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'pattern) in
    Obj.repr(
# 74 "src/main/ocaml/repl/Pardavinci.mly"
                           ( _1 )
# 407 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.pattern))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "src/main/ocaml/repl/Pardavinci.mly"
          ( raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 413 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'channel) in
    Obj.repr(
# 77 "src/main/ocaml/repl/Pardavinci.mly"
                           ( _1 )
# 420 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.channel))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "src/main/ocaml/repl/Pardavinci.mly"
          ( raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 426 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.channel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'symbol) in
    Obj.repr(
# 80 "src/main/ocaml/repl/Pardavinci.mly"
                         ( _1 )
# 433 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "src/main/ocaml/repl/Pardavinci.mly"
          ( raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 439 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.symbol))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'variation) in
    Obj.repr(
# 83 "src/main/ocaml/repl/Pardavinci.mly"
                               ( _1 )
# 446 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.variation))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "src/main/ocaml/repl/Pardavinci.mly"
          ( raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 452 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.variation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'information) in
    Obj.repr(
# 86 "src/main/ocaml/repl/Pardavinci.mly"
                                   ( _1 )
# 459 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.information))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "src/main/ocaml/repl/Pardavinci.mly"
          ( raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 465 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.information))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'lyst) in
    Obj.repr(
# 89 "src/main/ocaml/repl/Pardavinci.mly"
                     ( _1 )
# 472 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.lyst))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "src/main/ocaml/repl/Pardavinci.mly"
          ( raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 478 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.lyst))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'value) in
    Obj.repr(
# 92 "src/main/ocaml/repl/Pardavinci.mly"
                       ( _1 )
# 485 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.value))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "src/main/ocaml/repl/Pardavinci.mly"
          ( raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 491 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'duality) in
    Obj.repr(
# 95 "src/main/ocaml/repl/Pardavinci.mly"
                           ( _1 )
# 498 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.duality))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "src/main/ocaml/repl/Pardavinci.mly"
          ( raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 504 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.duality))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'agent_list) in
    Obj.repr(
# 98 "src/main/ocaml/repl/Pardavinci.mly"
                                 ( _1 )
# 511 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.agent list))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "src/main/ocaml/repl/Pardavinci.mly"
          ( raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 517 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.agent list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'guardedAgent_list) in
    Obj.repr(
# 101 "src/main/ocaml/repl/Pardavinci.mly"
                                               ( _1 )
# 524 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.guardedAgent list))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "src/main/ocaml/repl/Pardavinci.mly"
          ( raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 530 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.guardedAgent list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'pattern_list) in
    Obj.repr(
# 104 "src/main/ocaml/repl/Pardavinci.mly"
                                     ( _1 )
# 537 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.pattern list))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "src/main/ocaml/repl/Pardavinci.mly"
          ( raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 543 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.pattern list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'binding_list) in
    Obj.repr(
# 107 "src/main/ocaml/repl/Pardavinci.mly"
                                     ( _1 )
# 550 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.binding list))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "src/main/ocaml/repl/Pardavinci.mly"
          ( raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 556 "src/main/ocaml/repl/Pardavinci.ml"
               : Absdavinci.binding list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'agent_list) in
    Obj.repr(
# 111 "src/main/ocaml/repl/Pardavinci.mly"
                               ( Composition _2 )
# 563 "src/main/ocaml/repl/Pardavinci.ml"
               : 'agent))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'guardedAgent_list) in
    Obj.repr(
# 112 "src/main/ocaml/repl/Pardavinci.mly"
                                             ( Superposition _3 )
# 570 "src/main/ocaml/repl/Pardavinci.ml"
               : 'agent))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'channel) in
    Obj.repr(
# 113 "src/main/ocaml/repl/Pardavinci.mly"
                  ( Replication _2 )
# 577 "src/main/ocaml/repl/Pardavinci.ml"
               : 'agent))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'binding_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'agent) in
    Obj.repr(
# 114 "src/main/ocaml/repl/Pardavinci.mly"
                                           ( Reception (_3, _5) )
# 585 "src/main/ocaml/repl/Pardavinci.ml"
               : 'agent))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'binding_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'pattern_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'agent) in
    Obj.repr(
# 115 "src/main/ocaml/repl/Pardavinci.mly"
                                                              ( Filtration (_3, _5, _7) )
# 594 "src/main/ocaml/repl/Pardavinci.ml"
               : 'agent))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'variation) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'concretion) in
    Obj.repr(
# 116 "src/main/ocaml/repl/Pardavinci.mly"
                                           ( Intimation (_1, _4) )
# 602 "src/main/ocaml/repl/Pardavinci.ml"
               : 'agent))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'variation) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'concretion) in
    Obj.repr(
# 117 "src/main/ocaml/repl/Pardavinci.mly"
                                           ( Transmission (_1, _4) )
# 610 "src/main/ocaml/repl/Pardavinci.ml"
               : 'agent))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 118 "src/main/ocaml/repl/Pardavinci.mly"
          ( Actualization _1 )
# 617 "src/main/ocaml/repl/Pardavinci.ml"
               : 'agent))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'agent) in
    Obj.repr(
# 121 "src/main/ocaml/repl/Pardavinci.mly"
                                            ( Injection (_2, _4) )
# 625 "src/main/ocaml/repl/Pardavinci.ml"
               : 'guardedAgent))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'variation) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'agent) in
    Obj.repr(
# 124 "src/main/ocaml/repl/Pardavinci.mly"
                                          ( Applicant (_2, _4) )
# 633 "src/main/ocaml/repl/Pardavinci.ml"
               : 'abstraction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'information) in
    Obj.repr(
# 127 "src/main/ocaml/repl/Pardavinci.mly"
                                     ( Applicand _2 )
# 640 "src/main/ocaml/repl/Pardavinci.ml"
               : 'concretion))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'channel) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'pattern) in
    Obj.repr(
# 130 "src/main/ocaml/repl/Pardavinci.mly"
                                                            ( Question (_1, _3, _6) )
# 649 "src/main/ocaml/repl/Pardavinci.ml"
               : 'binding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'channel) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'pattern) in
    Obj.repr(
# 131 "src/main/ocaml/repl/Pardavinci.mly"
                                                      ( Interrogation (_1, _3, _6) )
# 658 "src/main/ocaml/repl/Pardavinci.ml"
               : 'binding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'symbol) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'pattern_list) in
    Obj.repr(
# 134 "src/main/ocaml/repl/Pardavinci.mly"
                                          ( Element (_1, _3) )
# 666 "src/main/ocaml/repl/Pardavinci.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'variation) in
    Obj.repr(
# 135 "src/main/ocaml/repl/Pardavinci.mly"
              ( Variable _1 )
# 673 "src/main/ocaml/repl/Pardavinci.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 136 "src/main/ocaml/repl/Pardavinci.mly"
          ( Materialization _1 )
# 680 "src/main/ocaml/repl/Pardavinci.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lyst) in
    Obj.repr(
# 137 "src/main/ocaml/repl/Pardavinci.mly"
         ( Procession _1 )
# 687 "src/main/ocaml/repl/Pardavinci.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lIdent) in
    Obj.repr(
# 140 "src/main/ocaml/repl/Pardavinci.mly"
                 ( Identification _1 )
# 694 "src/main/ocaml/repl/Pardavinci.ml"
               : 'channel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'variation) in
    Obj.repr(
# 141 "src/main/ocaml/repl/Pardavinci.mly"
              ( Nomination _1 )
# 701 "src/main/ocaml/repl/Pardavinci.ml"
               : 'channel))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'agent) in
    Obj.repr(
# 142 "src/main/ocaml/repl/Pardavinci.mly"
                               ( Transcription _3 )
# 708 "src/main/ocaml/repl/Pardavinci.ml"
               : 'channel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lIdent) in
    Obj.repr(
# 145 "src/main/ocaml/repl/Pardavinci.mly"
                ( Tag _1 )
# 715 "src/main/ocaml/repl/Pardavinci.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'uIdent) in
    Obj.repr(
# 148 "src/main/ocaml/repl/Pardavinci.mly"
                   ( Atomic _1 )
# 722 "src/main/ocaml/repl/Pardavinci.ml"
               : 'variation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'variation) in
    Obj.repr(
# 151 "src/main/ocaml/repl/Pardavinci.mly"
                        ( Indirection _1 )
# 729 "src/main/ocaml/repl/Pardavinci.ml"
               : 'information))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'agent) in
    Obj.repr(
# 152 "src/main/ocaml/repl/Pardavinci.mly"
          ( Reflection _1 )
# 736 "src/main/ocaml/repl/Pardavinci.ml"
               : 'information))
; (fun __caml_parser_env ->
    Obj.repr(
# 155 "src/main/ocaml/repl/Pardavinci.mly"
                     ( Empty  )
# 742 "src/main/ocaml/repl/Pardavinci.ml"
               : 'lyst))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pattern_list) in
    Obj.repr(
# 156 "src/main/ocaml/repl/Pardavinci.mly"
                               ( Enum _2 )
# 749 "src/main/ocaml/repl/Pardavinci.ml"
               : 'lyst))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'pattern_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'lyst) in
    Obj.repr(
# 157 "src/main/ocaml/repl/Pardavinci.mly"
                                          ( Cons (_2, _4) )
# 757 "src/main/ocaml/repl/Pardavinci.ml"
               : 'lyst))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'pattern_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'variation) in
    Obj.repr(
# 158 "src/main/ocaml/repl/Pardavinci.mly"
                                               ( ConsV (_2, _4) )
# 765 "src/main/ocaml/repl/Pardavinci.ml"
               : 'lyst))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'duality) in
    Obj.repr(
# 161 "src/main/ocaml/repl/Pardavinci.mly"
                ( BooleanLiteral _1 )
# 772 "src/main/ocaml/repl/Pardavinci.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'string) in
    Obj.repr(
# 162 "src/main/ocaml/repl/Pardavinci.mly"
           ( StringLiteral _1 )
# 779 "src/main/ocaml/repl/Pardavinci.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'int) in
    Obj.repr(
# 163 "src/main/ocaml/repl/Pardavinci.mly"
        ( IntegerLiteral _1 )
# 786 "src/main/ocaml/repl/Pardavinci.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'float) in
    Obj.repr(
# 164 "src/main/ocaml/repl/Pardavinci.mly"
          ( DoubleLiteral _1 )
# 793 "src/main/ocaml/repl/Pardavinci.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'agent) in
    Obj.repr(
# 165 "src/main/ocaml/repl/Pardavinci.mly"
                               ( Reification _3 )
# 800 "src/main/ocaml/repl/Pardavinci.ml"
               : 'value))
; (fun __caml_parser_env ->
    Obj.repr(
# 168 "src/main/ocaml/repl/Pardavinci.mly"
                   ( Verity  )
# 806 "src/main/ocaml/repl/Pardavinci.ml"
               : 'duality))
; (fun __caml_parser_env ->
    Obj.repr(
# 169 "src/main/ocaml/repl/Pardavinci.mly"
              ( Absurdity  )
# 812 "src/main/ocaml/repl/Pardavinci.ml"
               : 'duality))
; (fun __caml_parser_env ->
    Obj.repr(
# 172 "src/main/ocaml/repl/Pardavinci.mly"
                         ( []  )
# 818 "src/main/ocaml/repl/Pardavinci.ml"
               : 'agent_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'agent) in
    Obj.repr(
# 173 "src/main/ocaml/repl/Pardavinci.mly"
          ( (fun x -> [x]) _1 )
# 825 "src/main/ocaml/repl/Pardavinci.ml"
               : 'agent_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'agent) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'agent_list) in
    Obj.repr(
# 174 "src/main/ocaml/repl/Pardavinci.mly"
                            ( (fun (x,xs) -> x::xs) (_1, _3) )
# 833 "src/main/ocaml/repl/Pardavinci.ml"
               : 'agent_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 177 "src/main/ocaml/repl/Pardavinci.mly"
                                ( []  )
# 839 "src/main/ocaml/repl/Pardavinci.ml"
               : 'guardedAgent_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'guardedAgent) in
    Obj.repr(
# 178 "src/main/ocaml/repl/Pardavinci.mly"
                 ( (fun x -> [x]) _1 )
# 846 "src/main/ocaml/repl/Pardavinci.ml"
               : 'guardedAgent_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'guardedAgent) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'guardedAgent_list) in
    Obj.repr(
# 179 "src/main/ocaml/repl/Pardavinci.mly"
                                          ( (fun (x,xs) -> x::xs) (_1, _3) )
# 854 "src/main/ocaml/repl/Pardavinci.ml"
               : 'guardedAgent_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'pattern) in
    Obj.repr(
# 182 "src/main/ocaml/repl/Pardavinci.mly"
                       ( (fun x -> [x]) _1 )
# 861 "src/main/ocaml/repl/Pardavinci.ml"
               : 'pattern_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pattern_list) in
    Obj.repr(
# 183 "src/main/ocaml/repl/Pardavinci.mly"
                                ( (fun (x,xs) -> x::xs) (_1, _3) )
# 869 "src/main/ocaml/repl/Pardavinci.ml"
               : 'pattern_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 186 "src/main/ocaml/repl/Pardavinci.mly"
                           ( []  )
# 875 "src/main/ocaml/repl/Pardavinci.ml"
               : 'binding_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binding) in
    Obj.repr(
# 187 "src/main/ocaml/repl/Pardavinci.mly"
            ( (fun x -> [x]) _1 )
# 882 "src/main/ocaml/repl/Pardavinci.ml"
               : 'binding_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binding) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binding_list) in
    Obj.repr(
# 188 "src/main/ocaml/repl/Pardavinci.mly"
                                ( (fun (x,xs) -> x::xs) (_1, _3) )
# 890 "src/main/ocaml/repl/Pardavinci.ml"
               : 'binding_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 192 "src/main/ocaml/repl/Pardavinci.mly"
                    ( _1 )
# 897 "src/main/ocaml/repl/Pardavinci.ml"
               : 'string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 193 "src/main/ocaml/repl/Pardavinci.mly"
                    ( _1 )
# 904 "src/main/ocaml/repl/Pardavinci.ml"
               : 'int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 194 "src/main/ocaml/repl/Pardavinci.mly"
                    ( _1 )
# 911 "src/main/ocaml/repl/Pardavinci.ml"
               : 'float))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 195 "src/main/ocaml/repl/Pardavinci.mly"
                    ( UIdent (_1))
# 918 "src/main/ocaml/repl/Pardavinci.ml"
               : 'uIdent))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 196 "src/main/ocaml/repl/Pardavinci.mly"
                    ( LIdent (_1))
# 925 "src/main/ocaml/repl/Pardavinci.ml"
               : 'lIdent))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 197 "src/main/ocaml/repl/Pardavinci.mly"
                ( Wild (_1))
# 932 "src/main/ocaml/repl/Pardavinci.ml"
               : 'wild))
(* Entry pAgent *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry pGuardedAgent *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry pAbstraction *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry pConcretion *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry pBinding *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry pPattern *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry pChannel *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry pSymbol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry pVariation *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry pInformation *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry pLyst *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry pValue *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry pDuality *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry pAgent_list *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry pGuardedAgent_list *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry pPattern_list *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry pBinding_list *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let pAgent (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Absdavinci.agent)
let pGuardedAgent (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Absdavinci.guardedAgent)
let pAbstraction (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 3 lexfun lexbuf : Absdavinci.abstraction)
let pConcretion (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 4 lexfun lexbuf : Absdavinci.concretion)
let pBinding (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 5 lexfun lexbuf : Absdavinci.binding)
let pPattern (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 6 lexfun lexbuf : Absdavinci.pattern)
let pChannel (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 7 lexfun lexbuf : Absdavinci.channel)
let pSymbol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 8 lexfun lexbuf : Absdavinci.symbol)
let pVariation (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 9 lexfun lexbuf : Absdavinci.variation)
let pInformation (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 10 lexfun lexbuf : Absdavinci.information)
let pLyst (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 11 lexfun lexbuf : Absdavinci.lyst)
let pValue (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 12 lexfun lexbuf : Absdavinci.value)
let pDuality (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 13 lexfun lexbuf : Absdavinci.duality)
let pAgent_list (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 14 lexfun lexbuf : Absdavinci.agent list)
let pGuardedAgent_list (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 15 lexfun lexbuf : Absdavinci.guardedAgent list)
let pPattern_list (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 16 lexfun lexbuf : Absdavinci.pattern list)
let pBinding_list (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 17 lexfun lexbuf : Absdavinci.binding list)
