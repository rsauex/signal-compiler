(defpackage #:signal
  (:use #:cl #:lazy-list #:lexer #:parser #:parser-creation))

(in-package #:signal)

(defparser signal
  (signal-program -> program)

  (program -> "PROGRAM" identifier ";" blk "."
           -> "PROCEDURE" identifier parameters? ";" blk ";")
  
  (blk -> decls "BEGIN" stmt* "END")
  (decls -> const-decls?
             var-decls?
             math-func-decls?
             proc-decls?)

  
  (const-decls? -> :eps
                -> "CONST" const-decl*)
  
  (const-decl* -> * :eps const-decl)
  (const-decl -> identifier "=" constant ";")
  (constant -> minus? unsigned-number)
  (minus? -> :eps
          -> "-")

  
  (var-decls? -> :eps
              -> "VAR" var-decl*)
  
  (var-decl* -> * :eps var-decl)
  (var-decl -> identifier+ ":" var-attr+ ";")
  (identifier+ -> + "," identifier)
  (var-attr+ -> + :eps var-attr)
  (var-attr -> "SIGNAL"
            -> "INTEGER"
            -> "FLOAT"
            -> "BLOCKFLOAT"
            -> "EXT"
            -> "[" range+ "]")
  (range+ -> + "," range)
  (range -> unsigned-integer ".." unsigned-integer)

  (math-func-decls? -> :eps
                    -> "DEFFUNC" math-func-decl*)
  (math-func-decl* -> * :eps math-func-decl)
  (math-func-decl -> identifier "=" expr math-func-attrs ";")
  (math-func-attrs -> "\\" unsigned-integer "," unsigned-integer)

  (proc-decls? -> :eps
               -> proc-decl+)
  (proc-decl+ -> + :eps proc-decl)
  (proc-decl -> "PROCEDURE" identifier parameters? ";" blk ";")
  (parameters? -> :eps
               -> "(" var-decl* ")")

  (stmt* -> * :eps stmt)
  (stmt -> identifier dimension? ":=" expr :. ";"
        -> identifier actual-arguments? ";"
        -> :{ "IF" :! cond-expr :< "THEN" :> stmt* :< else-part? "ENDIF" :. ";" :}
        -> "WHILE" cond-expr "DO" stmt* "ENDWHILE" ";"
        -> "LOOP" stmt* "ENDLOOP" ";"
        -> "FOR"  for-decl "DO" stmt* "ENDFOR" ";"
        -> "CASE" expr "OF" alternative* "ENDCASE" ";"
        -> "LINK" identifier "," (:lex unsigned-integer) ";"
        -> "IN" (:lex unsigned-integer) ";"
        -> "OUT" (:lex unsigned-integer) ";"
        -> "RETURN" ";"
        -> ";")

  (else-part? -> :eps
              -> "ELSE" :> :v  stmt* :<)

  (for-decl -> identifier ":=" expr "TO" expr)

  (actual-arguments? -> :eps
                     -> actual-arguments)
  (actual-arguments -> "(" actual-argument* ")")
  (actual-argument* -> * "," expr)

  (alternative* -> * :eps alternative)
  (alternative -> expr ":" "/" stmt* "\\")

  (cond-expr -> cond-expr1 "OR" cond-expr
             -> :^ cond-expr1)
  (cond-expr1 -> (cond-expr2 "AND" cond-expr1)
              -> :^ cond-expr2
              :options :mimic cond-expr)
  (cond-expr2 -> (expr "<" expr)
              -> (expr "<=" expr)
              -> (expr "=" expr)
              -> (expr "<>" expr)
              -> (expr ">=" expr)
              -> (expr ">" expr)
              -> ("[" cond-expr "]")
              -> ("NOT" cond-expr2)
              :options :mimic cond-expr)

  (expr -> (expr1 "+" expr)
        -> (expr1 "-" expr)
        -> (expr1 "!" expr)
        -> (:^ expr1))
  (expr1 -> (expr2 "*" expr1)
         -> (expr2 "/" expr1)
         -> (expr2 "&" expr1)
         -> (expr2 "MOD" expr1)
         -> (:^ expr2)
         :options :mimic expr)
  (expr2 -> unsigned-number
         -> (identifier dimension?)
         -> (identifier actual-arguments)
         -> ("(" expr ")")
         -> ("-" expr2)
         -> ("^" expr2)
         :options :mimic expr)

  (variabl -> identifier dimension?)
  (dimension? -> :eps
              -> ("[" expr+ "]"))
  (expr+ -> + "," expr)

  (unsigned-number -> unsigned-integer fractional-part?)
  (fractional-part? -> :eps
                    -> ("#" sign? unsigned-integer))

  (identifier -> :lex identifier)

  (unsigned-integer -> :lex digits-string)
  (sign? -> :eps
         -> "+"
         -> "-"))

(deflexer signal
  (letter (:r #\A #\Z) :fragment)
  (digit (:r #\0 #\9) :fragment)
  (comma #\,)
  (l-s-br #\[)
  (r-s-br #\])
  (l-c-br #\()
  (r-c-br #\))
  (circum #\^)
  (slash #\/)
  (backslash #\\)
  (colon #\:)
  (semicolon #\;)
  (assing (#\: #\=))
  (two-dot "..")
  (dot #\.)
  (mul #\*)
  (and-o #\&)
  (mod "MOD")
  (add #\+)
  (sub #\-)
  (fract-des #\#)
  (not-o #\!)
  (less "<")
  (less-eq "<=")
  (eq "=")
  (not-eq "<>")
  (gr-eq ">=")
  (gr ">")
  (begin "BEGIN")
  (or "OR")
  (and "AND")
  (not "NOT")
  (do "DO")
  (to "TO")
  (endif "ENDIF")
  (else "ELSE")
  (then "THEN")
  (if "IF")
  (return "RETURN")
  (out "OUT")
  (in "IN")
  (link "LINK")
  (endcase "ENDCASE")
  (of "OF")
  (case "CASE")
  (endfor "ENDFOR")
  (for "FOR")
  (endloop "ENDLOOP")
  (loop "LOOP")
  (endwhile "ENDWHILE")
  (while "WHILE")
  (procedure "PROCEDURE")
  (deffunc "DEFFUNC")
  (ext "EXT")
  (blockfloat "BLOCKFLOAT")
  (float "FLOAT")
  (integer "INTEGER")
  (signal "SIGNAL")
  (var "VAR")
  (const "CONST")
  (end "END")
  (program "PROGRAM")
  (digits-string (:+ digit))
  (comment (#\( #\* (:*? :any) #\* #\)) :skip t)
  (identifier (letter (:* (:or letter digit))))
  (whitespace (:+ (:or #\Newline #\Space #\Return #\Vt #\Page #\Tab)) :skip t))
