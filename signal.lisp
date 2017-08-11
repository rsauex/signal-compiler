(defpackage #:signal
  (:use #:cl #:lazy-list #:cl-yatlp))

(in-package #:signal)

(defparser signal
  (signal-program -> program)

  (program -> "PROGRAM" id :. ";" blk :. "."
           -> "PROCEDURE" id :. parameters? :. ";" blk :. ";")
  
  (blk -> :> decls :< :v "BEGIN" :> stmt* :< :v "END")
  (decls -> const-decls? var-decls? math-func-decls? proc-decls?)

  (const-decls -> :v :{ "CONST" :! const-decl* :< :})
  (const-decl -> id "=" constant :. ";" :v)
  (constant -> minus :. unsigned-number
            -> unsigned-number)

  (var-decls -> :v :{ "VAR" :! var-decl* :< :})
  (var-decl -> id-list :. ":" var-attr+ :. ";" :v)
  (var-attr -> "SIGNAL"
            -> "INTEGER"
            -> "FLOAT"
            -> "BLOCKFLOAT"
            -> "EXT"
            -> "[" :. range-list :. "]")
  (range -> unsigned-integer :. ".." :. unsigned-integer)

  (math-func-decls -> :v :{ "DEFFUNC" :! math-func-decl* :< :})
  (math-func-decl -> id "=" expr math-func-attrs :. ";" :v)
  (math-func-attrs -> "\\" unsigned-integer :. ".." :. unsigned-integer)

  (proc-decls -> proc-decl+)
  (proc-decl -> :v :{ "PROCEDURE" id :. parameters? :. ";" blk :. ";" :v :})
  (parameters -> "(" :. parameter-list :. ")")
  (parameter -> id-list :. ":" var-attr+)
  (parameter-list :* -> ";" parameter)

  (stmt -> :v id :. dimension? ":=" expr
        -> :v id :. actual-arguments?
        -> :v :{ "IF" :! cond-expr :< "THEN" :> stmt* :< :v "ENDIF" :}
        -> :v :{ "IF" :! cond-expr :< "THEN" :> stmt* :< :v "ELSE" :> stmt* :< :v "ENDIF" :}
        -> :v :{ "WHILE" :! cond-expr :< "DO" :> stmt* :< :v "ENDWHILE" :}
        -> :v :{ "LOOP" :> stmt* :< :v "ENDLOOP" :}
        -> :v :{ "FOR" :!  for-decl :< "DO" :> stmt* :< :v "ENDFOR" :}
        -> :v :{ "CASE" :! expr :< "OF" :> alternative* :< :v "ENDCASE" :}
        -> :v "LINK" id "," unsigned-integer
        -> :v "IN" unsigned-integer
        -> :v "OUT" unsigned-integer
        -> :v "RETURN"
        -> ";")

  (for-decl -> id ":=" expr "TO" expr)

  (actual-arguments -> "(" :. actual-argument-list :. ")")
  (actual-argument --> expr)

  (alternative -> :v :{ "/" expr :. ":"  :> stmt* :< :})

  (cond-expr -> cond-expr1 "OR" cond-expr
             --> cond-expr1)
  (cond-expr1 -> cond-expr2 "AND" cond-expr1
              --> cond-expr2)
  (cond-expr2 -> expr "<" expr
              -> expr "<=" expr
              -> expr "=" expr
              -> expr "<>" expr
              -> expr ">=" expr
              -> expr ">" expr
              -> "[" cond-expr "]"
              -> "NOT" cond-expr2)

  (expr -> expr1 "+" expr
        -> expr1 "-" expr
        -> expr1 "!" expr
        --> expr1)
  (expr1 -> expr2 "*" expr1
         -> expr2 "/" expr1
         -> expr2 "&" expr1
         -> expr2 "MOD" expr1
         --> expr2)
  (expr2 --> unsigned-number
         --> id
         -> id :. dimension
         -> id :. actual-arguments
         -> "(" :. expr :. ")"
         -> "-" :. expr2
         -> "^" :. expr2)

  (dimension -> "[" :. expr-list :. "]")

  (unsigned-number -> unsigned-integer :. fractional-part
                   -> unsigned-integer)
  (fractional-part -> "#" :. sign? :. unsigned-integer)

  (minus -> "-")
  (sign -> "+"
        -> "-")
  
  (id -> :lex identifier)
  (unsigned-integer -> :lex digits-string)

  (actual-argument-list :* -> "," actual-argument)

  (id-list :+ -> "," id)
  (range-list :+ -> "," range)
  (expr-list :+ -> "," expr))

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
