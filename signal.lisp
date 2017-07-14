(defpackage #:signal
  (:use #:cl #:lazy-list #:lexer #:parser #:parser-creation))

(in-package #:signal)

(defparser signal
  (signal-program -> program)

  (program -> "PROGRAM" identifier :. ";" :v  blk :. "."
           -> "PROCEDURE" identifier parameters? :. ";" :v blk :. ";")
  
  (blk -> decls "BEGIN" stmt* "END")
  (decls -> const-decls? var-decls? math-func-decls? proc-decls?)

  (const-decls -> "CONST" const-decl*
               :options :mimic const-decls?)
  (const-decl -> identifier "=" constant ";")
  (constant -> minus? unsigned-number)

  (var-decls -> "VAR" var-decl*
             :options :mimic var-decls?)
  (var-decl -> identifier+ ":" var-attr+ ";")
  (var-attr -> "SIGNAL"
            -> "INTEGER"
            -> "FLOAT"
            -> "BLOCKFLOAT"
            -> "EXT"
            -> "[" range+ "]")
  (range -> unsigned-integer ".." unsigned-integer)

  (math-func-decls -> "DEFFUNC" math-func-decl*
                   :options :mimic math-func-decls?)
  (math-func-decl -> identifier "=" expr math-func-attrs ";")
  (math-func-attrs -> "\\" unsigned-integer "," unsigned-integer)

  (proc-decls -> proc-decl+
              :options :mimic proc-decls?)
  (proc-decl -> "PROCEDURE" identifier parameters? ";" blk ";")
  (parameters -> "(" var-decl* ")"
              :options :mimic parameters?)

  (stmt -> identifier dimension? ":=" expr :. ";"
        -> identifier actual-arguments? ";"
        -> :{ "IF" :! cond-expr :< "THEN" :> stmt* :< "ENDIF" :. ";" :}
        -> :{ "IF" :! cond-expr :< "THEN" :> stmt* :< "ELSE" :> :v stmt* :< "ENDIF" :. ";" :}
        -> "WHILE" cond-expr "DO" stmt* "ENDWHILE" ";"
        -> "LOOP" stmt* "ENDLOOP" ";"
        -> "FOR"  for-decl "DO" stmt* "ENDFOR" ";"
        -> "CASE" expr "OF" alternative* "ENDCASE" ";"
        -> "LINK" identifier "," unsigned-integer ";"
        -> "IN" unsigned-integer ";"
        -> "OUT" unsigned-integer ";"
        -> "RETURN" ";"
        -> ";")

  (for-decl -> identifier ":=" expr "TO" expr)

  (actual-arguments -> "(" actual-argument* ")"
                    :options :mimic actual-arguments?)
  (actual-argument* -> * "," expr)

  (alternative -> expr ":" "/" stmt* "\\")

  (cond-expr -> cond-expr1 "OR" cond-expr
             -> :^ cond-expr1)
  (cond-expr1 -> cond-expr2 "AND" cond-expr1
              -> :^ cond-expr2
              :options :mimic cond-expr)
  (cond-expr2 -> expr "<" expr
              -> expr "<=" expr
              -> expr "=" expr
              -> expr "<>" expr
              -> expr ">=" expr
              -> expr ">" expr
              -> "[" cond-expr "]"
              -> "NOT" cond-expr2
              :options :mimic cond-expr)

  (expr -> expr1 "+" expr
        -> expr1 "-" expr
        -> expr1 "!" expr
        -> :^ expr1)
  (expr1 -> expr2 "*" expr1
         -> expr2 "/" expr1
         -> expr2 "&" expr1
         -> expr2 "MOD" expr1
         -> :^ expr2
         :options :mimic expr)
  (expr2 -> unsigned-number
         -> identifier dimension?
         -> identifier actual-arguments
         -> "(" expr ")"
         -> "-" expr2
         -> "^" expr2
         :options :mimic expr)

  (variabl -> identifier dimension?)
  (dimension -> "[" expr+ "]"
             :options :mimic dimension?)

  (unsigned-number -> unsigned-integer fractional-part?)
  (fractional-part -> "#" sign? unsigned-integer
                   :options :mimic fractional-part?)

  (minus -> "-"
         :options :mimic minus?)
  (sign -> "+"
        -> "-"
        :options :mimic sign?)
  
  (identifier -> :lex identifier)
  (unsigned-integer -> :lex digits-string)

  (const-decls? -> :eps -> :^ const-decls)
  (minus? -> :eps -> :^ minus)
  (var-decls? -> :eps -> :^ var-decls)
  (math-func-decls? -> :eps -> :^ math-func-decls)
  (proc-decls? -> :eps -> :^ proc-decls)
  (parameters? -> :eps -> :^ parameters)
  (actual-arguments? -> :eps -> :^ actual-arguments)
  (dimension? -> :eps -> dimension)
  (sign? -> :eps -> :^ sign)
  (fractional-part? -> :eps -> fractional-part)

  (const-decl* -> * :eps const-decl)
  (var-decl* -> * :eps var-decl)
  (math-func-decl* -> * :eps math-func-decl)
  (stmt* -> * :eps stmt)
  (alternative* -> * :eps alternative)

  (var-attr+ -> + :eps var-attr)
  (identifier+ -> + "," identifier)
  (range+ -> + "," range)
  (proc-decl+ -> + :eps proc-decl)
  (expr+ -> + "," expr))

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
