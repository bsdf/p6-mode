(setq p6Attention
  (regexp-opt '("ACHTUNG" "ATTN" "ATTENTION" "FIXME" "NB" "TODO" "TBD" "WTF" "XXX" "NOTE") 'words))

(setq p6DeclareRoutine
  (regexp-opt '("macro" "sub" "submethod" "method" "multi" "proto" "only" "rule" "token" "regex" "category") 'words))

(setq p6Module
  (regexp-opt '("module" "class" "role" "package" "enum" "grammar" "slang" "subset") 'words))

(setq p6Variable
  (regexp-opt '("self") 'words))

(setq p6Include
  (regexp-opt '("use" "require") 'words))

(setq p6Conditional
  (regexp-opt '("if" "else" "elsif" "unless") 'words))

(setq p6VarStorage
  (regexp-opt '("let" "my" "our" "state" "temp" "has" "constant") 'words))

(setq p6Repeat
  (regexp-opt '("for" "loop" "repeat" "while" "until" "gather" "given") 'words))

(setq p6FlowControl
  (regexp-opt '("take" "do" "when" "next" "last" "redo" "return" "contend" "maybe" "defer"
                "default" "exit" "make" "continue" "break" "goto" "leave" "async" "lift") 'words))

(setq p6TypeConstraint
  (regexp-opt '("is" "as" "but" "trusts" "of" "returns" "handles" "where" "augment" "supersede") 'words))

(setq p6ClosureTrait
  (regexp-opt '("BEGIN" "CHECK" "INIT" "START" "FIRST" "ENTER" "LEAVE" "KEEP"
                "UNDO" "NEXT" "LAST" "PRE" "POST" "END" "CATCH" "CONTROL" "TEMP") 'words))

(setq p6Exception
  (regexp-opt '("die" "fail" "try" "warn") 'words))

(setq p6Property
  (regexp-opt '("prec" "irs" "ofs" "ors" "export" "deep" "binary" "unary" "reparsed" "rw" "parsed" "cached"
                "readonly" "defequiv" "will" "ref" "copy" "inline" "tighter" "looser" "equiv" "assoc"
                "required") 'words))

(setq p6Number
  (regexp-opt '("NaN" "Inf") 'words))

(setq p6Pragma
  (regexp-opt '("oo" "fatal") 'words))

(setq p6Type
  (regexp-opt '("Object" "Any" "Junction" "Whatever" "Capture" "Match"
                "Signature" "Proxy" "Matcher" "Package" "Module" "Class"
                "Grammar" "Scalar" "Array" "Hash" "KeyHash" "KeySet" "KeyBag"
                "Pair" "List" "Seq" "Range" "Set" "Bag" "Mapping" "Void" "Undef"
                "Failure" "Exception" "Code" "Block" "Routine" "Sub" "Macro"
                "Method" "Submethod" "Regex" "Str" "Blob" "Char" "Byte"
                "Codepoint" "Grapheme" "StrPos" "StrLen" "Version" "Num"
                "Complex" "num" "complex" "Bit" "bit" "bool" "True" "False"
                "Increasing" "Decreasing" "Ordered" "Callable" "AnyChar"
                "Positional" "Associative" "Ordering" "KeyExtractor"
                "Comparator" "OrderingPair" "IO" "KitchenSink" "Role"
                "Int" "int" "int1" "int2" "int4" "int8" "int16" "int32" "int64"
                "Rat" "rat" "rat1" "rat2" "rat4" "rat8" "rat16" "rat32" "rat64"
                "Buf" "buf" "buf1" "buf2" "buf4" "buf8" "buf16" "buf32" "buf64"
                "UInt" "uint" "uint1" "uint2" "uint4" "uint8" "uint16" "uint32"
                "uint64" "Abstraction" "utf8" "utf16" "utf32") 'words))

(setq p6Operator
  (regexp-opt '("div" "x" "xx" "mod" "also" "leg" "cmp" "before" "after" "eq" "ne" "le" "lt"
                "gt" "ge" "eqv" "ff" "fff" "and" "andthen" "Z" "X" "or" "xor"
                "orelse" "extra" "m" "mm" "rx" "s" "tr") 'words))

(setq p6Routines
  (regexp-opt '("eager" "hyper" "substr" "index" "rindex" "grep" "map" "sort" "join" "lines" "hints" "chmod"
                "split" "reduce" "min" "max" "reverse" "truncate" "zip" "cat" "roundrobin" "classify"
                "first" "sum" "keys" "values" "pairs" "defined" "delete" "exists" "elems" "end" "kv" "any"
                "all" "one" "wrap" "shape" "key" "value" "name" "pop" "push" "shift" "splice" "unshift" "floor"
                "ceiling" "abs" "exp" "log" "log10" "rand" "sign" "sqrt" "sin" "cos" "tan" "round" "strand"
                "roots" "cis" "unpolar" "polar" "atan2" "pick" "chop" "p5chop" "chomp" "p5chomp" "lc"
                "lcfirst" "uc" "ucfirst" "capitalize" "normalize" "pack" "unpack" "quotemeta" "comb"
                "samecase" "sameaccent" "chars" "nfd" "nfc" "nfkd" "nfkc" "printf" "sprintf" "caller"
                "evalfile" "run" "runinstead" "nothing" "want" "bless" "chr" "ord" "gmtime" "time" "eof"
                "localtime" "gethost" "getpw" "chroot" "getlogin" "getpeername" "kill" "fork" "wait"
                "perl" "graphs" "codes" "bytes" "clone" "print" "open" "read" "write" "readline" "say" "seek"
                "close" "opendir" "readdir" "slurp" "pos" "fmt" "vec" "link" "unlink" "symlink" "uniq" "pair"
                "asin" "atan" "sec" "cosec" "cotan" "asec" "acosec" "acotan" "sinh" "cosh" "tanh" "asinh"
                "acos" "acosh" "atanh" "sech" "cosech" "cotanh" "sech" "acosech" "acotanh" "asech" "ok"
                "plan" "ok" "dies" "ok" "lives" "ok" "skip" "todo" "pass" "flunk" "force" "todo" "use" "ok" "isa" "ok"
                "diag" "is" "deeply" "isnt" "like" "skip" "rest" "unlike" "cmp" "ok" "eval" "dies" "ok" "nok" "error"
                "eval" "lives" "ok" "approx" "is" "approx" "throws" "ok" "version" "lt" "plan" "eval" "succ" "pred"
                "times" "nonce" "once" "signature" "new" "connect" "operator" "undef" "undefine" "sleep"
                "from" "to" "infix" "postfix" "prefix" "circumfix" "postcircumfix" "minmax" "lazy" "count"
                "unwrap" "getc" "pi" "e" "context" "void" "quasi" "body" "each" "contains" "rewinddir" "subst"
                "can" "isa" "flush" "arity" "assuming" "rewind" "callwith" "callsame" "nextwith" "nextsame"
                "attr" "eval" "elsewhere" "none" "srand" "trim" "trim" "start" "trim" "end" "lastcall" "WHAT"
                "WHERE" "HOW" "WHICH" "VAR" "WHO" "WHENCE" "ACCEPTS" "REJECTS" "does" "not" "true" "iterator" "by"
                "re" "im" "invert" "flip") 'word))

(setq p6Comment "#.*$")

(setq p6Scalar "\\<$\\??\\(\\w\\|_\\)+\\>")

(setq p6Hash "%\\w+")

(setq p6Array "@\\(\\w\\|_\\)+\\>")

(setq p6String "\\(\"\\|'\\)\\(.*?\\)\\(\"\\|'\\)")

(setq p6HashString "\\([a-zA-Z_0-9]+\\)\\s-*?\\(?:=>\\)")

(setq p6ColonSomething "\s-*\\(:\\w+\\)")

(setq p6Package "\\(use \\)\\(\\w+\\(::\\w+\\)*\\)")

(setq p6-font-lock-keywords
      `(
        (,p6Comment . font-lock-comment-face)
        (,p6Package 2 font-lock-builtin-face)
        (,p6Scalar . font-lock-keyword-face)
        (,p6Array . font-lock-keyword-face)
        (,p6ColonSomething 1 font-lock-builtin-face)
        (,p6String . font-lock-string-face)
        (,p6HashString 1 font-lock-string-face)
        (,p6Operator . font-lock-type-face)
        (,p6Attention . font-lock-type-face)
        (,p6DeclareRoutine . font-lock-type-face)
        (,p6Module . font-lock-builtin-face)
        (,p6Variable . font-lock-type-face)
        (,p6Include . font-lock-builtin-face)
        (,p6Conditional . font-lock-type-face)
        (,p6VarStorage . font-lock-type-face)
        (,p6Repeat . font-lock-type-face)
        (,p6FlowControl . font-lock-type-face)
        (,p6TypeConstraint . font-lock-type-face)
        (,p6ClosureTrait . font-lock-type-face)
        (,p6Exception . font-lock-type-face)
        (,p6Property . font-lock-type-face)
        (,p6Number . font-lock-type-face)
        (,p6Pragma . font-lock-type-face)
        (,p6Type . font-lock-type-face)
        (,p6Routines . font-lock-keyword-face)
      ))

(define-derived-mode p6-mode fundamental-mode
  "p6 mode"
  "MaJoR MoDe FoR eDiTiNG perl6..."

  ;; syntax highlighting
  (setq font-lock-defaults '((p6-font-lock-keywords)))
)
