(defconst sollya-keywords '("begin" "else" "end" "begin" "from" "function" "proc" "procedure" "return"  "then" "void" "while"  "for" "to" "do" "if")
  "Keywords of the Sollya language.")
(defconst sollya-types		'("var" "double" "doubledouble" "doubleextended")
  "Variable types for the Sollya language")
(defconst sollya-constants	'("false" "pi" "true")
  "Constants of the Sollya language")
(defconst sollya-builtin	'("abs" "absolute" "accurateinfnorm" "acos"
				  "acosh" "asciiplot" "asin" "asinh" "atan" "atanh" "autodiff"
				  "autosimplify" "bashexecute"  "binary" "bind" "boolean" "by"
				  "canonical" "ceil" "chebyshevform" "checkinfnorm" "coeff"
				  "composepolynomials" "constant" "cos" "cosh" "decimal" "default"
				  "degree" "denominator" "diam" "dieonerrormode" "diff"
				  "dirtyfindzeros" "dirtyinfnorm" "dirtyintegral" "display"
				  "dyadic" "erf"
				  "erfc" "error" "evaluate" "execute" "exp" "expand" "expm1"
				  "exponent" "externalplot" "externalproc"  "file" "findzeros"
				  "fixed" "floating" "floor" "fpfindzeros" "fpminimax" 
				  "fullparentheses"  "getsuppressedmessages" "guessdegree"
				  "head" "hexadecimal" "honorcoeffprec" "hopitalrecursions" "horner"
				  "implementpoly" "implementconstant" "in" "inf" "infnorm"
				  "integer" "integral" "isbound" "isevaluable" "length" "library"
				  "libraryconstant" "list" "log" "log10" "log1p" "log2" "mantissa"
				  "max" "mid" "midpointmode" "min" "nearestint" "numberroots" "nop"
				  "numerator" "object" "of" "off" "on" "parse" "perturb"  "plot"
				  "points" "postscript" "postscriptfile" "powers" "prec" "precision"
				  "print" "printbinary" "printdouble" "printexpansion" "printfloat"
				  "printhexa" "printsingle" "printxml"  "quit"
				  "range" "rationalapprox" "rationalmode" "readfile" "readxml"
				  "relative" "remez" "rename" "restart" "revert" "round"
				  "roundcoefficients" "roundcorrectly" "roundingwarnings"
				  "simplifysafe" "searchgal" "showmessagenumbers" "simplify"
				  "simplifysafe" "sin" "single" "sinh" "sort" "sqrt" "string"
				  "subpoly" "substitute" "sup" "supnorm" "suppressmessage" "tail"
				  "tan" "tanh" "taylor" "taylorform" "taylorrecursions" "time"
				  "timing"  "tripledouble"  "unsuppressmessage" 
				  "verbosity" "version"  "worstcase" "write"
				  "zerodenominators")
  "Built-in functions of the Sollya language")

;; Generate a regex string for each category of keywords
(defconst sollya-keywords-regexp (regexp-opt sollya-keywords 'words))
(defconst sollya-types-regexp (regexp-opt sollya-types 'words))
(defconst sollya-constant-regexp (regexp-opt sollya-constants 'words))
(defconst sollya-builtin-regexp (regexp-opt sollya-builtin 'words))

(defconst sollya-strings (concat "\\(\\(\"\\|" line-prefix "[ \t]*\\\\\\)\\([^\"\\\\\n]\\|\\\\.\\)*\\(\"\\|\\\\[ \t]*$\\)\\|'\\([^'\\\\\n]\\|\\\\.[^'\n]*\\)'\\)")
  "Strings in the Sollya language."
  )

(defconst sollya-function-name (rx symbol-start (group (1+ (or word ?_))) (1+ space) (or "=" ":=")  (1+ space) "proc")
  "Regex to detect function names in function definitions in the Sollya language."
  )

;; create the list for font-lock.
;; each category of keyword is given a particular face
(defconst sollya-font-lock-keywords
  `(
    (,sollya-function-name (1 font-lock-function-name-face))

    ;; Font lock handling for variable declarations
    ("\\_<var "
     (0 font-lock-type-face)
     ("\\_<\\w+,*\\_>"
    	;; Pre-match form -- limit the sub-search to the end of the argument list.
      (save-excursion
	(goto-char (search-forward-regexp ";"))
	(point))
      ;; Post-match form
      (goto-char (match-end 0))
      (0 font-lock-variable-name-face)))

    (,sollya-types-regexp . font-lock-type-face)
    (,sollya-constant-regexp . font-lock-constant-face)
    (,sollya-builtin-regexp . font-lock-builtin-face)
    (,sollya-keywords-regexp . font-lock-keyword-face)
    (,sollya-strings . (1 font-lock-string-face))
    "Minimal highlighting expressions for `sollya-mode'."))

(provide 'sollya-fonts)
