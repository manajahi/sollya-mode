;;; sollya.el --- 
;; 
;; Filename: sollya.el
;; Description: 
;; Author: Mohamed Amine Najahi
;; Maintainer: 
;; Created: Mon Jun 29 13:50:25 2015 (+0200)
;; Version: 
;; Package-Requires: ()
;; Last-Updated: Mon Jun 29 20:52:09 2015 (+0200)
;;           By: Mohamed Amine Najahi
;;     Update #: 43
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;; Code:

(defvar sollya-shell-active nil
  "Non nil when a subshell is running.")

(defvar sollya-mode-hook nil
  "Hook run after entering `sollya-mode'.")

(defvar sollya-mode-map
  (let ((map (make-keymap)))
    ;; example definition
    map)
  "Keymap used in `sollya-mode' buffers.")


(defconst sollya-keywords '("begin" "else" "end" "begin" "from" "function" "proc" "procedure" "return"  "then" "void" "while"  "for" "to" "do" "if")
  "Commands of the Sollya interpreter.")


(defconst sollya-types '("var" "double" "doubledouble" "doubleextended"))
(defconst sollya-functions '())
(defconst sollya-constants '("false" "pi" "true"))
(defconst sollya-events '("at_rot_target" "at_target" "attach"))
(defconst sollya-builtin '("abs" "absolute" "accurateinfnorm" "acos"
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
			   "zerodenominators"))

;; generate regex string for each category of keywords
(defconst sollya-keywords-regexp (regexp-opt sollya-keywords 'words))
(defconst sollya-type-regexp (regexp-opt sollya-types 'words))
(defconst sollya-constant-regexp (regexp-opt sollya-constants 'words))
(defconst sollya-event-regexp (regexp-opt sollya-events 'words))
(defconst sollya-functions-regexp (regexp-opt sollya-functions 'words))
(defconst sollya-builtin-regexp (regexp-opt sollya-builtin 'words))

(defconst sollya-strings (concat "\\(\\(\"\\|" line-prefix "[ \t]*\\\\\\)\\([^\"\\\\\n]\\|\\\\.\\)*\\(\"\\|\\\\[ \t]*$\\)\\|'\\([^'\\\\\n]\\|\\\\.[^'\n]*\\)'\\)")
  "Hijacked from Haskell mode."
  )
;; (rx symbol-start "def" (1+ space) (group (1+ (or word ?_))))
;; (defconst sollya-function-name (rx symbol-start (group (1+ (or word ?_))) (1+ space) "="  (1+ space) "proc")
;;   "..."
;;   )
(defconst sollya-function-name (rx symbol-start (group (1+ (or word ?_))) (1+ space) (or "=" ":="))
  "..."
  )

;; create the list for font-lock.
;; each category of keyword is given a particular face
(defconst sollya-font-lock-keywords
  `(
    (,sollya-type-regexp . font-lock-type-face)
    (,sollya-constant-regexp . font-lock-constant-face)
    (,sollya-event-regexp . font-lock-builtin-face)
    (,sollya-functions-regexp . font-lock-function-name-face)
    (,sollya-builtin-regexp . font-lock-builtin-face)
    (,sollya-keywords-regexp . font-lock-keyword-face)
    (,sollya-strings . font-lock-string-face)
    (,sollya-function-name . font-lock-function-name-face)
    "Minimal highlighting expressions for `sollya-mode'."
      ))

;; (defconst sollya-font-lock-keywords
;;   `(
;;    ;; highlight all the reserved commands.
;;    (,(concat "\\_<" (regexp-opt sollya-keywords) "\\_>") . font-lock-builtin-face)
;;    ("\\('\\w*'\\)" . font-lock-variable-name-face)
;;    ("\\(\\([~?]\\|\\<\\)[a-z][a-zA-Z0-9_']*:\\)[^=]" 1 font-lock-variable-name-face)
;;    ;; ("//.*" . font-lock-comment-face)
;;    )
;;   "Minimal highlighting expressions for `sollya-mode'.")

(defvar sollya-mode-syntax-table nil
  "Syntax table for `sollya-mode'")

(if sollya-mode-syntax-table
    ()
  (setq sollya-mode-syntax-table
	(let ((st (make-syntax-table)))
	  (modify-syntax-entry ?_ "w" st)
	  (modify-syntax-entry ?\/ ". 124" st)
	  (modify-syntax-entry ?* ". 23b" st)
	  (modify-syntax-entry ?\n ">" st)     
	  st)))

;;;###autoload
(define-derived-mode sollya-mode prog-mode 
  "Sollya-mode"
  "Major mode for editing Sollya files and using `run-sollya'.

\\<sollya-mode-map>"
  (use-local-map sollya-mode-map)

  (setq major-mode 'sollya-mode)
  (setq mode-name "Sollya")

  (setq comment-start "\\*")
  (make-local-variable 'comment-end)
  (setq comment-end "*\\")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+ *")

  ;; make the buffer read only.
  ;; a contentious subject as some prefer the buffer to be overwritable.
  ;; (set (make-local-variable 'font-lock-defaults) '(sollya-font-lock-keywords t))

  (set (make-local-variable 'font-lock-defaults)
       '(sollya-font-lock-keywords
         nil ;; keywords-only
         nil ;; case-fold
         ((?_ . "w")     
          (?\/ . ". 124")
          (?* . ". 23b") 
  	  (?\n . ">")))
       )
  (set-syntax-table sollya-mode-syntax-table)
  (run-hooks 'sollya-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sol\\'" . sollya-mode))
(add-to-list 'auto-mode-alist '("\\.sollya\\'" . sollya-mode))

(provide 'sollya)
;;; sollya.el ends here
