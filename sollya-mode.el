;;; sollya_mode.el --- 
;; 
;; Filename: sollya_mode.el
;; Description: 
;; Author: Mohamed Amine Najahi
;; Maintainer: 
;; Created: Thu Jun 25 17:56:02 2015 (+0200)
;; Version: 
;; Package-Requires: ()
;; Last-Updated: Thu Jun 25 18:49:36 2015 (+0200)
;;           By: Mohamed Amine Najahi
;;     Update #: 9
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

;; (executable-find "sollya")
(defvar sollya-cli-file-path "/usr/local/bin/sollya"
  "Path to the program used by `run-sollya'")

(defvar sollya-cli-arguments '()
  "Commandline arguments to pass to `sollya-cli'")

(defvar sollya-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `run-sollya'")

(defvar sollya-prompt-regexp "^\\(?:\\[[^@]+@[^@]+\\]\\)"
  "Prompt for `run-sollya'.")

(defun run-sollya ()
  "Run an inferior instance of `sollya-cli' inside Emacs."
  (interactive)
  (let* ((sollya-program sollya-cli-file-path)
         (buffer (comint-check-proc "Sollya")))
    ;; pop to the "*Sollya*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'sollya-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*Sollya*"))
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "Sollya" buffer
             sollya-program sollya-cli-arguments)
      (sollya-mode))))

(defun sollya--initialize ()
  "Helper function to initialize Sollya"
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(defconst sollya-keywords
  '("abs" "absolute" "accurateinfnorm" "acos" "acosh" "asciiplot" "asin" "asinh" "atan" "atanh" "autodiff" "autosimplify" "bashexecute" "begin" "binary" "bind" "boolean" "by" "canonical" "ceil" "chebyshevform" "checkinfnorm" "coeff" "composepolynomials" "constant" "cos" "cosh" "decimal" "default" "degree" "denominator" "diam" "dieonerrormode" "diff" "dirtyfindzeros" "dirtyinfnorm" "dirtyintegral" "display" "do" "double" "doubledouble" "doubleextended" "dyadic" "else" "end" "erf" "erfc" "error" "evaluate" "execute" "exp" "expand" "expm1" "exponent" "externalplot" "externalproc" "false" "file" "findzeros" "fixed" "floating" "floor" "for" "fpfindzeros" "fpminimax" "from" "fullparentheses" "function" "getsuppressedmessages" "guessdegree" "head" "hexadecimal" "honorcoeffprec" "hopitalrecursions" "horner" "if" "implementpoly" "implementconstant" "in" "inf" "infnorm" "integer" "integral" "isbound" "isevaluable" "length" "library" "libraryconstant" "list" "log" "log10" "log1p" "log2" "mantissa" "max" "mid" "midpointmode" "min" "nearestint" "numberroots" "nop" "numerator" "object" "of" "off" "on" "parse" "perturb" "pi" "plot" "points" "postscript" "postscriptfile" "powers" "prec" "precision" "print" "printbinary" "printdouble" "printexpansion" "printfloat" "printhexa" "printsingle" "printxml" "proc" "procedure" "quit" "range" "rationalapprox" "rationalmode" "readfile" "readxml" "relative" "remez" "rename" "restart" "return" "revert" "round" "roundcoefficients" "roundcorrectly" "roundingwarnings" "simplifysafe" "searchgal" "showmessagenumbers" "simplify" "simplifysafe" "sin" "single" "sinh" "sort" "sqrt" "string" "subpoly" "substitute" "sup" "supnorm" "suppressmessage" "tail" "tan" "tanh" "taylor" "taylorform" "taylorrecursions" "then" "time" "timing" "to" "tripledouble" "true" "unsuppressmessage" "var" "verbosity" "version" "void" "while" "worstcase" "write" "zerodenominators"))

(defvar sollya-font-lock-keywords
  (list
   ;; highlight all the reserved commands.
   `(,(concat "\\_<" (regexp-opt sollya-keywords) "\\_>") . font-lock-keyword-face))
  "Additional expressions to highlight in `sollya-mode'.")

(define-derived-mode sollya-mode comint-mode "Sollya"
  "Major mode for `run-sollya'.

\\<sollya-mode-map>"
  nil "Sollya"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp sollya-prompt-regexp)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'font-lock-defaults) '(sollya-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) sollya-prompt-regexp))

;; this has to be done in a hook. grumble grumble.
(add-hook 'sollya-mode-hook 'sollya--initialize)


(set (make-local-variable 'font-lock-defaults) '(sollya-font-lock-keywords t))

(provide 'sollya-mode)

