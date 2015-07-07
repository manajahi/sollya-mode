;;; sollya.el ---
;; 
;; Filename: sollya.el
;; Description:
;; Author: Mohamed Amine Najahi
;; Maintainer:
;; Created: Mon Jun 29 13:50:25 2015 (+0200)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Tue Jul  7 13:24:22 2015 (+0200)
;;           By: Mohamed Amine Najahi
;;     Update #: 166
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

;;; Code:

(require 'inf-sollya)

(defvar sollya-shell-active nil
  "Non nil when a subshell is running.")

(defvar sollya-mode-hook nil
  "Hook run after entering `sollya-mode'.")

(defvar sollya-mode-map
  (let ((map (make-keymap)))
    map)
  "Keymap used in `sollya-mode' buffers.")


;; (defvar sollya-mode-syntax-table nil
;;   "Syntax table for `sollya-mode'")

;; (if sollya-mode-syntax-table
;;     ()
;;   (setq sollya-mode-syntax-table
;; 	(let ((st (make-syntax-table)))
;; 	  (modify-syntax-entry ?_ "w" st)
;; 	  (modify-syntax-entry ?\/ ". 124" st)
;; 	  (modify-syntax-entry ?* ". 23b" st)
;; 	  (modify-syntax-entry ?\n ">" st)
;; 	  st)))

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

  (set (make-local-variable 'font-lock-defaults)
       '(sollya-font-lock-keywords
         nil ;; keywords-only
         nil ;; case-fold
         ((?_ . "w")
          (?\/ . ". 124")
          (?* . ". 23b")
  	  (?\n . ">"))
	 )
       )
  (run-hooks 'sollya-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sol\\'" . sollya-mode))
(add-to-list 'auto-mode-alist '("\\.sollya\\'" . sollya-mode))

(provide 'sollya)
;;; sollya.el ends here
