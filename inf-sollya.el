;;; inf-sollya.el --- Sollya inf mode

;;; Commentary:
;; 

(require 'comint)
(require 'sollya-fonts)

;;; Code:

(defvar sollya-display-when-eval t
  "*If true, display the inferior sollya buffer when evaluating expressions.")

(defvar inferior-sollya-mode-hook nil
  "Hook run after entering `inferior-sollya-mode'.")

(defvar inferior-sollya-program "sollya"
  "Program name for invoking an inferior sollya from Emacs.")

(defvar inferior-sollya-prompt-regexp "^> ?"
  "Prompt for `inferior-sollya-mode'.")

(defvar inferior-sollya-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\C-c\C-i" 'sollya-interrupt-sollya)
    (define-key map "\C-c\C-k" 'sollya-kill-sollya)
    ;; (define-key map "\C-m" 'sollya-interactive-send-input)
    ;; (define-key map "\C-j" 'sollya-interactive-send-input-or-indent)
    ;; (define-key map "\M-\C-m" 'sollya-interactive-send-input-end-of-phrase)
    (define-key map "\t" 'completion-at-point)
    map)
  "Keymap used in `inferior-sollya-mode' buffers.")

(defun inferior-sollya-initialize ()
  "Helper function to initialize inferior-sollya."
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t)
  nil
  )

(add-hook 'inferior-sollya-mode-hook 'inferior-sollya-initialize)

(defun inferior-sollya-mode ()
  "Major mode for interacting with an inferior Sollya process.
Runs a Sollya as a subprocess of Emacs, with I/O through an
Emacs buffer.  A history of input phrases is maintained.  Phrases can
be sent from another buffer in sollya mode.

\\{inferior-caml-mode-map}"
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp inferior-sollya-prompt-regexp)
  (setq major-mode 'inferior-sollya-mode)
  (setq mode-name "Inferior-Sollya")

  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (use-local-map inferior-sollya-mode-map)

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
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)
  (run-hooks 'inferior-sollya-mode-hook)
  )


(defconst inferior-sollya-buffer-subname "inferior-sollya")
(defconst inferior-sollya-buffer-name
  (concat "*" inferior-sollya-buffer-subname "*"))

;; To show result of evaluation at toplevel
;; (defvar inferior-sollya-output nil)

;; (defun inferior-sollya-signal-output (s)
;;   (if (string-match "[^ ]" s) (setq inferior-sollya-output t)))

;; (defun inferior-sollya-mode-output-hook ()
;;   (set-variable 'comint-output-filter-functions
;;         (list (function inferior-sollya-signal-output))
;;         t))

;; (add-hook 'inferior-sollya-mode-hooks 'inferior-sollya-mode-output-hook)


(defun sollya-run-process-if-needed (&optional cmd)
  "Run the CMD process if not already done."
  (if (comint-check-proc inferior-sollya-buffer-name) nil
    (if (not cmd)
        (if (comint-check-proc inferior-sollya-buffer-name)
            (setq cmd inferior-sollya-program)
          (setq cmd (read-from-minibuffer "Sollya toplevel to run: "
                                          inferior-sollya-program))))
    (setq inferior-sollya-program cmd)
    (let ((cmdlist (inferior-sollya-args-to-list cmd)))
          ;; (process-connection-type nil))
      ;; (set-buffer (apply (function make-comint)
      ;;                    inferior-sollya-buffer-subname
      ;;                    (car cmdlist) nil (cdr cmdlist)))
      (set-buffer (apply (function make-comint)
                         inferior-sollya-buffer-subname
                         (car cmdlist) nil))
      (inferior-sollya-mode)
      (display-buffer inferior-sollya-buffer-name)
      t)
    (setq sollya-shell-active t)
    ))


(defun run-sollya ()
  "Run an inferior sollya process.
Input and output via buffer `*inferior-sollya*'."
  (interactive)
  (sollya-run-process-if-needed inferior-sollya-program)
  (switch-to-buffer-other-window inferior-sollya-buffer-name))


(defun inferior-sollya-args-to-list (string)
  "Transform the arguments given to launch the prompt inside STRING into a list."
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
          ((not (= where 0))
           (cons (substring string 0 where)
                 (inferior-sollya-args-to-list (substring string (+ 1 where)
							  (length string)))))
          (t (let ((pos (string-match "[^ \t]" string)))
               (if (null pos)
                   nil
                 (inferior-sollya-args-to-list (substring string pos
							  (length string)))))))))


(defun inferior-sollya-show-subshell ()
  "Show the inferior-sollya buffer."
  (interactive)
  (sollya-run-process-if-needed)
  (display-buffer inferior-sollya-buffer-name)

  ; Added to move the point of inferior-sollya to end of buffer
  (let ((buf (current-buffer))
        (sollya-buf  (get-buffer inferior-sollya-buffer-name))
        (count 0))
    (while
        (and (< count 10)
             (not (equal (buffer-name (current-buffer))
                         inferior-sollya-buffer-name)))
      (next-multiframe-window)
      (setq count (+ count 1)))
    (if  (equal (buffer-name (current-buffer))
                inferior-sollya-buffer-name)
        (goto-char (point-max)))
    (while
        (> count 0)
      (previous-multiframe-window)
      (setq count (- count 1)))
    )
  )


;; patched to move cursor after evaluation
(defun inferior-sollya-eval-region (start end)
  "Send the current region to the inferior sollya process."
  (interactive "r")
  (save-excursion (sollya-run-process-if-needed))
  (save-excursion
    (goto-char end)
    ;; (sollya-skip-comments-backward)
    (comint-send-region inferior-sollya-buffer-name start (point))

    ;; normally, ";" are part of the region
    ;; (if (and (>= (point) 1)
    ;;          (prog2 (backward-char 1) (looking-at ";")))
    ;;     (comint-send-string inferior-sollya-buffer-name "\n")
    ;;   (comint-send-string inferior-sollya-buffer-name "\n"))
    (comint-send-string inferior-sollya-buffer-name "\n")
    ;; the user may not want to see the output buffer
    (if sollya-display-when-eval
        (display-buffer inferior-sollya-buffer-name t))))

(defun sollya-interrupt-sollya ()
  "Interrupt the Sollya process."
  (interactive)
  (when (comint-check-proc inferior-sollya-buffer-name)
    (with-current-buffer inferior-sollya-buffer-name
      (comint-interrupt-subjob))))

(defun sollya-kill-sollya ()
  "Kill the Sollya process."
  (interactive)
  (when (comint-check-proc inferior-sollya-buffer-name)
    (with-current-buffer inferior-sollya-buffer-name
      (comint-kill-subjob))))


(provide 'inf-sollya)
;;; inf-sollya.el ends here
