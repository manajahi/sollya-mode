(require 'comint)
;(require 'sollya)

(defvar sollya-display-when-eval t
  "*If true, display the inferior sollya buffer when evaluating expressions.")

(defvar inferior-sollya-mode-hook nil
  "Hook run after entering `inferior-sollya-mode'.")

(defvar inferior-sollya-program "sollya"
  "*Program name for invoking an inferior sollya from Emacs.")

(defvar inferior-sollya-prompt-regexp "^\\(?:\\[[^@]+@[^@]+\\]\\)"
  "Prompt for `inferior-sollya-mode'.")

(defvar inferior-sollya-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\C-j" 'newline-and-indent)
    (define-key map "\t" 'completion-at-point)
    map)
  "Keymap used in `inferior-sollya-mode' buffers.")

(defun inferior-sollya-initialize ()
  "Helper function to initialize inferior-sollya."
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(add-hook 'inferior-sollya-mode-hook 'inferior-sollya-initialize)

(defun inferior-sollya-mode ()
  "Major mode for interacting with an inferior Sollya process.
Runs a Sollya as a subprocess of Emacs, with I/O through an
Emacs buffer. A history of input phrases is maintained. Phrases can
be sent from another buffer in sollya mode.

\\{inferior-caml-mode-map}"
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp inferior-sollya-prompt-regexp)
  (setq major-mode 'inferior-sollya-mode)
  (setq mode-name "Inferior Sollya")
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "\\*")
  (make-local-variable 'comment-end)
  (setq comment-end "*\\")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+ *")
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)
  (use-local-map inferior-sollya-mode-map)
  (run-hooks 'inferior-sollya-mode-hooks))


(defconst inferior-sollya-buffer-subname "inferior-sollya")
(defconst inferior-sollya-buffer-name
  (concat "*" inferior-sollya-buffer-subname "*"))

;; To show result of evaluation at toplevel
(defvar inferior-sollya-output nil)
(defun inferior-sollya-signal-output (s)
  (if (string-match "[^ ]" s) (setq inferior-sollya-output t)))

(defun inferior-sollya-mode-output-hook ()
  (set-variable 'comint-output-filter-functions
        (list (function inferior-sollya-signal-output))
        t))

(add-hook 'inferior-sollya-mode-hooks 'inferior-sollya-mode-output-hook)


;; To launch sollya whenever needed

(defun sollya-run-process-if-needed (&optional cmd)
  (if (comint-check-proc inferior-sollya-buffer-name) nil
    (if (not cmd)
        (if (comint-check-proc inferior-sollya-buffer-name)
            (setq cmd inferior-sollya-program)
          (setq cmd (read-from-minibuffer "Sollya toplevel to run: "
                                          inferior-sollya-program))))
    (setq inferior-sollya-program cmd)
    (let ((cmdlist (inferior-sollya-args-to-list cmd))
          (process-connection-type nil))
      (set-buffer (apply (function make-comint)
                         inferior-sollya-buffer-subname
                         (car cmdlist) nil (cdr cmdlist)))
      (inferior-sollya-mode)
      (display-buffer inferior-sollya-buffer-name)
      t)
    (setq sollya-shell-active t)
    ))


(defun run-sollya (&optional cmd)
  "Run an inferior sollya process.
Input and output via buffer `*inferior-sollya*'."
  (interactive
   (list (if (not (comint-check-proc inferior-sollya-buffer-name))
             (read-from-minibuffer "Sollya toplevel to run: "
                                   inferior-sollya-program))))
  (sollya-run-process-if-needed cmd)
  (switch-to-buffer-other-window inferior-sollya-buffer-name))


(defun inferior-sollya-args-to-list (string)
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
  "Send the current region to the inferior OCaml process."
  (interactive "r")
  (save-excursion (sollya-run-process-if-needed))
  (save-excursion
    (goto-char end)
    (sollya-skip-comments-backward)
    (comint-send-region inferior-sollya-buffer-name start (point))

    ;; AN : à revoir
    ;; normally, ";;" are part of the region
    (if (and (>= (point) 2)
             (prog2 (backward-char 2) (looking-at ";;")))
        (comint-send-string inferior-sollya-buffer-name "\n")
      (comint-send-string inferior-sollya-buffer-name ";;\n"))
    ;; the user may not want to see the output buffer
    (if sollya-display-when-eval
        (display-buffer inferior-sollya-buffer-name t))))


;; jump to errors produced by ocaml compiler
(defun inferior-sollya-goto-error (start end)
  "Jump to the location of the last error as indicated by inferior toplevel."
  (interactive "r")
  ;; (let ((loc (+ start
  ;;               (save-excursion
  ;;                 (set-buffer (get-buffer inferior-caml-buffer-name))
  ;;                 (re-search-backward
  ;;                  (concat comint-prompt-regexp
  ;;                          "[ \t]*Characters[ \t]+\\([0-9]+\\)-[0-9]+:$"))
  ;;                 (caml-string-to-int (match-string 1))))))
  ;;   (goto-char loc))
  )




;; (defun run-sollya ()
;;   "Run an inferior instance of `sollya-cli' inside Emacs."
;;   (interactive)
;;   (let* ((sollya-program sollya-cli-file-path)
;;          (buffer (comint-check-proc "Sollya")))
;;     ;; pop to the "*Sollya*" buffer if the process is dead, the
;;     ;; buffer is missing or it's got the wrong mode.
;;     (pop-to-buffer-same-window
;;      (if (or buffer (not (derived-mode-p 'sollya-mode))
;;              (comint-check-proc (current-buffer)))
;;          (get-buffer-create (or buffer "*Sollya*"))
;;        (current-buffer)))
;;     ;; create the comint process if there is no buffer.
;;     (unless buffer
;;       (apply 'make-comint-in-buffer "Sollya" buffer
;;              sollya-program sollya-cli-arguments)
;;       (sollya-mode))))

;; ;; Use Sollya hook to set some comint variables
;; (add-hook 'sollya-mode-hook 'sollya--initialize)

;; (defun sollya--initialize ()
;;   "Helper function to initialize Sollya."
;;   (setq comint-process-echoes t)
;;   (setq comint-use-prompt-regexp t))

;; (define-derived-mode inf-sollya comint-mode "inf-sollya"
;;   "Major mode for editing Sollya files and using `run-sollya'.

;; \\<sollya-mode-map>"
;;   (set-syntax-table sollya-mode-syntax-table)
;;   (use-local-map sollya-mode-map)

;;   (setq major-mode 'sollya-mode)
;;   (setq mode-name "Sollya")

;;   (setq comment-start "\\*")
;;   (make-local-variable 'comment-end)
;;   (setq comment-end "*\\")
;;   (make-local-variable 'comment-column)
;;   (setq comment-column 40)
;;   (make-local-variable 'comment-start-skip)
;;   (setq comment-start-skip "(\\*+ *")

;;   ;; set up the prompt
;;   (setq comint-prompt-regexp sollya-prompt-regexp)

;;   ;; make the buffer read only.
;;   ;; a contentious subject as some prefer the buffer to be overwritable.
;;   (setq comint-prompt-read-only t)
;;   (set (make-local-variable 'font-lock-defaults) '(sollya-font-lock-keywords t))
;;   ;;  (set (make-local-variable 'paragraph-start) sollya-prompt-regexp)
;;   (run-hooks 'sollya-mode-hook))

;; ;;;###autoload
;; (add-to-list 'auto-mode-alist '("\\.sol\\'" . sollya-mode))
;; (add-to-list 'auto-mode-alist '("\\.sollya\\'" . sollya-mode))

(provide 'inf-sollya)

