;; Fix for Emacs 24.3.50.1; see
;; <https://github.com/eschulte/emacs24-starter-kit/issues/30> and
;; <https://github.com/technomancy/emacs-starter-kit/issues/151>.
(require 'hippie-exp)

;;;; ESK

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("ELPA" . "http://tromey.com/elpa/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;;; Missing: css-mode, bc, ess, fp, lilypond, maxima, php-repl,
;;; wikipedia-mode, xml-lite
(defvar my-packages '(
                      apache-mode
                      clojure-mode
                      dired+
                      dsvn
                      ess
                      full-ack
                      gnuplot
                      go-mode
                      graphviz-dot-mode
                      haskell-mode
                      htmlize
                      lua-mode
                      magit
                      markdown-mode
                      mediawiki
                      org
                      paredit
                      php-mode
                      prolog
                      python-mode
                      slime
                      slime-repl
                      smart-tab
                      starter-kit
                      starter-kit-bindings
                      starter-kit-js
                      starter-kit-lisp
                      unbound
                      undo-tree
                      yaml-mode
                      )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(setq visible-bell nil)
(delete-selection-mode t)
(setq column-number-mode t)

;;;; Miscellaneous

;;; We need this, after all?
(require 'ess-site)

;;; For makefiles, dot, SQL and such things.
(setq tab-width 4)
(setq-default tab-width 4)

;;; TODO: shell no longer tells me where we're executing.

;;; http://www.emacswiki.org/emacs/TabCompletion#toc2
(smart-tab-mode-on)

;;; Disable hl-line-mode in ESK; found here:
;;; <http://stackoverflow.com/questions/3545458/disable-hl-line-in-emacs-when-using-emacs-starter-kit>.
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

(setq version-control t)
(setq kept-new-versions 100)
(setq kept-old-versions 100)

;;; Should scroll to the bottom of *compilation*.
(setq compilation-scroll-output 1)

(setq user-mail-address "pcd@roxygen.org")

;;; Answer y-or-ns with enter.
(define-key query-replace-map (kbd "C-m") 'act)

;;; Prevent "Active processes exist" on exit; thanks, Jürgen Hötzel:
;;; <http://stackoverflow.com/a/2708042>.
(add-hook 'comint-exec-hook
          (lambda () (process-kill-without-query (get-buffer-process (current-buffer)) nil)))

;;; Hack to disable flyspell, which was freezing up when writing e.g.
;;; git commit-comments.
(eval-after-load "flyspell"
  '(defun flyspell-mode (&optional arg)))

;;;;; Bindings

(defun copy-region-to-clipboard ()
  (interactive)
  (shell-command-on-region (region-beginning)
                           (region-end)
                           "xsel -i -b"))

;;; Thanks, unbound; on the other hand, see
;;; <http://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html>:
;;; "Don't define C-c letter as a key in Lisp programs. Sequences
;;; consisting of C-c and a letter (either upper or lower case) are
;;; reserved for users; they are the only sequences reserved for
;;; users, so do not block them."
(global-set-key (kbd "C-c a") 'list-matching-lines)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c R") 'recompile)
(global-set-key (kbd "C-c s") 'svn-status)
(global-set-key (kbd "C-c x") 'copy-region-to-clipboard)
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "<up>") 'windmove-up)
(global-set-key (kbd "<down>") 'windmove-down)
(global-set-key (kbd "<right>") 'windmove-right)
(global-set-key (kbd "<left>") 'windmove-left)

;;;;; Auto-modes

(add-to-list 'auto-mode-alist '("\\.bsh\\'" . java-mode))
(add-to-list 'auto-mode-alist '("\\.bats\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("\\.egg-locations\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.meta\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.release-info\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.setup\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.shtml\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.stex\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.stumpwmrc\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.sxml\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . auto-fill-mode))
;;; Regex from <http://www.emacswiki.org/emacs/MuttInEmacs>.
(add-to-list 'auto-mode-alist '("/mutt" .
                                (lambda ()
                                  (footnote-mode)
                                  (auto-fill-mode)
                                  (turn-on-orgtbl))))

(setq vc-follow-symlinks t)

;;;; Language-specific things

;;;;; org-mode

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)
   (dot . t)
   (gnuplot . t)
   (R . t)))

(setq org-confirm-babel-evaluate nil)

;;; Thanks, Bernt Hansen: <http://doc.norang.ca/org-mode.html>.
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
(add-to-list 'org-src-lang-modes '("ditaa" . artist))

;;; See <http://orgmode.org/worg/org-issues.html> and
;;; <http://article.gmane.org/gmane.emacs.orgmode/33955>.
(defun org-babel-execute:ditaa (body params)
  "Execute a block of Ditaa code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((result-params (split-string (or (cdr (assoc :results params)) "")))
	 (out-file ((lambda (el)
		      (or el
			  (error
			   "ditaa code block requires :file header argument")))
		    (cdr (assoc :file params))))
	 (cmdline (cdr (assoc :cmdline params)))
	 (in-file (org-babel-temp-file "ditaa-"))
	 (cmd (concat "ditaa"
		      " " cmdline
		      " " (org-babel-process-file-name in-file)
		      " " (org-babel-process-file-name out-file))))
    (with-temp-file in-file (insert body))
    (message cmd) (shell-command cmd)
    nil))

;;; From
;;; <http://lists.gnu.org/archive/html/emacs-orgmode/2011-10/msg00304.html>;
;;; avoids `Invalid function: org-called-interactively-p' when doing
;;; `org-store-link'.

;;; This doesn't work for e.g. org-tangle:
(eval-after-load 'org-mode
  '(progn
     (defalias 'org-called-interactively-p 'called-interactively-p)))

(defalias 'org-called-interactively-p 'called-interactively-p)

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)" "CANCELED(c)")))

;;; Edit source code in the current window.
(setq org-src-window-setup 'current-window)

;;; Alternatively, `time'.
(setq org-log-done 'note)

;;; Open link in browser.
(setq org-return-follows-link t)

;;; Activate the shortcut keys specified in org-todo-keywords.
(setq org-use-fast-todo-selection t)

;;; Browse links in Opera.
(defun browse-url-opera (url &optional new-window)
  (setq url (browse-url-encode-url url))
  (start-process "opera"
                 nil
                 "opera"
                 "-newtab"
                 url))

(setq browse-url-browser-function 'browse-url-opera)

(define-skeleton org-mode-src-skel
  "Insert #+BEGIN_SRC <source>...#+END_SRC blocks."
  "Source: "
  >
  "#+BEGIN_SRC " str
  \n
  _
  \n
  "#+END_SRC"
  > \n)

(define-skeleton org-mode-example-skel
  "Insert #+BEGIN_EXAMPLE...#+END_EXAMPLE blocks."
  nil
  >
  "#+BEGIN_EXAMPLE"
  \n
  _
  \n
  "#+END_EXAMPLE"
  > \n)

(define-skeleton org-mode-quote-skel
  "Insert #+BEGIN_QUOTE...#+END_QUOTE blocks."
  nil
  >
  "#+BEGIN_QUOTE"
  \n
  _
  \n
  "#+END_QUOTE"
  > \n)

(add-hook
 'org-mode-hook
 (lambda ()
   (define-key org-mode-map (kbd "C-c C-x C-s") 'org-mode-src-skel)
   (define-key org-mode-map (kbd "C-c C-x C-q") 'org-mode-quote-skel)
   (define-key org-mode-map (kbd "C-c C-x C-e") 'org-mode-example-skel)

   ;; For LaTeX output, use no indentation but paragraph-skips by
   ;; default.
   (add-to-list 'org-export-latex-packages-alist '("" "parskip"))))

(add-hook
 'org-src-mode-hook
 (lambda ()
   (define-key org-src-mode-map (kbd "C-x C-s") 'org-edit-src-save)))

;;;;; Scheme

;;; Evaluate whole Scheme buffer.
(defun scheme-send-buffer ()
  "Just send the goddamn thing."
  (interactive)
  (scheme-send-region (point-min) (point-max)))

(defun sh-execute-buffer ()
  "Just send the goddamn thing."
  (interactive)
  (sh-execute-region (point-min) (point-max)))

(defun scheme-send-buffer-and-go ()
  "Send and go."
  (interactive)
  (scheme-send-buffer)
  (switch-to-buffer-other-window "*scheme*"))

(setq scheme-program-name "csi -n")

;;; Indent-functions for match
(put 'and-let* 'scheme-indent-function 1)
(put 'call-with-database 'scheme-indent-function 1)
(put 'call-with-sqlite3-connection 'scheme-indent-function 1)
(put 'call-with-values 'scheme-indent-function 1)
(put 'dotimes 'scheme-indent-function 1)
(put 'for-each 'scheme-indent-function 1)
(put 'handle-exceptions 'scheme-indent-function 1)
(put 'match 'scheme-indent-function 1)
(put 'match-lambda 'scheme-indent-function 1)
(put 'match-lambda* 'scheme-indent-function 1)
(put 'match-let 'scheme-indent-function 1)
(put 'match-let* 'scheme-indent-function 1)
(put 'match-letrec 'scheme-indent-function 1)
(put 'module 'scheme-indent-function 1)
(put 'parameterize 'scheme-indent-function 1)
(put 'receive 'scheme-indent-function 1)
(put 'regex-case 'scheme-indent-function 1)
(put 'set-read-syntax! 'scheme-indent-function 1)
(put 'ssql->sql 'scheme-indent-function 1)
(put 'thunk 'scheme-indent-function 1)
(put 'type-case 'scheme-indent-function 1)
(put 'type-case* 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'while 'scheme-indent-function 1)
(put 'with-working-directory 'scheme-indent-function 1)

;;;;;; mini-kanren
(put 'run 'scheme-indent-function 1)
(put 'run* 'scheme-indent-function 1)
(put 'fresh 'scheme-indent-function 1)
(put 'project 'scheme-indent-function 1)

;;; Unification (i.e. "identical to" as goal)
(font-lock-add-keywords
 'scheme-mode
 '(("(\\(==\\)\\>"
    (0 (prog1 ()
         (compose-region (match-beginning 1)
                         (match-end 1)
                         ?≡))))))

;;; Some way to program these so it maps e.g. `u' -> `ᵘ'.
(font-lock-add-keywords
 'scheme-mode
 '(("\\<\\(?:cond\\|if\\)\\(a\\)"
    (0 (prog1 ()
         (compose-region (match-beginning 1)
                         (match-end 1)
                         ?ᵃ))))))

(font-lock-add-keywords
 'scheme-mode
 '(("\\<\\(?:cond\\|if\\)\\(e\\)"
    (0 (prog1 ()
         (compose-region (match-beginning 1)
                         (match-end 1)
                         ?ᵉ))))))

(font-lock-add-keywords
 'scheme-mode
 '(("\\<\\(?:cond\\|if\\)\\(i\\)"
    (0 (prog1 ()
         (compose-region (match-beginning 1)
                         (match-end 1)
                         ?ⁱ))))))

(font-lock-add-keywords
 'scheme-mode
 '(("\\<\\(?:cond\\|if\\)\\(u\\)"
    (0 (prog1 ()
         (compose-region (match-beginning 1)
                         (match-end 1)
                         ?ᵘ))))))

;;; Would be nice to be able to generalize this.
(font-lock-add-keywords
 'scheme-mode
 '(("\\<\\(?:car\\|cdr\\|cons\\|list\\|null\\|pair\\)\\(o\\)"
    (0 (prog1 ()
         (compose-region (match-beginning 1)
                         (match-end 1)
                         ?ᵒ))))))

;;; The following doesn't work because the o is terminal at some point
;;; before the word is finished; leading to infix-os being
;;; font-locked.

;; (font-lock-add-keywords
;;  'scheme-mode
;;  '(("\\(o\\)\\>"
;;     (0 (prog1 ()
;;          (compose-region (match-beginning 1)
;;                          (match-end 1)
;;                          ?ᵒ))))))

;;;;; Paredit

;; Stop SLIME's REPL from grabbing DEL, which is annoying when
;; backspacing over a '('; from
;; <http://www.emacswiki.org/emacs/ParEdit#toc3>.
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

(defun paredit-plus-one ()
  (paredit-mode +1))

(defun setup-lisp-like (mode-map eval-buffer)
  (paredit-plus-one)
  (define-key mode-map (kbd "C-c b") eval-buffer)
  (define-key mode-map (kbd "C-c B") 'scheme-send-buffer-and-go)

  ;; From <http://mumble.net/~campbell/scheme/style.txt>: "Not only
  ;; does this clarify the organization of the code, but readers of the
  ;; code can then navigate the code's structure with Outline Mode
  ;; commands such as `C-c C-f', `C-c C-b', `C-c C-u', and `C-c C-d'
  ;; (forward, backward, up, down, respectively, headings)."
  ;;
  ;; Doesn't seem to work, though.
  (setq outline-regexp "\f\n;;;;+ ")

  (outline-minor-mode)
  (define-key mode-map (kbd "C-c C-n") 'outline-next-visible-heading)
  (define-key mode-map (kbd "C-c C-p") 'outline-previous-visible-heading)
  (define-key mode-map (kbd "C-c C-f") 'outline-forward-same-level)
  (define-key mode-map (kbd "C-c C-b") 'outline-backward-same-level)
  (define-key mode-map (kbd "C-c C-u") 'outline-up-heading))

(defun setup-scheme ()
  (setup-lisp-like scheme-mode-map 'scheme-send-buffer))

(defun setup-clojure ()
  (setup-lisp-like clojure-mode-map 'scheme-send-buffer))

(defun setup-slime ()
  (setup-lisp-like slime-mode-map 'slime-eval-buffer))

(defun setup-emacs-lisp ()
  (setup-lisp-like emacs-lisp-mode-map 'scheme-eval-buffer))

(defun setup-lisp ()
  (setup-lisp-like lisp-mode-map 'slime-eval-buffer))

(add-hook 'scheme-mode-hook 'setup-scheme)
(add-hook 'scheme-mode-hook
          (lambda ()
            (put 'and-let* 'scheme-indent-function 1)
            (put 'receive 'scheme-indent-function 1)))
(add-hook 'inferior-scheme-mode-hook 'paredit-plus-one)
(add-hook 'clojure-mode-hook 'setup-clojure)
(add-hook 'lisp-mode-hook 'setup-lisp)
(add-hook 'emacs-lisp-mode-hook 'setup-emacs-lisp)
(add-hook 'inferior-lisp-mode-hook 'paredit-plus-one)
(add-hook 'slime-mode-hook 'setup-slime)
(add-hook 'slime-repl-mode-hook 'paredit-plus-one)
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

(add-hook 'sh-mode-hook
          (lambda ()
            (define-key sh-mode-map (kbd "C-c b") 'sh-execute-buffer)))

;;;;; Latex

;;; Don't we need more scaffolding for xelatex?
(setq tex-dvi-view-command "evince")
;; (setq latex-run-command "xelatex -shell-escape")
(setq org-latex-to-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"))
(setq org-export-latex-default-packages-alist
      '(
        ;; ("AUTO" "inputenc" t)
        ;; ("T1" "fontenc" t)
        ("" "fixltx2e" nil)
        ("" "xltxtra" nil)
        ("" "fontspec" nil)
        ("" "graphicx" t)
        ("" "longtable" nil)
        ("" "float" nil)
        ("" "wrapfig" nil)
        ("" "soul" t)
        ("" "textcomp" t)
        ;; ("" "marvosym" t)
        ;; ("" "wasysym" t)
        ;; ("" "latexsym" t)
        ("" "amssymb" t)
        ("" "amsmath" t)
        ("xetex,pdfborder=0 0 0,colorlinks,linkcolor=blue,citecolor=blue,urlcolor=blue" "hyperref" nil)
        "\\tolerance=1000")
      )

;; (setq org-export-latex-classes
;;       ("article"
;;        "\\documentclass[11pt]{article}"
;;        ("\\section{%s}" . "\\section*{%s}")
;;        ("\\subsection{%s}" . "\\subsection*{%s}")
;;        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;        ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;        ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;; (eval-after-load 'tex-mode
;;   '(progn
;;      (defun tex-print (&optional alt)
;;        "Print the .dvi file made by \\[tex-region], \\[tex-buffer] or \\[tex-file].
;; Runs the shell command defined by `tex-dvi-print-command'. If prefix argument
;; is provided, use the alternative command, `tex-alt-dvi-print-command'."
;;        (interactive "P")
;;        (let ((print-file-name-dvi (tex-append tex-print-file ".pdf"))
;;              test-name)
;;          (if (and (not (equal (current-buffer) tex-last-buffer-texed))
;;                   (buffer-file-name)
;;                   ;; Check that this buffer's printed file is up to date.
;;                   (file-newer-than-file-p
;;                    (setq test-name (tex-append (buffer-file-name) ".pdf"))
;;                    (buffer-file-name)))
;;              (setq print-file-name-dvi test-name))
;;          (if (not (file-exists-p print-file-name-dvi))
;;              (error "No appropriate `.pdf' file could be found")
;;            (if (tex-shell-running)
;;                (tex-kill-job)
;;              (tex-start-shell))
;;            (tex-send-command
;;             (if alt tex-alt-dvi-print-command tex-dvi-print-command)
;;             print-file-name-dvi t))))))

;;;;; Graphviz

;;; Get rid of the irritating eight-wide indents.
(setq graphviz-dot-indent-width 2)

;;; Get rid of the irritating semi-colon behavior.
(setq graphviz-dot-auto-indent-on-semi nil)

;;;;; Clojure

;;; Indentation for λ (should we suffice with Emacs changing the
;;; face?)
(put 'defλ 'clojure-doc-string-elt 2)
(put 'defλ 'clojure-indent-function 'defun)
(put 'λ 'clojure-indent-function 'defun)
(put 'lambda 'clojure-indent-function 'defun)
(put 'let-macro-characters 'clojure-backtracking-indent '((2) 2))

;;; Face-support for λ; TODO: make a package for this?
(add-hook
 'clojure-mode-hook
 (lambda ()
   (font-lock-add-keywords
    nil
    `((,(concat "(\\(?:lambda.core/\\)?\\(defλ\\)\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                "\\(\\sw+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
      (,(concat "(\\(?:lambda.core/\\)?"
                (regexp-opt '("defλ"
                              "λ"
                              "lambda") t)
                "\\>"
                )
       1 font-lock-builtin-face)))))

;;;;; Slime

(add-hook
 'slime-mode-hook
 (lambda ()
   (setq slime-protocol-version 'ignore)
   (slime-setup '(slime-repl))
   (add-to-list 'slime-lisp-implementations
                '(sbcl ("/usr/local/bin/sbcl --noinform")))))
