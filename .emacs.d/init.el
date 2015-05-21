;; Fix for Emacs 24.3.50.1; see
;; <https://github.com/eschulte/emacs24-starter-kit/issues/30> and
;; <https://github.com/technomancy/emacs-starter-kit/issues/151>.
(require 'hippie-exp)

;;; Need for ad-hoc things.
(add-to-list 'load-path "~/.emacs.d/")

;;;; ESK

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("ELPA" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;;; Missing: css-mode, bc, ess, fp, lilypond, maxima, php-repl,
;;; wikipedia-mode, xml-lite
(defvar my-packages '(
                      apache-mode
                      clojure-mode
                      dired+
                      discord
                      dsvn
                      ess
                      full-ack
                      gnuplot
                      go-mode
                      graphviz-dot-mode
                      haskell-mode
                      htmlize
                      keyfreq
                      lua-mode
                      magit
                      markdown-mode
                      mediawiki
                      nrepl
                      openwith
                      org-plus-contrib
                      paredit
                      php-mode
                      prolog
                      python-mode
                      slime
                      slime-repl
                      smart-tab
                      sql-indent
                      starter-kit
                      starter-kit-bindings
                      starter-kit-js
                      starter-kit-lisp
                      typopunct
                      unbound
                      undo-tree
                      window-number
                      xclip
                      yaml-mode
                      )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(setq visible-bell nil)
(delete-selection-mode t)
(setq column-number-mode t)

;;; For ``Delete excess backup versions of /home/peter/.recentf?''
(setq delete-old-versions t)

;;;; Miscellaneous

;;; So that Emacs recognizes aliases when running commands.
(setq shell-file-name "zsh")
(setq shell-command-switch "-ic")

;;; Turn color on
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;; Also turn color on for ad-hoc commands (see
;;; <http://stackoverflow.com/questions/5819719/emacs-shell-command-output-not-showing-ansi-colors-but-the-code>).
(defadvice display-message-or-buffer (before ansi-color activate)
  "Process ANSI color codes in shell output."
  (let ((buf (ad-get-arg 0)))
    (and (bufferp buf)
         (string= (buffer-name buf) "*Shell Command Output*")
         (with-current-buffer buf
           (ansi-color-apply-on-region (point-min) (point-max))))))

;;; Ox-ravel, for creating Rnw from org-mode.
(require 'ox-ravel)

;;; xclip-mode
(require 'xclip)
(turn-on-xclip)

;;; Why do we need this suddenly?
;; (normal-erase-is-backspace-mode 1)

;;; Openwith; thanks, Victor Deryagin:
;;; <http://stackoverflow.com/a/6845470>.

(openwith-mode t)
(setf openwith-associations
  '(("\\.pdf\\'" "evince" (file))
    ("\\.mp3\\'" "mplayer" (file))
    ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "mplayer" ("-idx" file))
    ("\\.\\(?:jp?g\\|png\\)\\'" "sxiv" (file))))

;;; Normal comments in Javascript, despite the fact that we use
;;; paredit
(add-hook 'js-mode-hook
  (lambda ()
    (define-key paredit-mode-map (kbd "M-;") 'comment-dwim)))

;;; Add Discordian date to other-dates
(defadvice calendar-other-dates
  (after calendar-other-dates-with-discordian)
  (push (format "Discordian date: %s"
                (calendar-discordian-date-string date))
        ad-return-value))

(eval-after-load "calendar"
  '(progn
     (require 'discord)
     (ad-activate 'calendar-other-dates)))

(add-hook 'calendar-mode-hook
  (lambda ()
    (define-key calendar-mode-map (kbd "p D")
      (lambda ()
        (interactive)
        (calendar-discordian-print-date)))))

;;; Calendar, for sunrise and sunset
(setq calendar-latitude 34.1)
(setq calendar-longitude -118.2)
(setq calendar-location-name "Los Angeles, CA")

;;;;; Occur

;;; Rename the occur buffer.
(add-hook 'occur-hook
  (lambda ()
    ;; Follows automatically in the buffer.
    (next-error-follow-minor-mode)
    (occur-rename-buffer)))

;;; Thanks,
;;; <http://www.masteringemacs.org/articles/2011/07/20/searching-buffers-occur-mode/>.
(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

;;; Enable isearch-occur with C-o.
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;;; Convert an occur search into a multi-occur search from within
;;; occur.
(defun occur-multi-occur ()
  "Starts multi-occur for the current search term on all buffers with the first matching buffer's major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode
    (with-current-buffer (car (nth 2 occur-revert-arguments))
      major-mode))
   (car occur-revert-arguments)))

;;; Bind occur-multi-occur to m.
(define-key occur-mode-map "m" 'occur-multi-occur)

;;; Killing a line backwards; see
;;; <http://www.emacswiki.org/emacs/BackwardKillLine>.
(defun kill-line-backward (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

;;; Subword mode
(global-subword-mode 1)

;;; Window-number
(autoload 'window-number-mode "window-number"
  "A global minor mode that enables selection of windows according
to
numbers with the C-x C-j prefix.  Another mode,
`window-number-meta-mode' enables the use of the M- prefix."
  t)

(autoload 'window-number-meta-mode "window-number"
  "A global minor mode that enables use of the M- prefix to select
windows, use `window-number-mode' to display the window numbers in
the mode-line."
  t)

(window-number-mode 1)
(window-number-meta-mode 1)

;;; Keyfreq, for collecting keystroke statistics
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;;; Winner mode
(winner-mode 1)

;;; Windmove
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;;; Python
(defun python-send-buffer-and-go ()
  "Send the buffer to the inferior Python process.
Then switch to the process buffer."
  (interactive)
  (python-send-buffer)
  (python-switch-to-python t))

(add-hook 'python-mode-hook
  (lambda ()
    (define-key python-mode-map (kbd "C-c M-c") 'python-send-buffer-and-go)
    (define-key python-mode-map (kbd "C-c z")
      (lambda () (interactive) (python-switch-to-python t)))))

;;; SQL
(defun my-sql-save-history-hook ()
  (let ((lval 'sql-input-ring-file-name)
        (rval 'sql-product))
    (if (symbol-value rval)
        (let ((filename
               (concat "~/.emacs.d/sql/"
                       (symbol-name (symbol-value rval))
                       "-history.sql")))
          (set (make-local-variable lval) filename))
      (error
       (format "SQL history will not be saved because %s is nil"
               (symbol-name rval))))))

(eval-after-load "sql"
  '(load-library "sql-indent"))

(add-hook 'sql-interactive-mode-hook 'my-sql-save-history-hook)
(add-hook 'sql-mode-hook
  (lambda ()
    (define-key sql-mode-map (kbd "TAB") 'sql-indent-line)))

;; The executable is osql, but osql doesn't seem to pass things to
;; isql correctly.
(setq sql-ms-options '("--" "-w" "300" "-n"))

;;; Dired should grep case insensitively.
(setq find-grep-options "-q -i")
(add-hook 'dired-mode-hook
  (lambda ()
    (define-key dired-mode-map (kbd "F") 'dired-do-find-marked-files)))

;;; find-name-dired should run case-insensitively.
(setq read-file-name-completion-ignore-case t)

;;; Dired should reuse files when changing directories.
(diredp-toggle-find-file-reuse-dir 1)

;;; Insert the buffer-name when working with the minibuffer; thanks,
;;; polyglot: <http://stackoverflow.com/q/455345>.
(defun current-buffer-not-mini ()
  "Return current-buffer if current buffer is not the *mini-buffer*
  else return buffer before minibuf is activated."
  (if (not (window-minibuffer-p)) (current-buffer)
    (if (eq (get-lru-window) (next-window))
        (window-buffer (previous-window)) (window-buffer (next-window)))))

(define-key minibuffer-local-map
  (kbd "C-c TAB") (lambda () (interactive)
                    (insert (buffer-name (current-buffer-not-mini)))))

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

(define-key comint-mode-map (kbd "C-c C-z") nil)
(define-key comint-mode-map (kbd "C-c C-z .") 'browse-url-at-point)
(define-key comint-mode-map (kbd "C-c C-z b") 'browse-url-of-buffer)
(define-key comint-mode-map (kbd "C-c C-z r") 'browse-url-of-region)
(define-key comint-mode-map (kbd "C-c C-z u") 'browse-url)
(define-key comint-mode-map (kbd "C-c C-z v") 'browse-url-of-file)

;;; Hack to disable flyspell, which was freezing up when writing e.g.
;;; git commit-comments.
(eval-after-load "flyspell"
  '(defun flyspell-mode (&optional arg)))

;;;;; Bindings

;; (defun copy-region-to-clipboard ()
;;   (interactive)
;;   (shell-command-on-region (region-beginning)
;;                            (region-end)
;;                            "xsel -i -b"))

;;; Thanks, unbound; on the other hand, see
;;; <http://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html>:
;;; "Don't define C-c letter as a key in Lisp programs. Sequences
;;; consisting of C-c and a letter (either upper or lower case) are
;;; reserved for users; they are the only sequences reserved for
;;; users, so do not block them."
(global-set-key (kbd "C-c a") 'list-matching-lines)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c G") 'autogen)
(global-set-key (kbd "C-c f") 'find-grep-dired)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c L") 'google-lint)
(global-set-key (kbd "C-c n") 'find-name-dired)
(global-set-key (kbd "C-c o") 'occur)
(global-set-key (kbd "C-c O") 'multi-occur-in-this-mode)
(global-set-key (kbd "C-c r") 'rgrep)
(global-set-key (kbd "C-c R") 'recompile)
(global-set-key (kbd "C-c s") 'svn-status)
(global-set-key (kbd "C-c u") 'kill-line-backward)
(global-set-key (kbd "C-c U") 'rename-uniquely)
(global-set-key (kbd "C-c x") 'copy-region-to-clipboard)
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
;; From
;; <https://lists.gnu.org/archive/html/help-gnu-emacs/2006-08/msg00528.html>.
(global-set-key (kbd "C-c C-z .") 'browse-url-at-point)
(global-set-key (kbd "C-c C-z b") 'browse-url-of-buffer)
(global-set-key (kbd "C-c C-z r") 'browse-url-of-region)
(global-set-key (kbd "C-c C-z u") 'browse-url)
(global-set-key (kbd "C-c C-z v") 'browse-url-of-file)
(global-set-key (kbd "C-x TAB") 'indent-rigidly)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "<up>") 'windmove-up)
(global-set-key (kbd "<down>") 'windmove-down)
(global-set-key (kbd "<right>") 'windmove-right)
(global-set-key (kbd "<left>") 'windmove-left)

;;; Compensate for screen.
(define-key input-decode-map "\e[1;2A" [S-up])
(define-key input-decode-map "\e[1;2B" [S-down])
(define-key input-decode-map "\e[1;2D" [S-left])
(define-key input-decode-map "\e[1;2C" [S-right])

(define-key input-decode-map "\e[1;3A" [M-up])
(define-key input-decode-map "\e[1;3B" [M-down])
(define-key input-decode-map "\e[1;3D" [M-left])
(define-key input-decode-map "\e[1;3C" [M-right])

(define-key input-decode-map "\e[1;4A" [S-M-up])
(define-key input-decode-map "\e[1;4B" [S-M-down])
(define-key input-decode-map "\e[1;4D" [S-M-left])
(define-key input-decode-map "\e[1;4C" [S-M-right])

(define-key input-decode-map "\e[1;5A" [C-up])
(define-key input-decode-map "\e[1;5B" [C-down])
(define-key input-decode-map "\e[1;5D" [C-left])
(define-key input-decode-map "\e[1;5C" [C-right])

(define-key input-decode-map "\e[1;6A" [C-S-up])
(define-key input-decode-map "\e[1;6B" [C-S-down])
(define-key input-decode-map "\e[1;6D" [C-S-left])
(define-key input-decode-map "\e[1;6C" [C-S-right])

(define-key input-decode-map "\e[1;7A" [C-M-up])
(define-key input-decode-map "\e[1;7B" [C-M-down])
(define-key input-decode-map "\e[1;7D" [C-M-left])
(define-key input-decode-map "\e[1;7C" [C-M-right])

;;;;; Auto-modes

(add-to-list 'auto-mode-alist '("\\.bsh\\'" . java-mode))
(add-to-list 'auto-mode-alist '("\\.bats\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("\\.egg-locations\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.gss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.hy\\'" . scheme-mode))
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

;;;;; Clojure

(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)

;;;;; org-mode

(require 'org-special-blocks)

;;; Stuff to publish the daybook.
(load "~/prg/org/daybook/daybook.el")

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

(defun browse-url-elinks (url &optional new-window)
  (setq url (browse-url-encode-url url))
  (start-process "elinks"
                 nil
                 "elinks"
                 "\"openurl("
                 url
                 "\", new-tab)"))

(defun browse-url-conkeror (url &optional new-window)
  (setq url (browse-url-encode-url url))
  (start-process "conkeror"
                 nil
                 "conkeror"
                 url))

(defun browse-url-conkeror (url &optional new-window)
  (setq url (browse-url-encode-url url))
  (start-process "conkeror"
                 nil
                 "conkeror"
                 url))

(defun browse-url-chrome (url &optional new-window)
  (setq url (browse-url-encode-url url))
  (start-process "google-chrome"
                 nil
                 "google-chrome"
                 url))

(setq browse-url-browser-function 'browse-url-chrome)

(defun browse-lucky (start end)
  (interactive "r")
  (let ((q (buffer-substring-no-properties start end)))
    (browse-url (concat "http://www.google.com/search?btnI&q="
                        (url-hexify-string q)))))

(define-skeleton org-mode-src-skel-with-tangle
  "Insert #+BEGIN_SRC <source>...#+END_SRC blocks with :tangle."
  nil
  > "#+BEGIN_SRC " (skeleton-read "Source: ")
  (let ((tangle (skeleton-read "Tangle: ")))
    (if (string= "" tangle)
        ""
      (concat " :tangle " tangle)))
  \n
  _
  \n
  "#+END_SRC"
  > \n)

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
    (define-key org-mode-map (kbd "C-c C-x C-s") 'org-mode-src-skel-with-tangle)
    (define-key org-mode-map (kbd "C-c C-x C-q") 'org-mode-quote-skel)
    (define-key org-mode-map (kbd "C-c C-x C-e") 'org-mode-example-skel)

    ;; For LaTeX output, use no indentation but paragraph-skips by
    ;; default.
    ;; (add-to-list 'org-export-latex-packages-alist '("" "parskip"))

    ;; Let's do auto-quotes, dashes, &c.; see:
    ;; <http://www.emacswiki.org/emacs/TypographicalPunctuationMarks>.
    (require 'typopunct)
    (typopunct-change-language 'english t)
    (typopunct-mode 1)

    ;; Wrap selected text in smart quotes.
    (defadvice typopunct-insert-quotation-mark (around wrap-region activate)
      (let* ((lang (or (get-text-property (point) 'typopunct-language)
                       typopunct-buffer-language))
             (omark (if single
                        (typopunct-opening-single-quotation-mark lang)
                      (typopunct-opening-quotation-mark lang)))
             (qmark (if single
                        (typopunct-closing-single-quotation-mark lang)
                      (typopunct-closing-quotation-mark lang))))
        (cond
         (mark-active
          (let ((skeleton-end-newline nil)
                (singleo (typopunct-opening-single-quotation-mark lang))
                (singleq (typopunct-closing-single-quotation-mark lang)))
            (if (> (point) (mark))
                (exchange-point-and-mark))
            (save-excursion
              (while (re-search-forward (regexp-quote (string omark)) (mark) t)
                (replace-match (regexp-quote (string singleo)) nil nil)))
            (save-excursion
              (while (re-search-forward (regexp-quote (string qmark)) (mark) t)
                (replace-match (regexp-quote (string singleq)) nil nil)))
            (skeleton-insert (list nil omark '_ qmark) -1)))
         ((looking-at (regexp-opt (list (string omark) (string qmark))))
          (forward-char 1))
         (t ad-do-it))))

    (defun org-export-html-format-image (src par-open)
      "Create image tag with source and attributes."
      (save-match-data
        (if (string-match (regexp-quote "ltxpng/") src)
            (format "<img src=\"%s\" alt=\"%s\"/>"
                    src (org-find-text-property-in-string 'org-latex-src src))
          (let* ((caption (org-find-text-property-in-string 'org-caption src))
                 (attr (org-find-text-property-in-string 'org-attributes src))
                 (label (org-find-text-property-in-string 'org-label src)))
            ;; (setq caption (and caption (org-html-do-expand caption)))
            (concat
             (if caption
                 (format "%s<div %sclass=\"figure\"><p>"
                         (if org-par-open "</p>\n" "")
                         (if label (format "id=\"%s\" " (org-solidify-link-text label)) "")))
             (format "<img src=\"%s\"%s />"
                     src
                     (if (string-match "\\<alt=" (or attr ""))
                         (concat " " attr )
                       (concat " " attr " alt=\"" src "\"")))
             (if caption
                 (format "</p>%s</div>%s"
                         (concat "\n<p>" caption "</p>")
                         (if org-par-open "\n<p>" ""))))))))))

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
(put 'add-hook 'lisp-indent-function 1)
(put 'bind-lambda 'lisp-indent-function 1)
(put 'call-with-database 'scheme-indent-function 1)
(put 'call-with-sqlite3-connection 'scheme-indent-function 1)
(put 'call-with-values 'scheme-indent-function 1)
(put 'dotimes 'scheme-indent-function 1)
(put 'for 'scheme-indent-function 1)
(put 'for-each 'scheme-indent-function 1)
(put 'handle-exceptions 'scheme-indent-function 1)
(put 'hash-table-walk 'scheme-indent-function 1)
(put 'match 'scheme-indent-function 1)
(put 'match-lambda 'scheme-indent-function 0)
(put 'match-lambda* 'scheme-indent-function 0)
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
(put 'until 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'while 'scheme-indent-function 1)
(put 'with 'scheme-indent-function 1)
(put 'with-mutex-locked 'scheme-indent-function 1)
(put 'with-primitive-procedures 'scheme-indent-function 1)
(put 'with-require 'scheme-indent-function 1)
(put 'with-semaphore-acquired 'scheme-indent-function 1)
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
;; (font-lock-add-keywords
;;  'scheme-mode
;;  '(("\\<\\(?:cond\\|if\\)\\(a\\)"
;;     (0 (prog1 ()
;;          (compose-region (match-beginning 1)
;;                          (match-end 1)
;;                          ?ᵃ))))))

;; (font-lock-add-keywords
;;  'scheme-mode
;;  '(("\\<\\(?:cond\\|if\\)\\(e\\)"
;;     (0 (prog1 ()
;;          (compose-region (match-beginning 1)
;;                          (match-end 1)
;;                          ?ᵉ))))))

;; (font-lock-add-keywords
;;  'scheme-mode
;;  '(("\\<\\(?:cond\\|if\\)\\(i\\)"
;;     (0 (prog1 ()
;;          (compose-region (match-beginning 1)
;;                          (match-end 1)
;;                          ?ⁱ))))))

;; (font-lock-add-keywords
;;  'scheme-mode
;;  '(("\\<\\(?:cond\\|if\\)\\(u\\)"
;;     (0 (prog1 ()
;;          (compose-region (match-beginning 1)
;;                          (match-end 1)
;;                          ?ᵘ))))))

;; ;;; Would be nice to be able to generalize this.
;; (font-lock-add-keywords
;;  'scheme-mode
;;  '(("\\<\\(?:car\\|cdr\\|cons\\|list\\|null\\|pair\\)\\(o\\)"
;;     (0 (prog1 ()
;;          (compose-region (match-beginning 1)
;;                          (match-end 1)
;;                          ?ᵒ))))))

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
  (paredit-mode +1)
  ;; Rescue Taylor's more reasonable defaults from the starter-kit's
  ;; bindings.
  (define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-round)
  (define-key paredit-mode-map (kbd "M-)") 'paredit-close-round-and-newline))

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
;; (setq tex-dvi-view-command "xdvi")
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

;;; Set an external viewer.
(setq graphviz-dot-view-command "display %s")

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

;;; Magit

;; Can't see green-on-blue, for some reason; from
;; <http://readystate4.com/2011/02/22/emacs-changing-magits-default-diff-colors/>.
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (when (not window-system)
       (set-face-background 'magit-item-highlight "white")
       (set-face-background 'magit-tag "black"))))

;; http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
                    (set-buffer-modified-p nil))))))

(let ((host-file (format "~/.emacs.d/%s.el" system-name)))
  (if (file-exists-p host-file)
      (load host-file)))
