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
                      ace-jump-buffer
                      ace-jump-helm-line
                      ace-jump-mode
                      ace-jump-zap
                      ace-window
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
                      helm
                      helm-descbinds
                      helm-swoop
                      htmlize
                      keyfreq
                      lua-mode
                      magit
                      markdown-mode
                      mediawiki
                      multiple-cursors
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
                      use-package
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

;;; Ace-window

;; Redefine the action keys so we can select windows with the
;; home-row.
(setq aw-dispatch-alist
  '((?x aw-delete-window " Ace - Delete Window")
    (?m aw-swap-window " Ace - Swap Window")
    (?n aw-flip-window)
    (?v aw-split-window-vert " Ace - Split Vert Window")
    (?b aw-split-window-horz " Ace - Split Horz Window")
    (?i delete-other-windows " Ace - Maximize Window")
    (?o delete-other-windows)))

;; On second thought, let's use home-row keys which are not already
;; defined in aw-dispatch-list.
(setq aw-keys '(?a ?e ?u ?i ?d ?h ?t ?s))

;; Otherwise, the dimming makes the screens unreadable.
(eval-after-load "ace-window"
  '(progn
     (set-face-foreground 'aw-background-face "gray100")))

;;; Kill the region, so that it's available for yanking, instead of
;;; just deleting it.
(setq ajz/zap-function 'kill-region)

;;; Consider this if we don't use backwards very much; practice
;;; backwards, though.
(setq ajz/forward-only nil)

;;; Sort by closest instead of the ace-default.
(setq ajz/sort-by-closest t)

;;; Only pick the nearest 52 characters.
(setq ajz/52-character-limit t)

;;; Indent when issuing open-line; see e.g.
;;; <http://www.emacswiki.org/emacs/OpenNextLine> or
;;; <http://emacsredux.com/blog/2013/03/26/smarter-open-line/>.
(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

;;; Same thing for the line above; see e.g.
;;; <http://emacsredux.com/blog/2013/06/15/open-line-above/>.
(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

;;; Smarter-move-beginning-of-the-line combines back-to-indentation
;;; (M-m) and move-beginning-of-line (C-a); from
;;; <http://goo.gl/2Pr7I8>.
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;;; Copy filename to the clipboard; from
;;; <http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/>.
(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;;; Ace-jump-mode

;; Everything becomes invisible, otherwise; should we let emacs know
;; that we have a dark background, somehow?
;;
;; E.g. (set-variable 'frame-background-mode 'dark) doesn't seem to
;; work.
(eval-after-load "ace-jump-mode"
  '(progn
     (set-face-foreground 'ace-jump-face-background "gray100")))

;; Let's try using the home-keys, even though the author recommends
;; using more than 10.
;; (setq ace-jump-mode-move-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))

;;; Helm-mode
(helm-mode 1)

;; Fuzzy match
(setq helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match t
      helm-locate-fuzzy-match t
      helm-apropos-fuzzy-match t)

;; Add a hook in eshell-mode for command-history.
(add-hook 'eshell-mode-hook
  #'(lambda ()
      (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))

;; Comint command-history
(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)

;; Minibuffer command-history
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

;; Use ack.
(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

;; Man-page at point
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

;; Helm-descbinds
(helm-descbinds-mode)

;; Helm-swoop should save file after edit.
(setq helm-multi-swoop-edit-save t)

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
;; (require 'ox-ravel)

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
(bind-keys
 :map global-map
 ("<f1> a" . helm-apropos)
 ("C-<" . mc/mark-previous-like-this)
 ("C->" . mc/mark-next-like-this)
 ("C-a" . smarter-move-beginning-of-line)
 ("C-c ;" . comment-or-uncomment-region)
 ("C-c C-<" . mc/mark-all-like-this)
 ("C-c C-c" . mc/edit-lines)
 ("C-c C-h SPC" . helm-all-mark-rings)
 ;; <https://lists.gnu.org/archive/html/help-gnu-emacs/2006-08/msg00528.html>
 ("C-c C-z ." . browse-url-at-point)
 ("C-c C-z b" . browse-url-of-buffer)
 ("C-c C-z r" . browse-url-of-region)
 ("C-c C-z u" . browse-url)
 ("C-c C-z v" . browse-url-of-file)
 ("C-c G" . autogen)
 ("C-c L" . google-lint)
 ("C-c M-i" . helm-multi-swoop)
 ("C-c O" . multi-occur-in-this-mode)
 ("C-c R" . recompile)
 ("C-c SPC" . ace-jump-mode)
 ("C-c U" . rename-uniquely)
 ("C-c a" . list-matching-lines)
 ("C-c c" . compile)
 ("C-c f" . find-grep-dired)
 ("C-c h o" . helm-occur)
 ("C-c l" . org-store-link)
 ("C-c n" . find-name-dired)
 ("C-c o" . occur)
 ("C-c r" . rgrep)
 ("C-c s" . svn-status)
 ("C-c u" . kill-line-backward)
 ("C-c x" . copy-region-to-clipboard)
 ("C-h" . kill-whole-line)
 ("C-o" . smart-open-line-above)
 ("C-x C-f" . helm-find-files)
 ("C-x C-r" . revert-buffer)
 ("C-x M-i" . helm-multi-swoop-all)
 ("C-x SPC" . ace-jump-mode-pop-mark)
 ("C-x TAB" . indent-rigidly)
 ("C-x b" . helm-mini)
 ("C-x o" . ace-window)
 ("M-%" . query-replace-regexp)
 ("M-I" . helm-swoop-back-to-last-point)
 ("M-SPC" . set-rectangular-region-anchor)
 ("M-i" . helm-swoop)
 ("M-o" . smart-open-line)
 ("M-x" . helm-M-x)
 ("M-y" . helm-show-kill-ring)
 ("M-z" . ace-jump-zap-to-char)

 ("<up>" . windmove-up)
 ("<down>" . windmove-down)
 ("<right>" . windmove-right)
 ("<left>" . windmove-left))

(bind-key "M-i" 'helm-swoop-from-isearch isearch-mode-map)

(bind-key "M-p" 'previous-history-element minibuffer-local-map)

(require 'helm-swoop)
(bind-keys :map helm-swoop-map
           ("M-i" . 'helm-multi-swoop-all-from-helm-swoop)
           ("C-r" . 'helm-previous-line)
           ("C-s" . 'helm-next-line))
(bind-keys :map helm-multi-swoop-map
           ("C-r" . helm-previous-line)
           ("C-s" . helm-next-line))

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
                                  (auto-fill-mode 0)
                                  (turn-on-orgtbl))))

(setq vc-follow-symlinks t)

;;;; Language-specific things

;;;;; Clojure

(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)

;;;;; org-mode

;;; Stuff to publish the daybook.
(if (file-exists-p "~/prg/org/daybook/daybook.el")
    (load "~/prg/org/daybook/daybook.el"))

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

(defun browse-url-chrome (url &optional new-window)
  (setq url (browse-url-encode-url url))
  (start-process "google-chrome"
                 nil
                 "google-chrome"
                 url))

(setq browse-url-browser-function 'browse-url-conkeror)

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
(put 'add-hook 'lisp-indent-function 1)
(put 'and-let* 'scheme-indent-function 1)
(put 'bind-lambda 'scheme-indent-function 1)
(put 'bind-let 'scheme-indent-function 1)
(put 'bind-let* 'scheme-indent-function 1)
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
(put 'nwhile 'scheme-indent-function 1)
(put 'parameterize 'scheme-indent-function 1)
(put 'process-with-environment 'scheme-indent-function 1)
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
(put 'with-error-output-to-port 'scheme-indent-function 1)
(put 'with-input-from-download 'scheme-indent-function 1)
(put 'with-lazy-lists 'scheme-indent-function 1)
(put 'with-mutex-locked 'scheme-indent-function 1)
(put 'with-natural-language 'scheme-indent-function 1)
(put 'with-output-to-mail 'scheme-indent-function 1)
(put 'with-output-to-pipe 'scheme-indent-function 1)
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

(setq tex-dvi-view-command "mupdf")
(setq org-latex-pdf-process
      '("latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f %f"))

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

(setq magit-auto-revert-mode nil)

(setq magit-last-seen-setup-instructions "1.4.0")

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ac-html use-package yaml-mode xclip window-number undo-tree unbound typopunct starter-kit-lisp starter-kit-js starter-kit-bindings sql-indent smart-tab slime-repl python-mode php-mode p4 org-plus-contrib openwith multiple-cursors mediawiki markdown-mode lua-mode keyfreq htmlize helm-swoop helm-descbinds haskell-mode graphviz-dot-mode google go-mode gnuplot full-ack ess dsvn discord dired+ clojure-mode apache-mode ace-window ace-jump-zap ace-jump-helm-line ace-jump-buffer))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
