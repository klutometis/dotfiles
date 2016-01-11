(require 'package)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("ELPA" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;;; Bootstrap use-package.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;;; Use-package is not needed at runtime, apparently; see
;;; <https://github.com/jwiegley/use-package>.
(eval-when-compile
  (require 'use-package))

;;; Typed text replaces selection.
(delete-selection-mode t)

;;; Show the column-number in addition to the row-number in the
;;; status-bar.
(setq column-number-mode t)

;;; For ``Delete excess backup versions of /home/peter/.recentf?''
(setq delete-old-versions t)

;;; Use-package should always download the packages, if they don't
;;; exist.
(setq use-package-always-ensure t)

(use-package ace-jump-helm-line)

(use-package ace-jump-buffer)

(use-package ace-jump-mode
  :bind*  (("C-c SPC" . ace-jump-mode)
           ("C-x SPC" . ace-jump-mode-pop-mark))
  :config
  ;; Let's try using the home-keys, even though the author recommends
  ;; using more than 10.
  ;; 
  ;; (setq ace-jump-mode-move-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
  
  ;; Everything becomes invisible, otherwise; should we let emacs know
  ;; that we have a dark background, somehow?
  ;;
  ;; E.g. (set-variable 'frame-background-mode 'dark) doesn't seem to
  ;; work.
  (set-face-foreground 'ace-jump-face-background "gray100"))

(use-package ace-jump-zap
  :bind* ("M-z" . ace-jump-zap-to-char)
  :init
  ;; Kill the region, so that it's available for yanking, instead of
  ;; just deleting it.
  (setq ajz/zap-function 'kill-region)

  ;; Consider this if we don't use backwards very much; practice
  ;; backwards, though.
  (setq ajz/forward-only nil)

  ;; Sort by closest instead of the ace-default.
  (setq ajz/sort-by-closest t)

  ;; Only pick the nearest 52 characters.
  (setq ajz/52-character-limit t))

(use-package ace-window
  :bind ("C-x o" . ace-window)
  :init
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
  
  :config
  ;; Otherwise, the dimming makes the screens unreadable.
  (set-face-foreground 'aw-background-face "gray100"))

(use-package apache-mode
  :mode ("\\.conf\\'" . apache-mode))

(use-package auto-package-update
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "03:00")
  (setq auto-package-update-delete-old-versions t))

(use-package better-defaults
  :config
  ;;; Don't flash on bell, after all.
  (setq visible-bell nil))

(use-package browse-url
  :config
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

  (setq browse-url-browser-function 'browse-url-chrome)
  
  (defun browse-lucky (start end)
    (interactive "r")
    (let ((q (buffer-substring-no-properties start end)))
      (browse-url (concat "http://www.google.com/search?btnI&q="
                          (url-hexify-string q))))))

(use-package calendar
  :init
  (setq calendar-latitude 37.3894
        calendar-longitude -122.0819
        calendar-location-name "Mountain View, CA")
  
  :config
  ;; Add Discordian date to other-dates
  (defadvice calendar-other-dates
      (after calendar-other-dates-with-discordian)
    (push (format "Discordian date: %s"
                  (calendar-discordian-date-string date))
          ad-return-value))
  (use-package discord)
  (ad-activate 'calendar-other-dates)
  (add-hook 'calendar-mode-hook
    (lambda ()
      (bind-key "p D"
                (lambda ()
                  (interactive)
                  (calendar-discordian-print-date))
                calendar-mode-map))))

(use-package cl)

(use-package clojure-mode)

;;; TODO: Think about binding something tab-like to company-complete.
(use-package company
  :bind ("C-TAB" . company-complete)
  :config
  (global-company-mode)
  
  ;; Make company actually visible in the terminal; thanks, nsf!
  ;; <https://github.com/nsf/gocode/blob/master/emacs-company/README.md>.
  (custom-set-faces
   '(company-preview
     ((t (:foreground "darkgray" :underline t))))
   '(company-preview-common
     ((t (:inherit company-preview))))
   '(company-tooltip
     ((t (:background "lightgray" :foreground "black"))))
   '(company-tooltip-selection
     ((t (:background "steelblue" :foreground "white"))))
   '(company-tooltip-common
     ((((type x)) (:inherit company-tooltip :weight bold))
      (t (:inherit company-tooltip))))
   '(company-tooltip-common-selection
     ((((type x)) (:inherit company-tooltip-selection :weight bold))
      (t (:inherit company-tooltip-selection)))))
  
  (use-package helm-company
    :config
    (bind-key "C-:" 'helm-company company-mode-map)
    (bind-key "C-:" 'helm-company company-active-map)))

(use-package cc-mode
  :mode ("\\.bsh\\'" . java-mode)
  :config
  (setq fill-column 80))

;;; Let's try this, even though it's pretty aggressive.
(use-package column-enforce-mode
  :config
  (global-column-enforce-mode 1))

(use-package desktop
  :config
  (desktop-save-mode 1)
  (add-to-list 'desktop-globals-to-save 'kill-ring))

(use-package dired+
  :config
  ;; Dired should reuse files when changing directories.
  (diredp-toggle-find-file-reuse-dir 1))

(use-package dired-subtree
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))

(use-package discord)

(use-package eshell
  :config
  (add-hook 'eshell-mode-hook
    (lambda () (bind-key "C-c C-l" 'helm-eshell-history eshell-mode-map))))

(use-package ess
  :config
  (use-package julia-mode))

(use-package find-dired
  :bind (("C-c f" . find-grep-dired)
         ("C-c n" . find-name-dired))
  :init
  ;; Grep case-insensitively.
  (setq find-grep-options "-q -i")
  ;; Allow find to follow links.
  (setq find-program "find -L")
  :config
  ;; Find-name-dired should run case-insensitively.
  (setq read-file-name-completion-ignore-case t)

  (add-hook 'dired-mode-hook
    (lambda ()
      (bind-key "F" 'dired-do-find-marked-files dired-mode-map))))

(use-package flyspell
  :disabled t)

(use-package full-ack)

(use-package gnuplot)

(use-package graphviz-dot-mode
  :mode "\\.dot\\'"
  :config
  ;; Get rid of the irritating eight-wide indents.
  (setq graphviz-dot-indent-width 2)

  ;; Get rid of the irritating semi-colon behavior.
  (setq graphviz-dot-auto-indent-on-semi nil)

  ;; Set an external viewer.
  (setq graphviz-dot-view-command "display %s"))

(use-package grep
  :bind ("C-c r" . rgrep))

(use-package haskell-mode)

;;; TODO: Once <https://github.com/jwiegley/use-package/issues/121> is
;;; fixed, use :bind with :map (instead of bind-key in :config).
(use-package helm
  :bind (("<f1> a" . helm-apropos)
         ("C-c h o" . helm-occur)
         ;; Can't stand this, for some reason; let's head back to
         ;; ido. Helm-buffers-list can take a second or more with a
         ;; lot of buffers
         ;;
         ;; Actually, we'll bind it to C-x C-b; and keep
         ;; e.g. switch-to-buffer or ido-switch-buffers at C-x b.
         ;;
         ;; Third thought, we'll keep it vanilla switch-to-buffer;
         ;; which gets helmized.
         ;;
         ;; Other people have experienced this, too, with tramp:
         ;; <https://github.com/emacs-helm/helm/issues/749>.
         ;; 
         ;; ("C-x b" . switch-to-buffer)
         ;; 
         ;; You know what? I can't fucking stand helm-find-files,
         ;; either; let's go back to ido!
         ;; 
         ;; ("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring))
  :bind-keymap ("C-c C-l" . helm-minibuffer-history)
  :init
  ;; Fuzzy match
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-locate-fuzzy-match t
        helm-apropos-fuzzy-match t)

  :config
  (helm-mode 1)
  (bind-key "C-c C-l" 'helm-minibuffer-history minibuffer-local-map)
  ;; Use ack.
  (when (executable-find "ack-grep")
    (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
          helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))
  ;; Man-page at point
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages))

(use-package helm-descbinds
  :config
  (helm-descbinds-mode))

(use-package helm-swoop
  :bind (("M-i" . helm-multi-swoop-all)
         ("M-I" . helm-swoop-back-to-last-point))
  :init
  ;; Helm-swoop should save file after edit.
  (setq helm-multi-swoop-edit-save t)
  
  :config
  (bind-keys :map helm-swoop-map
             ("M-i" . 'helm-multi-swoop-all-from-helm-swoop)
             ("C-r" . 'helm-previous-line)
             ("C-s" . 'helm-next-line))
  (bind-keys :map helm-multi-swoop-map
             ("C-r" . helm-previous-line)
             ("C-s" . helm-next-line))
  (bind-key "M-i" 'helm-swoop-from-isearch isearch-mode-map))

(use-package hippie-exp)

(use-package htmlize)

(use-package ido
  :bind
  ;; Helm-buffers-list (i.e. helm-mini) was driving me fucking crazy
  ;; with remote files; let's go back to good old ido.
  (("C-x b" . ido-switch-buffer)
   ("C-x C-f" . ido-find-file)))

(use-package js
  :config
  ;; Normal comments in Javascript, despite the fact that we use
  ;; paredit
  (add-hook 'js-mode-hook
    (lambda ()
      (bind-key "M-;" 'comment-dwim paredit-mode-map))))

;;; Keyfreq, for collecting keystroke statistics
(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package lua-mode)

(use-package magit
  :bind ("C-c g" . magit-status)
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  ;; These doesn't appear to be valid anymore; maybe magit-diff-added
  ;; and magit-diff-removed?
  ;; 
  ;; (set-face-foreground 'magit-diff-add "green3")
  ;; (set-face-foreground 'magit-diff-del "red3")
  (unless window-system
    ;; Maybe magit-section-highlight
    ;; (set-face-background 'magit-item-highlight "white")
    ;; (set-face-background 'magit-tag "black")
    )
  (setq magit-auto-revert-mode nil))

(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook
    (lambda ()
      (typopunct-change-language 'english t)
      (typopunct-mode 1)
      (auto-fill-mode 1))))

(use-package mediawiki)

;;; Local package for everything that can't be done with use-package.
;;; 
;;; TODO: consider calling it bootstrap and placing towards the
;;; beginning?
(use-package miscellenea
  :load-path "lisp/"
  :ensure nil)

(use-package multiple-cursors
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-c C-c" . mc/edit-lines)
         ("M-SPC" . set-rectangular-region-anchor)))

;;; Openwith; thanks, Victor Deryagin:
;;; <http://stackoverflow.com/a/6845470>.
(use-package openwith
  :init
  (setf openwith-associations
  '(("\\.pdf\\'" "mupdf" (file))
    ("\\.mp3\\'" "mplayer" (file))
    ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "mplayer" ("-idx" file))
    ("\\.\\(?:jp?g\\|png\\)\\'" "sxiv" (file))))
  :config
  (openwith-mode t))

;;; TODO: This is asymetrically long; break it up or put it somewhere
;;; else, somehow? See e.g.
;;; <http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html>.
(use-package org
  :ensure org-plus-contrib
  :init
  ;; Modify the MathJax path to work with https hosts.
  (setq org-html-mathjax-options
        '((path "//cdn.mathjax.org/mathjax/latest/MathJax.js")))
  :config
  (add-hook 'org-mode-hook
    (lambda ()
      (auto-fill-mode)
      (typopunct-change-language 'english t)
      (typopunct-mode 1)))
  
  ;; Make the code-export work with light backgrounds; see e.g.
  ;; <https://raw.githubusercontent.com/kaushalmodi/.emacs.d/master/misc/css/leuven_theme.css>.
  (setq org-html-head "<style type=\"text/css\">
    /* Set the colors in <pre> blocks from the Leuven theme */
    pre                                      {background-color:#FFFFFF;}
    pre span.org-builtin                     {color:#006FE0;font-weight:bold;}
    pre span.org-string                      {color:#008000;}
    pre span.org-keyword                     {color:#0000FF;}
    pre span.org-variable-name               {color:#BA36A5;}
    pre span.org-function-name               {color:#006699;}
    pre span.org-type                        {color:#6434A3;}
    pre span.org-preprocessor                {color:#808080;font-weight:bold;}
    pre span.org-constant                    {color:#D0372D;}
    pre span.org-comment-delimiter           {color:#8D8D84;}
    pre span.org-comment                     {color:#8D8D84;font-style:italic}
    pre span.org-outshine-level-1            {color:#8D8D84;font-style:italic}
    pre span.org-outshine-level-2            {color:#8D8D84;font-style:italic}
    pre span.org-outshine-level-3            {color:#8D8D84;font-style:italic}
    pre span.org-outshine-level-4            {color:#8D8D84;font-style:italic}
    pre span.org-outshine-level-5            {color:#8D8D84;font-style:italic}
    pre span.org-outshine-level-6            {color:#8D8D84;font-style:italic}
    pre span.org-outshine-level-7            {color:#8D8D84;font-style:italic}
    pre span.org-outshine-level-8            {color:#8D8D84;font-style:italic}
    pre span.org-outshine-level-9            {color:#8D8D84;font-style:italic}
    pre span.org-rainbow-delimiters-depth-1  {color:#707183;}
    pre span.org-rainbow-delimiters-depth-2  {color:#7388d6;}
    pre span.org-rainbow-delimiters-depth-3  {color:#909183;}
    pre span.org-rainbow-delimiters-depth-4  {color:#709870;}
    pre span.org-rainbow-delimiters-depth-5  {color:#907373;}
    pre span.org-rainbow-delimiters-depth-6  {color:#6276ba;}
    pre span.org-rainbow-delimiters-depth-7  {color:#858580;}
    pre span.org-rainbow-delimiters-depth-8  {color:#80a880;}
    pre span.org-rainbow-delimiters-depth-9  {color:#887070;}
    pre span.org-sh-quoted-exec              {color:#FF1493;}
  </style>")
  
  ;; Actually use the style defined above in org-html-head.
  (setq org-html-htmlize-output-type 'css)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (ditaa . t)
     (dot . t)
     (gnuplot . t)
     (python . t)
     ;; This has been failing with "Invalid function:
     ;; org-babel-header-args-safe-fn", for some reason.
     ;; (R . t)
     ))

  (setq org-confirm-babel-evaluate nil)

  ;; Thanks, Bernt Hansen: <http://doc.norang.ca/org-mode.html>;
  ;; adding a few redundant aliases, too, because of case-sensitivity.
  (add-to-list 'org-src-lang-modes '("c" . c++))
  (add-to-list 'org-src-lang-modes '("c++" . c++))
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
  (add-to-list 'org-src-lang-modes '("ditaa" . artist))
  
  ;; See <http://orgmode.org/worg/org-issues.html> and
  ;; <http://article.gmane.org/gmane.emacs.orgmode/33955>.
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

  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)" "CANCELED(c)")))

  ;; Edit source code in the current window.
  (setq org-src-window-setup 'current-window)

  ;; Alternatively, `time'.
  (setq org-log-done 'note)

  ;; Open link in browser.
  (setq org-return-follows-link t)

  ;; Activate the shortcut keys specified in org-todo-keywords.
  (setq org-use-fast-todo-selection t)

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

  (bind-keys :map org-mode-map
             ("C-c C-x C-s" . org-mode-src-skel-with-tangle)
             ("C-c C-x C-q" . org-mode-quote-skel)
             ("C-c C-x C-e" . org-mode-example-skel))

  ;; For LaTeX output, use no indentation but paragraph-skips by
  ;; default.
  ;; (add-to-list 'org-export-latex-packages-alist '("" "parskip"))
  
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
                       (if org-par-open "\n<p>" ""))))))))

    (bind-key "C-x C-s" 'org-edit-src-save org-src-mode-map)

    (setq org-latex-pdf-process
          '("latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f %f")))

(use-package paredit
  :config
  ;; Rescue Taylor's more reasonable defaults.
  (bind-keys :map paredit-mode-map
             ("M-(" . paredit-wrap-round)
             ("M-)" . paredit-clone-round-and-newline)))

(use-package php-mode)

(use-package protobuf-mode
  :mode (("\\\\.proto\\\\'" . protobuf-mode)))

(use-package python
  :config
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
        (lambda () (interactive) (python-switch-to-python t))))))

(use-package scheme
  :mode (("\\.egg-locations\\'" . scheme-mode)
         ("\\.hy\\'" . scheme-mode)
         ("\\.meta\\'" . scheme-mode)
         ("\\.release-info\\'" . scheme-mode)
         ("\\.setup\\'" . scheme-mode)
         ("\\.shtml\\'" . scheme-mode)
         ("\\.stex\\'" . scheme-mode)
         ("\\.stumpwmrc\\'" . scheme-mode)
         ("\\.sxml\\'" . scheme-mode))
  :config
  (add-hook 'scheme-mode-hook 'enable-paredit-mode)
  
  (setq scheme-program-name "csi -n")
  
  ;; Evaluate whole Scheme buffer.
  (defun scheme-send-buffer ()
    "Just send the goddamn thing."
    (interactive)
    (scheme-send-region (point-min) (point-max)))

  (defun scheme-send-buffer-and-go ()
    "Send and go."
    (interactive)
    (scheme-send-buffer)
    (switch-to-buffer-other-window "*scheme*"))

  (bind-keys :map scheme-mode-map
             ("C-c b" . scheme-send-buffer)
             ("C-c B" . scheme-send-buffer-and-go))
  
  ;; Indent-functions for match, etc.
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

  ;; mini-kanren
  (put 'run 'scheme-indent-function 1)
  (put 'run* 'scheme-indent-function 1)
  (put 'fresh 'scheme-indent-function 1)
  (put 'project 'scheme-indent-function 1)

  ;; Unification (i.e. "identical to" as goal)
  (font-lock-add-keywords
   'scheme-mode
   '(("(\\(==\\)\\>"
      (0 (prog1 ()
           (compose-region (match-beginning 1)
                           (match-end 1)
                           ?≡)))))))

(use-package shell
  :init
  (bind-key "C-c C-l" 'helm-comint-input-ring shell-mode-map)
  ;; So that Emacs recognizes aliases when running commands.
  (setq shell-file-name "zsh")
  (setq shell-command-switch "-ic")

  :config
  ;; Turn color on
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  
  ;; Also turn color on for ad-hoc commands (see
  ;; <http://stackoverflow.com/questions/5819719/emacs-shell-command-output-not-showing-ansi-colors-but-the-code>).
  (defadvice display-message-or-buffer (before ansi-color activate)
    "Process ANSI color codes in shell output."
    (let ((buf (ad-get-arg 0)))
      (and (bufferp buf)
           (string= (buffer-name buf) "*Shell Command Output*")
           (with-current-buffer buf
             (ansi-color-apply-on-region (point-min) (point-max))))))

  (defun sh-execute-buffer ()
    "Just send the goddamn thing."
    (interactive)
    (sh-execute-region (point-min) (point-max)))

  (add-hook 'sh-mode-hook
    (lambda ()
      (bind-key "C-c b" 'sh-execute-buffer sh-mode-map))))

(use-package sh-script
  :mode ("\\.bats\\'" . sh-mode))

(use-package slime
  :config
  (add-hook
   'slime-mode-hook
   (lambda ()
     (setq slime-protocol-version 'ignore)
     (add-to-list 'slime-lisp-implementations
                  '(sbcl ("/usr/local/bin/sbcl --noinform"))))))

;;; http://www.emacswiki.org/emacs/TabCompletion#toc2
(use-package smart-tab
  :config (smart-tab-mode-on))

(use-package sort
  :bind ("C-c s" . sort-lines))

(use-package sql
  :config
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

  (add-hook 'sql-interactive-mode-hook 'my-sql-save-history-hook)
  (add-hook 'sql-mode-hook
    (lambda ()
      (define-key sql-mode-map (kbd "TAB") 'sql-indent-line)))

  ;; The executable is osql, but osql doesn't seem to pass things to
  ;; isql correctly.
  (setq sql-ms-options '("--" "-w" "300" "-n")))

(use-package sql-indent)
  
(use-package surfraw
  :load-path "lisp/"
  :ensure nil
  :config
  (surfraw-init)
  (bind-keys* :map global-map
              :prefix-map surfraw-prefix-map
              :prefix "C-c C-s"
              ("c" . surfraw-code-search)
              ("C" . surfraw-code-search-region)
              ("d" . surfraw-drive)
              ("D" . surfraw-drive-region)
              ("e" . surfraw-oed-region)
              ("E" . surfraw-oed-region)
              ("g" . surfraw-google)
              ("G" . surfraw-google-region)
              ("h" . surfraw-chicken-regex)
              ("H" . surfraw-chicken-regex-region)
              ("m" . surfraw-gmail)
              ("M" . surfraw-gmail-region)
              ("r" . surfraw-groups)
              ("R" . surfraw-groups-region)
              ("w" . surfraw-W)
              ("W" . surfraw-W-region)
              
              ("a" .
               (lambda () (interactive) (surfraw-bookmark "calendar")))
              ("q" .
               (lambda () (interactive) (surfraw-bookmark "critique")))))

(use-package tex-mode
  :config
  (setq tex-dvi-view-command "mupdf"))

(use-package thingatpt+)

;; Let's do auto-quotes, dashes, &c.; see:
;; <http://www.emacswiki.org/emacs/TypographicalPunctuationMarks>.
(use-package typopunct
  :config
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
       (t ad-do-it)))))

(use-package unbound)

(use-package undo-tree)

(use-package use-package)

(use-package windmove
  :bind (("<up>" . windmove-up)
         ("<down>" . windmove-down)
         ("<right>" . windmove-right)
         ("<left>" . windmove-left))
  :config
  (windmove-default-keybindings)
  (setq windmove-wrap-around t))

(use-package winner
  :config
  (winner-mode 1))

(use-package xclip
  :if (executable-find "xclip")
  :config
  (turn-on-xclip))

(use-package yaml-mode)

;;;; Miscellanea
;;;;
;;;; TODO: Find some mechanism to store these in an auxiliary package;
;;;; see
;;;; e.g. <http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html#“local”-packages>.

;;; TODO: Do we need to set this in specific modes (e.g. cc-mode), or
;;; does it suffice to set it globally here?
(setq-default fill-column 80)

(defalias 'yes-or-no-p 'y-or-n-p)

(put 'add-hook 'lisp-indent-function 1)
(put 'use-package 'lisp-indent-function 1)

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)

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

(bind-key "C-o" 'isearch-occur isearch-mode-map)
(bind-key "m" 'occur-multi-occur occur-mode-map)

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

;;; Killing a line backwards; see
;;; <http://www.emacswiki.org/emacs/BackwardKillLine>.
(defun kill-line-backward (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

;;; Subword mode
(global-subword-mode 1)

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

;;; For makefiles, dot, SQL and such things.
(setq tab-width 4)
(setq-default tab-width 4)

(setq version-control t)
(setq kept-new-versions 100)
(setq kept-old-versions 100)

(setq compilation-scroll-output 'first-error)

(setq user-mail-address "pcd@roxygen.org")

(bind-key "C-m" 'act query-replace-map)

;;; Prevent "Active processes exist" on exit; thanks, Jürgen Hötzel:
;;; <http://stackoverflow.com/a/2708042>.
(add-hook 'comint-exec-hook
  (lambda () (process-kill-without-query (get-buffer-process (current-buffer)) nil)))

(bind-keys
 :map comint-mode-map
 ("C-c C-z" . nil)
 ("C-c C-z ." . 'browse-url-at-point)
 ("C-c C-z b" . 'browse-url-of-buffer)
 ("C-c C-z r" . 'browse-url-of-region)
 ("C-c C-z u" . 'browse-url)
 ("C-c C-z v" . 'browse-url-of-file))

;;; Thanks, unbound; on the other hand, see
;;; <http://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html>:
;;; "Don't define C-c letter as a key in Lisp programs. Sequences
;;; consisting of C-c and a letter (either upper or lower case) are
;;; reserved for users; they are the only sequences reserved for
;;; users, so do not block them."
(bind-keys
 :map global-map
 ("C-a" . smarter-move-beginning-of-line)
 ("C-c ;" . comment-or-uncomment-region)
 ("C-c C-h" . help-command)
 ("C-c C-k" . copy-line)
 ;; <https://lists.gnu.org/archive/html/help-gnu-emacs/2006-08/msg00528.html>
 ("C-c C-z ." . browse-url-at-point)
 ("C-c C-z b" . browse-url-of-buffer)
 ("C-c C-z r" . browse-url-of-region)
 ("C-c C-z u" . browse-url)
 ("C-c C-z v" . browse-url-of-file)
 ("C-c O" . multi-occur-in-this-mode)
 ("C-c P" . copy-file-name-to-clipboard)
 ("C-c R" . recompile)
 ("C-c U" . rename-uniquely)
 ("C-c a" . list-matching-lines)
 ("C-c c" . compile)
 ("C-c h" . help-command)
 ("C-c l" . org-store-link)
 ("C-c o" . occur)
 ("C-c p" . pwd)
 ("C-c u" . kill-line-backward)
 ("C-h" . kill-whole-line)
 ("C-o" . smart-open-line-above)
 ("C-x C-r" . revert-buffer) 
 ("C-x TAB" . indent-rigidly)
 ("M-%" . query-replace-regexp)
 ("M-;" . comment-dwim-line)
 ("M-o" . smart-open-line)
 )

(bind-key "M-p" 'previous-history-element minibuffer-local-map)

;;; Compensate for screen.
(bind-keys
 :map input-decode-map
 ("\e[1;2A" . [S-up])
 ("\e[1;2B" . [S-down])
 ("\e[1;2D" . [S-left])
 ("\e[1;2C" . [S-right])

 ("\e[1;3A" . [M-up])
 ("\e[1;3B" . [M-down])
 ("\e[1;3D" . [M-left])
 ("\e[1;3C" . [M-right])

 ("\e[1;4A" . [S-M-up])
 ("\e[1;4B" . [S-M-down])
 ("\e[1;4D" . [S-M-left])
 ("\e[1;4C" . [S-M-right])

 ("\e[1;5A" . [C-up])
 ("\e[1;5B" . [C-down])
 ("\e[1;5D" . [C-left])
 ("\e[1;5C" . [C-right])

 ("\e[1;6A" . [C-S-up])
 ("\e[1;6B" . [C-S-down])
 ("\e[1;6D" . [C-S-left])
 ("\e[1;6C" . [C-S-right])

 ("\e[1;7A" . [C-M-up])
 ("\e[1;7B" . [C-M-down])
 ("\e[1;7D" . [C-M-left])
 ("\e[1;7C" . [C-M-right]))

(add-to-list 'auto-mode-alist '("\\.txt\\'" . auto-fill-mode))
;;; Regex from <http://www.emacswiki.org/emacs/MuttInEmacs>.
(add-to-list 'auto-mode-alist '("/mutt" .
                                (lambda ()
                                  (footnote-mode)
                                  (auto-fill-mode 0)
                                  (turn-on-orgtbl)
                                  (typopunct-change-language 'english t)
                                  (typopunct-mode 1))))

(setq vc-follow-symlinks t)

;;; Stuff to publish the daybook.
(if (file-exists-p "~/prg/org/daybook/daybook.el")
    (load "~/prg/org/daybook/daybook.el"))

;;; From <http://steve.yegge.googlepages.com/my-dot-emacs-file>.
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "fNew name: ")
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

;;; Copy the current line, don't kill it; see
;;; <http://www.emacswiki.org/emacs/CopyingWholeLines> for other
;;; solutions, also <http://stackoverflow.com/questions/88399>.
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

;;; Thanks, Bozhidar; see <http://stackoverflow.com/a/9414763>.
(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

 ;;; Load a host-specific file, if one exists.
(let ((host-file (format "~/.emacs.d/%s.el" system-name)))
  (if (file-exists-p host-file)
      (load host-file)))

;;; Thanks, Jason Viers;
;;; <http://programmers.stackexchange.com/a/19683>; should we modify
;;; it to also go to the next line with (next-line) à la
;;; <http://stackoverflow.com/a/9697222>?
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command. If no region is
selected and current line is not blank and we are not at the end
of the line, then comment current line.  Replaces default
behaviour of comment-dwim, when it inserts comment at the end of
the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))


;; Thanks, Ryan McGeary! See <http://goo.gl/nZQE70>; see also
;; <http://goo.gl/F07DiO>.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
