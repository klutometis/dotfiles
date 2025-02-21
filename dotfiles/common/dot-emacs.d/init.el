(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

;; Load a separate file containing all global settings and functions;
;; front-loading to enjoy the function definitions when configuring packages
;; below.
(load "~/.emacs.d/init-settings.el")

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

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package avy
  :init
  (unbind-key "C-c a")
  (load-file "~/.emacs.d/avy-actions.el")

  :bind (("C-c a c" . avy-goto-char)
         ("C-c a C" . avy-goto-char-2)
         ("C-c a t" . avy-goto-char-timer)
         ("C-c a l" . avy-goto-line)
         ("C-c a w" . avy-goto-word-1)
         ("C-c a e" . avy-goto-word-0)
         ("C-c a r" . avy-resume))

  :custom
  (avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
  (avy-dispatch-alist
   '((?x . avy-action-kill-move)
     (?X . avy-action-kill-stay)
     (?m . avy-action-mark)
     (?f . avy-action-copy)     ;; replaced ?n
     (?v . avy-action-yank)     ;; replaced ?i
     (?Y . avy-action-yank-line)
     (?z . avy-action-zap-to-char)
     (?k . avy-action-kill-stay)
     (?K . avy-action-kill-whole-line)
     (?w . avy-action-copy)
     (?W . avy-action-copy-whole-line)
     (?y . avy-action-yank)
     (?Y . avy-action-yank-whole-line)
     (?p . avy-action-teleport)  ;; replaced ?t
     (?P . avy-action-teleport-whole-line) ;; replaced ?T
     (?M . avy-action-mark-to-char)
     (?\; . avy-action-flyspell)))

  :custom-face
  (avy-lead-face ((t (:background "yellow" :foreground "black" :weight bold))))
  (avy-lead-face-0 ((t (:background "cyan" :foreground "black" :weight bold))))
  (avy-lead-face-1 ((t (:background "green" :foreground "black" :weight bold))))
  (avy-lead-face-2 ((t (:background "magenta" :foreground "black" :weight bold)))))

(use-package bazel
  :hook ((bazel-mode-hook
          . (lambda () (add-hook 'before-save-hook 'bazel-buildifier)))))

(use-package better-defaults
  :config
  ;;; Don't flash on bell, after all.
  (setq visible-bell nil))

(use-package combobulate
  :bind (("C-<M-a>" . combobulate-navigate-beginning-of-defun))
  :custom
  (combobulate-key-prefix "C-c o")
  :hook ((prog-mode . combobulate-mode)))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)) ;; Default is 0.2

(use-package compile
  :config
  (setq compilation-ask-about-save nil)
  (setq compilation-always-kill t))

(use-package desktop
  :config
  (desktop-save-mode 1)
  (add-to-list 'desktop-globals-to-save 'kill-ring))

(use-package dictionary)

(use-package dired-subtree
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)))

(use-package edit-indirect)

(use-package emacs
  :init
  ;; Configure input-decode-map for terminal Meta + Arrow keys
  (when (not (display-graphic-p))
    (define-key input-decode-map "\e[1;3A" [M-up])
    (define-key input-decode-map "\e[1;3B" [M-down])
    (define-key input-decode-map "\e[1;3C" [M-right])
    (define-key input-decode-map "\e[1;3D" [M-left])
    (define-key input-decode-map "\e[1;9A" [C-M-a])
    (define-key input-decode-map "\e[1;9Z" [C-M-z]))

  :config
  ;; Show the column-number in addition to the row-number in the
  ;; status-bar.
  (column-number-mode 1)
  :bind (("M-z" . zap-to-char)
         ("C-<M-z>" . zap-up-to-char))
  :hook
  ((find-file-hook . (lambda () (setq buffer-save-without-query t)))))

(use-package embark)

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
  :hook ((dired-mode-hook
          . (lambda () (bind-key "F" 'dired-do-find-marked-files dired-mode-map)))))

;; (use-package flycheck
;;   :init (global-flycheck-mode))

(use-package format-all
  :config
  (add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
  (let ((prettier-flags '("--print-width=80" "--tab-width=2" "--use-tabs=false" "--semi=true" "--single-quote=true" "--quote-props=preserve"  "--bracket-spacing=false" "--trailing-comma=all" "--arrow-parens=always" "--embedded-language-formatting=off" "--bracket-same-line=true" "--single-attribute-per-line=false" "--jsx-single-quote=false" "--plugins=google3Plugin" "--html-whitespace-sensitivity=strict")))
    (setq-default format-all-formatters
                  `(
                    ("Bazel" (buildifier))
                    ("C++" (clang-format))
                    ("Emacs Lisp" (emacs-lisp))
                    ("Graphviz" (nop))
                    ("HTML" (prettier))
                    ("JSON" (deno))
                    ("JavaScript" (deno))
                    ("Markdown" (prettier "--print-width=80" "--prose-wrap=always"))
                    ("Python" (black))
                    ("SCSS" (prettier . ,prettier-flags))
                    ("Shell" (shfmt "-i" "2" "-ci" "-bn" "-sr"))
                    ("Shell" (shfmt "-i" "2" "-ci" "-bn"))
                    ("Slidev" (prettier))
                    ("TypeScript" (prettier . ,prettier-flags))
                    ("YAML" (prettier))
                    )))
  (defun my-format-all-buffer-around-advice (orig-fun &rest args)
    "Around advice for `format-all-buffer' to restore the cursor position."
    (let ((current-pos (point-marker)))
      (apply orig-fun args)
      (goto-char current-pos)))

  (advice-add 'format-all-buffer :around #'my-format-all-buffer-around-advice)

  (add-hook 'before-save-hook 'format-all-buffer)

  (define-format-all-formatter nop
    (:executable "nop")
    (:install)
    (:languages "Graphviz")
    (:features)
    (:format (format-all--buffer-easy executable "-"))))

(use-package full-ack)

(use-package gptel
  :ensure t
  :bind (("C-c C-l s" . gptel-send)
         ("C-c C-l c" . gptel-clear)
         ("C-c C-l i" . gptel-insert)
         ("C-c C-l k" . gptel-set-api-key)
         ("C-c C-l m" . gptel-switch-model)
         ("C-c C-l n" . gptel-next-response)
         ("C-c C-l p" . gptel-previous-response)))

(use-package graphviz-dot-mode
  :init
  (setq graphviz-dot-indent-width 2))

(use-package grep
  :bind ("C-c r" . rgrep)
  :config
  (setq grep-save-buffers 'save-all-file-buffers))

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
         ("C-x b" . switch-to-buffer)
         ;;
         ;; You know what? I can't fucking stand helm-find-files,
         ;; either; let's go back to ido!
         ;;
         ;; ("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ([tab] . helm-execute-persistent-action))
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
  ;; Use ack.
  (when (executable-find "ack-grep")
    (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
          helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))
  ;; Man-page at point
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages))

(use-package helpful)

(use-package keyfreq
  :config
  (keyfreq-mode 1)           ;; Enable keyfreq-mode
  (keyfreq-autosave-mode 1)) ;; Automatically save frequency data


(use-package language-id
  :config
  (push '("Graphviz" (graphviz-dot-mode (language-id--file-name-extension ".dot"))) language-id--definitions)
  (push '("Slidev" (markdown-mode (language-id--file-name-regexp "slides\\.md"))) language-id--definitions))

;; (use-package lsp-mode
;;   :init
;;   ;; (setq lsp-prefer-flymake nil) ;; Use lsp-ui and flycheck instead of flymake
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   (setq lsp-clients-clangd-args
;;         '("--background-index"
;;           "--suggest-missing-includes"
;;           "--clang-tidy"
;;           "--header-insertion=iwyu"))
;;   :config
;;   (setq lsp-headerline-breadcrumb-enable nil)
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          (c++-mode . lsp)
;;          ;; if you want which-key integration
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)

;; (use-package lsp-ui
;;   :commands lsp-ui-mode
;;   :config
;;   (setq lsp-ui-sideline-enable nil
;;         lsp-ui-doc-enable nil))

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
  (setq magit-auto-revert-mode nil)

  ;; Make this slightly more permissive such that the terminal question-mark or
  ;; colon are optional.
  (setq magit-process-yes-or-no-prompt-regexp
        " [\[(]\\([Yy]\\(?:es\\)?\\)[/|]\\([Nn]o?\\)[\])] ?[?:]? ?$"))

(use-package markdown-mode
  :hook ((markdown-mode-hook
          . (lambda ()
              (auto-fill-mode 1)
              (unbind-key "C-c C-s" markdown-mode-map)))))

(use-package midnight
  :config
  (setq clean-buffer-list-delay-general 7)
  (midnight-delay-set 'midnight-delay 0)
  (setq
   clean-buffer-list-delay-general 1
   clean-buffer-list-kill-regexps '("^[*].*"))
  (push "*compilation*" clean-buffer-list-kill-never-buffer-names)
  (push "notes" clean-buffer-list-kill-never-buffer-names)
  (push "TODO" clean-buffer-list-kill-never-buffer-names)
  (add-hook 'midnight-hook
            (lambda () (interactive)
              (desktop-save desktop-dirname))))

(use-package paredit
  :bind (:map paredit-mode-map
              ("M-(" . paredit-wrap-round)
              ("M-)" . paredit-clone-round-and-newline))
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)
         (scheme-mode . paredit-mode)))

(use-package python
  :config
  ;; Let's go 4 to be congruent with `black`.
  (setq python-indent-offset 4))

(use-package savehist
  :config
  (setq savehist-save-minibuffer-history 1)
  (setq savehist-additional-variables
        '(kill-ring search-ring regexp-search-ring compile-history log-edit-comment-ring
                    helm-M-x-input-history helm-find-files-history helm-grep-history))
  (setq savehist-file "~/.emacs.d/savehist")
  ;; Hack to attempt to deal with 100% CPU every couple minutes.
  ;; (setq savehist-autosave-interval 600)
  (savehist-mode 1))

(use-package sort
  :bind ("C-c s" . sort-lines))

(use-package typescript-mode
  :config
  (setq typescript-indent-level 2))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  (treesit-font-lock-level 6)
  :config
  (global-treesit-auto-mode)
  (treesit-auto-add-to-auto-mode-alist 'all))

(use-package transient)

(use-package which-key
  :config
  (which-key-mode))

(use-package windmove
  :bind (("<up>" . windmove-up)
         ("<down>" . windmove-down)
         ("<right>" . windmove-right)
         ("<left>" . windmove-left))
  :config
  (windmove-default-keybindings)
  (setq windmove-wrap-around t))

;;; For manipulating stacks of windows.
(use-package winner
  :config
  (winner-mode 1))

(use-package xclip
  :config
  (setq xclip-method 'xsel)
  (setq xclip-program "xsel")

  :init
  (defun xclip-copy-to-clipboard ()
    "Copy selection to clipboard using xclip."
    (interactive)
    (when (use-region-p)  ; Ensure there is a text selection
      (xclip-set-selection 'clipboard (buffer-substring-no-properties (region-beginning) (region-end)))
      (deactivate-mark)))  ; Optionally clear the selection

  (defun xclip-paste-from-clipboard ()
    "Paste text from clipboard using xclip."
    (interactive)
    (insert (xclip-get-selection 'clipboard)))

  :bind
  (("M-C-w" . xclip-copy-to-clipboard)
   ("M-C-y" . xclip-paste-from-clipboard)))

(use-package yasnippet
  :config
  (yas-global-mode 01)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(use-package yasnippet-snippets
  :after yasnippet)
