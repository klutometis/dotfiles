;; Bootstrap straight.el  -*- lexical-binding: t; -*-
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Integrate use-package with straight.el
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq package-install-upgrade-built-in t)

;; Just discard changes in dirty repos (gptel, for instance, is
;; perpetually dirty).
(defun my-straight-auto-discard-advice (local-repo)
  "Automatically discard changes in straight repos without prompting."
  (let ((status (let ((straight--process-trim nil))
                  (straight--process-output
                   "git" "-c" "status.branch=false"
                   "status" "--short"))))
    (if (string-empty-p status)
        t  ; Clean worktree, proceed normally
      ;; Dirty worktree - automatically discard changes
      (straight--output "Auto-discarding changes in repository %S" local-repo)
      (and (straight--process-output "git" "reset" "--hard")
           (straight--process-output "git" "clean" "-ffd"))
      t)))  ; Return t after discarding changes

(advice-add 'straight-vc-git--ensure-worktree :override #'my-straight-auto-discard-advice)

;; Optional: auto-update all packages on Emacs startup
(add-hook 'emacs-startup-hook #'straight-pull-all)

;; Load a separate file containing all global settings and functions;
;; front-loading to enjoy the function definitions when configuring packages
;; below.
(load "~/.emacs.d/init-settings.el")

;;; Load transient early and eagerly.
(use-package transient :demand t)

(use-package ace-window
  :bind ("C-x o" . ace-window)
  :init
  (setq aw-dispatch-alist
        '((?d aw-delete-window " Ace - Delete Window")
          (?s aw-swap-window " Ace - Swap Window")
          (?f aw-flip-window " Ace - Flip Window")
          (?v aw-split-window-vert " Ace - Split Vert Window")
          (?h aw-split-window-horz " Ace - Split Horz Window")
          (?m delete-other-windows " Ace - Maximize Window")))
  (setq aw-keys '(?a ?o ?e ?i ?h ?t ?n ?s))

  :custom-face
  (aw-leading-char-face ((t (:foreground "yellow" :background "black" :weight bold))))
  (aw-minibuffer-leading-char-face ((t (:foreground "yellow" :background "black" :weight bold)))))

(use-package agent-shell
  :custom
  (agent-shell-anthropic-claude-command '("npx" "-y" "@zed-industries/claude-code-acp@latest" "--permission-mode" "plan"))
  (agent-shell-google-gemini-command '("npx" "-y" "@google/gemini-cli@latest" "--experimental-acp"))
  (agent-shell-openai-codex-command '("npx" "-y" "@zed-industries/codex-acp@latest")))

(use-package aidermacs
  :bind (("C-c A" . aidermacs-transient-menu)
         ("C-c M" . aidermacs-select-profile))
  :custom
  (aidermacs-program "aider-claude")
  ;; Disruptive, for some reason to force-show diff; maybe can call manually.
  (aidermacs-show-diff-after-change nil)
  :config
  ;; Custom enthusiastic accept-change function
  (defun aidermacs-accept-change ()
    "Send an enthusiastic acceptance to aidermacs."
    (interactive)
    (aidermacs--send-command "/code Let's go!"))

  ;; Utility functions for reasoning settings
  (defun aidermacs-toggle-thinking-tokens ()
    "Toggle thinking tokens between different levels or disable."
    (interactive)
    (let ((current-setting (completing-read "Thinking tokens: " '("32k" "8k" "4k" "1k" "0") nil t "32k")))
      (aidermacs--send-command (format "/think-tokens %s" current-setting))
      (message "Set thinking tokens to %s" current-setting)))

  (defun aidermacs-set-reasoning-effort ()
    "Set reasoning effort level."
    (interactive)
    (let ((effort (completing-read "Reasoning effort: " '("low" "medium" "high") nil t)))
      (aidermacs--send-command (format "/reasoning-effort %s" effort))
      (message "Set reasoning effort to %s" effort)))

  (defun aidermacs-select-profile ()
    "Select an aider configuration profile (Claude, Gemini, or OpenAI)."
    (interactive)
    (let* ((profiles '(("🔵 Claude (Sonnet 4.5 + Haiku 4.5)" . "aider-claude")
                       ("🟢 Gemini (Flash + Flash Lite)" . "aider-gemini")
                       ("🟠 OpenAI (GPT-5 Mini + Nano)" . "aider-gpt")))
           (choice (completing-read "Select aider profile: " profiles nil t)))
      (let ((program (cdr (assoc choice profiles))))
        (setq aidermacs-program program)
        ;; Clear the cached program path so aidermacs-get-program re-resolves it.
        (setq aidermacs--resolved-program nil)
        (message "Switched to %s" choice)))))

(use-package all-the-icons)

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package auth-source-pass
  :config
  (auth-source-pass-enable))

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
          . (lambda () (add-hook 'before-save-hook 'bazel-buildifier))))
  :config
  (add-to-list 'auto-mode-alist '("/BUILD\\(?:\\.bazel\\)?\\'" . bazel-mode))
  (add-to-list 'auto-mode-alist '("/WORKSPACE\\(?:\\.bazel\\)?\\'" . bazel-mode))
  (add-to-list 'auto-mode-alist '("\\.bzl\\'" . bazel-mode)))

(use-package better-defaults
  :config
  ;; Don't flash on bell, after all.
  (setq visible-bell nil))

(use-package clipetty
  :config
  (global-clipetty-mode 1))

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

(use-package deadgrep)

(use-package deferred)

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
  (unless (display-graphic-p)
    (define-key input-decode-map "\e[1;3A" [M-up])
    (define-key input-decode-map "\e[1;3B" [M-down])
    (define-key input-decode-map "\e[1;3C" [M-right])
    (define-key input-decode-map "\e[1;3D" [M-left])
    (define-key input-decode-map "\e[1;9A" [C-M-a])
    (define-key input-decode-map "\e[1;9Z" [C-M-z]))

  (defun chmod-current-file (mode)
    "Chmod the current file to the specified MODE."
    (interactive "sMode: ")
    (let ((filename (buffer-file-name)))
      (when filename
        (call-process "chmod" nil 0 nil mode filename)
        (message "Chmodded %s to %s" filename mode))))

  :custom
  ;; Enable terminal mouse tracking
  (mouse-wheel-follow-mouse t)
  (mouse-wheel-scroll-amount '(1 ((shift) . 5))) ;; optional

  :config
  ;; Enable mouse support in terminal
  (xterm-mouse-mode 1)

  ;; Show the column-number in addition to row-number
  (column-number-mode 1)

  (defun toggle-terminal-mouse ()
    (interactive)
    (if (bound-and-true-p xterm-mouse-mode)
        (progn
          (xterm-mouse-mode -1)
          (message "Mouse disabled. Alacritty copy/paste restored."))
      (xterm-mouse-mode 1)
      (message "Mouse enabled in Emacs.")))

  :bind (("C-c ?"     . help-for-help)
         ("M-z"       . zap-to-char)
         ("C-<M-z>"   . zap-up-to-char)
         ("C-c m"     . chmod-current-file)
         ("C-c M-m" . toggle-terminal-mouse)
         ;; Get rid of suspension (messes with tmux).
         ("C-z" . nil)
         ("C-x C-z" . nil))

  :hook
  ((find-file-hook . (lambda () (setq buffer-save-without-query t)))))

(use-package embark)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package find-dired
  :bind (("C-c n" . find-name-dired))
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

(use-package flycheck)

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
                    ("Python" (pyformat "-s" "2"))
                    ("SCSS" (prettier . ,prettier-flags))
                    ("Shell" (shfmt "-i" "2" "-ci" "-bn" "-sr"))
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
    (:format (format-all--buffer-easy executable "-")))

  (define-format-all-formatter pyformat
    (:executable "pyformat")
    (:install)
    (:languages "Python")
    (:features)
    (:format (format-all--buffer-easy executable))))

;;; Activate dark-mode in terminal.
(use-package frame
  :straight nil
  :custom
  (frame-background-mode 'dark)
  :hook
  (after-init . (lambda () (mapc 'frame-set-background-mode (frame-list)))))

(use-package full-ack)

(use-package gptel
  :bind (("C-c t s" . gptel-send)
         ("C-c t r" . gptel-rewrite)
         ("C-c t a" . gptel-add)
         ("C-c t f" . gptel-add-file)
         ("C-c t x" . gptel-context-remove-all)
         ("C-c t m" . gptel-menu)
         ("C-c t g" . gptel))
  :custom
  (gptel-default-mode 'text-mode)
  (gptel-model 'gpt-4o)
  (gptel-track-media t)
  (gptel-include-reasoning t)
  (gptel-confirm-tool-calls nil)
  (gptel-include-tool-results t)
  :hook ((gptel-post-stream-hook . gptel-auto-scroll)
         (gptel-post-response-functions . gptel-end-of-response))
  :custom-face
  (gptel-context-highlight-face ((t (:weight bold :background "#2a2f4a" :extend t))))
  (gptel-rewrite-highlight-face ((t (:weight bold :background "#1f3a24" :extend t))))

  :config
  (require 'gptel-integrations)

  (defun save-gptel-buffer-with-timestamp ()
    "Save the current gptel buffer to a directory with a timestamped filename."
    (interactive)
    (let ((directory "~/prg/gptel/")  ; Specify your desired directory here
          (filename (format-time-string "gptel-%Y%m%d-%H%M%S.txt")))
      (write-region (point-min) (point-max) (concat directory filename))
      (message "Buffer saved as %s" (concat directory filename))))

  (defun check-and-save-gptel-buffer ()
    "Check if the gptel buffer token count exceeds a limit and save it."
    (when (and (derived-mode-p 'gptel-mode)
               (> (buffer-size) 30000))  ; Adjust the token count threshold as needed
      (save-gptel-buffer-with-timestamp)
      (message "Buffer saved due to exceeding token limit.")))

  (defun gptel-cleanup-code-fences (beg end)
    "Remove Markdown-style code fences (like ```python) from GPTel rewrite response.
This operates in-place on the rewritten region between BEG and END."
    (save-excursion
      ;; Remove trailing fence if present
      (goto-char end)
      (forward-line 0) ; Move to beginning of the last line
      (when (looking-at "^```\\s-*$")
        (delete-region (line-beginning-position) (1+ (line-end-position)))) ; Remove line and newline

      ;; Remove leading fence if present
      (goto-char beg)
      (when (looking-at "^```.*$")
        (delete-region (line-beginning-position) (1+ (line-end-position))))))

  (defun gptel-replace-buffer-with-overlay ()
    "Replace current buffer contents with text from GPTel overlay."
    (interactive)
    (let* ((ovs (overlays-in (point-min) (point-max)))
           (target (car ovs))
           (content
            (cond
             ((overlay-get target 'display)
              (format "%s" (overlay-get target 'display)))
             ((and (overlay-start target) (overlay-end target))
              (buffer-substring-no-properties
               (overlay-start target)
               (overlay-end target)))
             (t (user-error "No usable overlay content found")))))
      (erase-buffer)
      (insert content)
      (message "Replaced buffer contents with overlay text.")))

  (setq gptel-backends
        (list
         (gptel-make-anthropic "Claude"
           :stream t
           :key (getenv "ANTHROPIC_API_KEY")
           :request-params '(:thinking (:type "enabled" :budget_tokens 24000)))
         (gptel-make-openai "OpenAI"
           :stream t
           :key (getenv "OPENAI_API_KEY")
           :request-params '(:thinking (:type "enabled" :budget_tokens 24000)))
         (gptel-make-gemini "Gemini"
           :stream t
           :key (getenv "GOOGLE_API_KEY")
           :request-params '(:thinking (:type "enabled" :budget_tokens 24000))))))

(use-package graphviz-dot-mode
  :init
  (setq graphviz-dot-indent-width 2))

(use-package grep
  :config
  (setq grep-save-buffers 'save-all-file-buffers))

;; Vertico stack for completion - modern, modular alternative to Helm
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)  ; Cycle through candidates
  (vertico-count 20)) ; Show more candidates

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

(use-package consult
  :bind (;; Replace helm bindings
         ("<f1> a" . consult-apropos)        ; was helm-apropos
         ("C-c h o" . consult-line)          ; was helm-occur
         ("C-x b" . consult-buffer)          ; was helm-buffers-list
         ("M-y" . consult-yank-pop)          ; was helm-show-kill-ring
         ;; Replace grep/find bindings
         ("C-c r" . consult-ripgrep)         ; was rgrep
         ("C-c f" . consult-find)            ; was find-grep-dired
         ;; Additional useful commands
         ("M-g i" . consult-imenu)
         ("M-g M-g" . consult-goto-line)
         ("C-x C-r" . consult-recent-file))
  :custom
  (consult-narrow-key "<")  ; Use < to narrow
  :config
  ;; Use ripgrep if available, otherwise fallback to grep
  (when (executable-find "rg")
    (setq consult-ripgrep-args
          "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip")))

;; Embark for actions on candidates
(use-package embark
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim))
  :custom
  (embark-quit-after-action nil))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package helpful)

(use-package keyfreq
  :config
  (keyfreq-mode 1)           ;; Enable keyfreq-mode
  (keyfreq-autosave-mode 1)) ;; Automatically save frequency data

(use-package language-id
  :config
  (push '("Bazel" (bazel-mode (language-id--file-name-regexp "BUILD"))) language-id--definitions)
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

(use-package mcp
  :config
  (defun load-mcp-servers-from-json (json-file)
    "Load MCP server configuration from JSON file and convert to mcp-hub-servers format."
    (let* ((json-object-type 'alist)
           (json-array-type 'list)
           (json-key-type 'string)
           (json-data (json-read-file json-file))
           (servers (cdr (assoc "mcpServers" json-data))))
      (mapcar (lambda (server)
                (let* ((name (car server))
                       (config (cdr server))
                       (command (cdr (assoc "command" config)))
                       (args (cdr (assoc "args" config)))
                       (env (cdr (assoc "env" config))))
                  `(,name . (:command ,command
                                      :args ,args
                                      ,@(when env
                                          `(:env (,@(mapcan (lambda (pair)
                                                              (list (intern (concat ":" (car pair)))
                                                                    (cdr pair)))
                                                            env))))))))
              servers)))

  (setq mcp-hub-servers (load-mcp-servers-from-json "~/etc/mcp.json")))

(use-package midnight
  :init
  (midnight-mode 1)
  :custom
  (midnight-delay 0)
  (clean-buffer-list-delay-general 1)
  (clean-buffer-list-kill-regexps '(".*"))
  :hook
  (midnight . (lambda () (desktop-save desktop-dirname))))

(use-package paredit
  :bind (:map paredit-mode-map
              ("M-(" . paredit-wrap-round)
              ("M-)" . paredit-clone-round-and-newline))
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)
         (scheme-mode . paredit-mode)))

(use-package monet
  :straight (:type git :host github :repo "stevemolitor/monet"))

;; for eat terminal backend:
(use-package eat
  :straight (:type git
                   :host codeberg
                   :repo "akib/emacs-eat"
                   :files ("*.el" ("term" "term/*.el") "*.texi"
                           "*.ti" ("terminfo/e" "terminfo/e/*")
                           ("terminfo/65" "terminfo/65/*")
                           ("integration" "integration/*")
                           (:exclude ".dir-locals.el" "*-tests.el"))))

;; for vterm terminal backend:
(use-package vterm :straight t)

;; install claude-code.el, using :depth 1 to reduce download size:
(use-package claude-code
  :straight (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main" :depth 1
                   :files ("*.el" (:exclude "images/*")))
  :bind-keymap
  ("C-c C" . claude-code-command-map) ;; C-c followed by uppercase C
  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode))
  :config
  ;; optional IDE integration with Monet
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)

  (claude-code-mode))

(use-package poetry)

(use-package projectile
  :config
  (projectile-mode +1))

;;; Make pulse more readable in terminal.
(use-package pulse
  :custom-face
  (pulse-highlight-start-face
   ((((class color) (min-colors 88) (background dark))
     :background "navy" :foreground "white" :extend t)
    (((class color) (min-colors 88) (background light))
     :background "light goldenrod yellow" :extend t)
    (t :inherit secondary-selection)))
  (pulse-highlight-face
   ((((class color) (min-colors 88) (background dark))
     :background "navy" :foreground "white" :extend t)
    (((class color) (min-colors 88) (background light))
     :background "light goldenrod yellow" :extend t)
    (t :inherit secondary-selection))))

(use-package python-mode
  :bind (:map python-ts-mode-map ("C-c C-l" . nil))
  :config
  ;; Let's go 4 to be congruent with `black`.
  (setq python-indent-offset 4))

(use-package savehist
  :custom
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables
   '(minibuffer-history
     compile-history
     extended-command-history
     kill-ring
     search-ring
     regexp-search-ring
     log-edit-comment-ring
     file-name-history
     consult--grep-history))
  (savehist-file "~/.emacs.d/savehist")
  ;; (savehist-autosave-interval 600) ; optional CPU fix
  :config
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

(use-package transpose-frame
  :bind (("C-c w t" . transpose-frame)
         ("C-c w f" . flip-frame)
         ("C-c w F" . flop-frame)
         ("C-c w r" . rotate-frame)
         ("C-c w c" . rotate-frame-clockwise)
         ("C-c w a" . rotate-frame-anticlockwise)))

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

;;; Load a host-specific file, if one exists.
(let ((host-file (format "~/.emacs.d/%s.el" system-name)))
  (if (file-exists-p host-file)
      (load host-file)))
