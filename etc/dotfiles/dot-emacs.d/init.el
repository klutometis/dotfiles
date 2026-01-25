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


(use-package compile
  :config
  (setq compilation-ask-about-save nil)
  (setq compilation-always-kill t))

;; Modern completion stack: Corfu + Cape + Eglot
(use-package corfu
  :custom
  (corfu-auto t)                      ;; Enable auto completion
  (corfu-auto-delay 0.1)              ;; Delay before showing completions
  (corfu-auto-prefix 2)               ;; Minimum prefix length for auto completion
  (corfu-cycle t)                     ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect 'prompt)           ;; Preselect the prompt
  (corfu-on-exact-match nil)          ;; Don't auto-insert on exact match
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)))

(use-package cape
  :init
  ;; Add completion sources to completion-at-point-functions
  ;; Order matters: earlier sources are tried first
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; cape-ispell or cape-dict for spell checking completions
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  :config
  ;; Make cape-dabbrev case-sensitive for better matches
  (setq cape-dabbrev-check-other-buffers t))

(use-package eglot
  :hook ((c++-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (sh-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)  ;; Shutdown server when last managed buffer is killed
  (eglot-sync-connect nil) ;; Async connection
  :config
  ;; Add clangd arguments for C++
  (add-to-list 'eglot-server-programs
               '((c++-mode c-mode) . ("clangd"
                                      "--background-index"
                                      "--clang-tidy"
                                      "--header-insertion=iwyu"
                                      "--suggest-missing-includes")))
  ;; Ensure cape-capf is used for eglot completion
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))



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
  (mouse-wheel-scroll-amount '(1 ((shift) . 5)))
  ;; Send backups to alternative location
  (backup-directory-alist `(("." . "~/.emacs.d/backups")))
  (vc-make-backup-files t)
  (kept-old-versions 5)
  (kept-new-versions 5)
  ;; Custom file location
  (custom-file "~/.emacs.d/custom.el")
  ;; Compilation settings
  (compilation-scroll-output t)
  ;; Disable visible bell
  (visible-bell nil)

  :config
  ;; Enable mouse support in terminal
  (xterm-mouse-mode 1)

  ;; Show the column-number in addition to row-number
  (column-number-mode 1)

  ;; Enable subword mode globally
  (global-subword-mode 1)

  ;; Typed text replaces selection
  (delete-selection-mode t)

  ;; Load custom file if it exists
  (when (file-exists-p custom-file)
    (load custom-file))

  ;; Enable upcase-region
  (put 'upcase-region 'disabled nil)

  (defun toggle-terminal-mouse ()
    (interactive)
    (if (bound-and-true-p xterm-mouse-mode)
        (progn
          (xterm-mouse-mode -1)
          (message "Mouse disabled. Alacritty copy/paste restored."))
      (xterm-mouse-mode 1)
      (message "Mouse enabled in Emacs.")))

  ;; Helper functions from init-settings.el

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

  (defun kill-line-backward (arg)
    "Kill ARG lines backward."
    (interactive "p")
    (kill-line (- 1 arg)))

  (defun copy-file-name-to-clipboard ()
    "Copy the current buffer file name to the clipboard."
    (interactive)
    (let ((filename (if (equal major-mode 'dired-mode)
                        default-directory
                      (buffer-file-name))))
      (when filename
        (kill-new filename)
        (message "Copied buffer file name '%s' to the clipboard." filename))))

  (defun save-all-buffers-no-confirmation (orig-func &rest args)
    "Save all buffers without confirmation."
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t))
              ((symbol-function 'yes-or-no-p) (lambda (prompt) t)))
      (apply orig-func args)))

  (advice-add 'save-some-buffers :around #'save-all-buffers-no-confirmation)

  (defun save-all-file-buffers ()
    "Save all buffers with file names that are modified, without confirmation."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (buffer-modified-p))
          (save-buffer)))))

  (defun select-next-subword ()
    "Extend selection to the next subword, or select the next subword if none is selected."
    (interactive)
    (if (use-region-p)
        (progn
          (goto-char (region-end))
          (subword-right 1))
      (progn
        (set-mark (point))
        (subword-right 1))))

  (defun current-buffer-not-mini ()
    "Return current-buffer if current buffer is not the *mini-buffer*
  else return buffer before minibuf is activated."
    (if (not (window-minibuffer-p)) (current-buffer)
      (if (eq (get-lru-window) (next-window))
          (window-buffer (previous-window)) (window-buffer (next-window)))))

  (define-key minibuffer-local-map
              (kbd "C-c TAB") (lambda () (interactive)
                                (insert (buffer-name (current-buffer-not-mini)))))

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

  (defun get-buffers-matching-extension (extension)
    (let ((matching-buffers '()))
      (dolist (buffer (buffer-list) matching-buffers)
        (let ((file-name (buffer-file-name buffer)))
          (if (and file-name
                   (string= (file-name-extension file-name)
                            extension))
              (add-to-list 'matching-buffers buffer))))))

  (defun multi-occur-with-this-extension ()
    "Show all lines matching REGEXP in buffers whose filenames have
this extension."
    (interactive)
    (multi-occur
     (get-buffers-matching-extension (file-name-extension buffer-file-name))
     (car (occur-read-primary-args))))

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

  (defun smart-open-line ()
    "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
    (interactive)
    (move-end-of-line nil)
    (newline-and-indent))

  (defun smart-open-line-above ()
    "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
    (interactive)
    (move-beginning-of-line nil)
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode))

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

  (defun y-or-n-p-with-return (orig-func &rest args)
    "Make RET act as yes in y-or-n-p prompts."
    (let ((query-replace-map (copy-keymap query-replace-map)))
      (define-key query-replace-map (kbd "RET") 'act)
      (apply orig-func args)))

  (advice-add 'y-or-n-p :around #'y-or-n-p-with-return)

  (defalias 'yes-or-no-p 'y-or-n-p)

  :bind (("C-c ?"     . help-for-help)
         ("M-z"       . zap-to-char)
         ("C-<M-z>"   . zap-up-to-char)
         ("C-c m"     . chmod-current-file)
         ("C-c M-m"   . toggle-terminal-mouse)
         ;; Get rid of suspension (messes with tmux).
         ("C-z" . nil)
         ("C-x C-z" . nil)
         ("C-a"       . smarter-move-beginning-of-line)
         ("C-c ;"     . comment-or-uncomment-region)
         ("C-c C-k"   . copy-line)
         ("C-c C-o"   . multi-occur-in-matching-buffers)
         ("C-c O"     . multi-occur-with-this-extension)
         ("C-c P"     . copy-file-name-to-clipboard)
         ("C-c R"     . recompile)
         ("C-c U"     . rename-uniquely)
         ("C-c a"     . list-matching-lines)
         ("C-c c"     . compile)
         ("C-c h"     . help-command)
         ("C-c o"     . occur)
         ("C-c p"     . pwd)
         ("C-c u"     . kill-line-backward)
         ("C-h"       . kill-whole-line)
         ("C-o"       . smart-open-line-above)
         ("C-x C-r"   . revert-buffer)
         ("C-x s"     . save-all-file-buffers)
         ("C-x TAB"   . indent-rigidly)
         ("M-%"       . query-replace-regexp)
         ("M-;"       . comment-dwim)
         ("M-o"       . smart-open-line))

  :hook
  ((find-file-hook . (lambda () (setq buffer-save-without-query t)))
   (sh-mode-hook . (lambda () (add-hook 'before-save-hook #'whitespace-cleanup nil :local)))))

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
         ("<f1> a" . apropos)        ; was helm-apropos
         ("C-c h o" . consult-line)          ; was helm-occur
         ("C-x b" . consult-buffer)          ; was helm-buffers-list
         ("M-y" . consult-yank-pop)          ; was helm-show-kill-ring
         ;; Replace grep/find bindings
         ("C-c r" . consult-ripgrep)         ; Project root
         ("C-c R" . consult-ripgrep-current-dir)  ; Current directory
         ("C-c f" . consult-find)            ; was find-grep-dired
         ;; Additional useful commands
         ("M-g i" . consult-imenu)
         ("M-g M-g" . consult-goto-line)
         ("C-x C-r" . consult-recent-file))
  :custom
  (consult-narrow-key "<")  ; Use < to narrow
  :config
  ;; Helper function for ripgrep from current directory
  (defun consult-ripgrep-current-dir ()
    "Run consult-ripgrep starting from current directory."
    (interactive)
    (consult-ripgrep default-directory))

  ;; Use ripgrep if available, otherwise fallback to grep
  (when (executable-find "rg")
    (setq consult-ripgrep-args
          "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip"))

  ;; Enable automatic preview for ripgrep and other search commands
  (consult-customize
   consult-ripgrep consult-grep consult-git-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; Preview with 0.2s debounce to avoid flickering
   :preview-key '(:debounce 0.2 any))

  ;; Immediate preview for yank-ring (like Helm)
  (consult-customize
   consult-yank-pop consult-yank-from-kill-ring
   :preview-key 'any))

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
