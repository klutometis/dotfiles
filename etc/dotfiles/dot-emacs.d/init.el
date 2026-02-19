;; Bootstrap Elpaca  -*- lexical-binding: t; -*-
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode)
  ;; Assume :ensure t for all use-package declarations (like straight-use-package-by-default)
  (setq use-package-always-ensure t))

;; Block until elpaca-use-package is ready
(elpaca-wait)

;;; Load transient early and eagerly.
(use-package transient :ensure t :demand t)

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


(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package auth-source-pass
  :ensure nil
  :config
  (auth-source-pass-enable))

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

(use-package clipetty
  :custom
  ;; Force OSC 52 even on local terminal (Alacritty supports it)
  (clipetty-assume-nested-mux t)
  :config
  (global-clipetty-mode 1))

(use-package combobulate
  :ensure (:host github :repo "mickeynp/combobulate")
  :bind (("C-<M-a>" . combobulate-navigate-beginning-of-defun))
  :custom
  (combobulate-key-prefix "C-c o")
  :hook ((prog-mode . combobulate-mode)))


(use-package compile
  :ensure nil
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
  :config
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
  :ensure nil
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

(use-package activities
  :config
  (activities-mode)
  (activities-tabs-mode)
  :bind (("C-x C-a C-n" . activities-new)
         ("C-x C-a C-d" . activities-define)
         ("C-x C-a C-a" . activities-resume)
         ("C-x C-a C-s" . activities-suspend)
         ("C-x C-a C-k" . activities-kill)
         ("C-x C-a RET" . activities-switch)
         ("C-x C-a b"   . activities-switch-buffer)
         ("C-x C-a g"   . activities-revert)
         ("C-x C-a l"   . activities-list)))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package gptel
  :ensure t
  :custom
  (gptel-model 'claude-sonnet-4-6)
  (gptel-backend
   (gptel-make-anthropic "Claude"
     :stream t
     :key (lambda () (getenv "ANTHROPIC_API_KEY"))))
  :bind (("C-c G" . gptel)
         ("C-c M-g" . gptel-send)))

(use-package ai-code
  :ensure t
  :custom
  ;; Use gptel for AI-generated headlines in the prompt file
  (ai-code-use-gptel-headline t)
  (ai-code-notes-use-gptel-headline t)
  (ai-code-task-use-gptel-filename t)
  ;; Ask per-send: lets you choose TDD vs test-after-change vs off each time
  (ai-code-auto-test-type 'ask-me)
  ;; Desktop notifications when background sessions finish (Linux D-Bus)
  (ai-code-notifications-enabled t)
  (ai-code-notifications-show-on-response t)
  :config
  ;; Opencode is the backend
  (ai-code-set-backend 'opencode)
  ;; Use eat for mouse passthrough (try vterm if performance is an issue)
  (setq ai-code-backends-infra-terminal-backend 'eat)
  ;; Enable @ file completion in comments and AI sessions
  (ai-code-prompt-filepath-completion-mode 1)
  ;; Register ai-code's bundled snippets with yasnippet
  (with-eval-after-load 'yasnippet
    (add-to-list 'yas-snippet-dirs
                 (expand-file-name "snippets"
                                   (file-name-directory (locate-library "ai-code"))))
    (yas-reload-all))
  ;; AI commands in Magit popups
  (with-eval-after-load 'magit
    (ai-code-magit-setup-transients))
  ;; 1s auto-revert so AI edits appear immediately
  (setq auto-revert-interval 1)
  :bind
  ("C-c A" . ai-code-menu))

(use-package dired
  :ensure nil  ; Built-in package
  :custom
  ;; When two dired buffers are open, operations target the other window
  (dired-dwim-target t)
  ;; Show directories first, then files
  (dired-listing-switches "-alh --group-directories-first")
  ;; Auto-refresh dired buffers when files change
  (dired-auto-revert-buffer t)
  ;; Kill dired buffers when leaving them
  (dired-kill-when-opening-new-dired-buffer t))

(use-package dired-subtree
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)))

(use-package dired-narrow
  :bind (:map dired-mode-map
              ("/" . dired-narrow-fuzzy)))

(use-package dired-rainbow
  :config
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown "#ffed4e" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#c7c7ff" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#ffed4e" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
  (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

(use-package emacs
  :ensure nil
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
  (backup-by-copying t)
  (vc-make-backup-files t)
  (kept-old-versions 5)
  (kept-new-versions 5)
  ;; Custom file location
  (custom-file "~/.emacs.d/custom.el")
  ;; Compilation settings
  (compilation-scroll-output t)
  ;; Disable visible bell
  (visible-bell nil)
  ;; Better defaults settings
  (uniquify-buffer-name-style 'forward)
  (save-interprogram-paste-before-kill t)
  (apropos-do-all t)
  (mouse-yank-at-point t)
  (require-final-newline t)
  (load-prefer-newer t)
  (frame-inhibit-implied-resize t)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (indent-tabs-mode nil)

  :config
  ;; Enable mouse support in terminal
  (xterm-mouse-mode 1)

  ;; Show the column-number in addition to row-number
  (column-number-mode 1)

  ;; Enable subword mode globally
  (global-subword-mode 1)

  ;; Typed text replaces selection
  (delete-selection-mode t)

  ;; Auto-refresh buffers when files change on disk
  (global-auto-revert-mode 1)

  ;; UI cleanup (disable toolbars, scrollbars, menu bar)
  (unless (memq window-system '(mac ns))
    (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))

  ;; Better defaults modes
  (show-paren-mode 1)
  (save-place-mode 1)
  (require 'uniquify)

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
         ("C-c c"     . compile)
         ("C-c h"     . help-command)
         ("C-c o"     . occur)
         ("C-c p"     . pwd)
         ("C-c u"     . kill-line-backward)
         ("C-c k"     . kill-whole-line)
         ("C-o"       . smart-open-line-above)
         ("C-x s"     . save-all-file-buffers)
         ("C-x TAB"   . indent-rigidly)
         ("M-%"       . query-replace-regexp)
         ("M-;"       . comment-dwim)
         ("M-o"       . smart-open-line))

  :hook
  ((find-file-hook . (lambda () (setq buffer-save-without-query t)))
   (sh-mode-hook . (lambda () (add-hook 'before-save-hook #'whitespace-cleanup nil :local)))))

(use-package find-dired
  :ensure nil  ; Built-in package
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

(use-package flymake
  :ensure nil  ; Built-in package
  :bind (:map flymake-mode-map
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)))

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
  :ensure nil
  :custom
  (frame-background-mode 'dark)
  :hook
  (after-init . (lambda () (mapc 'frame-set-background-mode (frame-list)))))

(use-package graphviz-dot-mode
  :init
  (setq graphviz-dot-indent-width 2))

(use-package grep
  :ensure nil  ; Built-in package
  :config
  (setq grep-save-buffers 'save-all-file-buffers))

;; Vertico stack for completion - modern, modular alternative to Helm
(use-package vertico
  :config
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
  :config
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
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)))

(use-package language-id
  :config
  (push '("Bazel" (bazel-mode (language-id--file-name-regexp "BUILD"))) language-id--definitions)
  (push '("Graphviz" (graphviz-dot-mode (language-id--file-name-extension ".dot"))) language-id--definitions)
  (push '("Slidev" (markdown-mode (language-id--file-name-regexp "slides\\.md"))) language-id--definitions))

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
  :ensure nil
  :init
  (midnight-mode 1)
  :custom
  (midnight-delay 0)
  (clean-buffer-list-delay-general 1)
  (clean-buffer-list-kill-regexps '(".*"))
  :hook
  (midnight . (lambda () (desktop-save desktop-dirname)))
  (midnight . (lambda ()
                (message "Elpaca: Starting package update...")
                (elpaca-fetch-all t)
                (elpaca-merge-all nil t))))

(use-package paredit
  :bind (:map paredit-mode-map
              ("M-(" . paredit-wrap-round)
              ("M-)" . paredit-clone-round-and-newline))
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)
         (scheme-mode . paredit-mode)))

(use-package eat
  :ensure t
  :config
  (defun eat-send-escape ()
    "Send a literal ESC to the eat subprocess."
    (interactive)
    (eat-self-input 1 ?\e))
  :bind (:map eat-semi-char-mode-map
         ("C-g" . eat-send-escape)
         :map eat-char-mode-map
         ("C-g" . eat-send-escape)))

;; for vterm terminal backend:
(use-package vterm
  :ensure t
  :config
  (defun vterm-send-escape ()
    "Send a literal ESC to the vterm subprocess."
    (interactive)
    (vterm-send-key "<escape>"))
  :bind (:map vterm-mode-map
         ;; C-g sends literal ESC to vterm (e.g. to dismiss opencode UI)
         ("C-g" . vterm-send-escape)))

(use-package projectile
  :config
  (projectile-mode +1))

;;; Make pulse more readable in terminal.
(use-package pulse
  :ensure nil
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
  :ensure nil  ; Built-in package
  :custom
  (savehist-save-minibuffer-history t)
  ;; Increase history length from default 100
  (history-length 1000)
  (savehist-additional-variables
   '(minibuffer-history
     compile-history
     extended-command-history
     kill-ring
     search-ring
     regexp-search-ring
     log-edit-comment-ring
     file-name-history
     ;; Consult-specific histories
     consult--grep-history
     consult--find-history
     consult--line-history
     consult--buffer-history))
  (savehist-file "~/.emacs.d/savehist")
  ;; (savehist-autosave-interval 600) ; optional CPU fix
  :config
  (savehist-mode 1))

(use-package sort
  :ensure nil  ; Built-in package
  :bind ("C-c s" . sort-lines))

(use-package typescript-mode
  :config
  (setq typescript-indent-level 2))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  (treesit-font-lock-level 6)
  :config
  ;; Suppress annoying tree-sitter font-lock mismatch warnings
  (setq warning-suppress-types '((treesit-font-lock-rules-mismatch)))
  (global-treesit-auto-mode)
  (treesit-auto-add-to-auto-mode-alist 'all))

(use-package transpose-frame
  :bind (("C-c w t" . transpose-frame)
         ("C-c w f" . flip-frame)
         ("C-c w F" . flop-frame)
         ("C-c w r" . rotate-frame)
         ("C-c w c" . rotate-frame-clockwise)
         ("C-c w a" . rotate-frame-anticlockwise)))

(use-package which-key
  :config
  (which-key-mode)
  :custom
  (which-key-idle-delay 0.3)
  (which-key-sort-order 'which-key-key-order-alpha))

(use-package windmove
  :ensure nil
  :bind (("<up>" . windmove-up)
         ("<down>" . windmove-down)
         ("<right>" . windmove-right)
         ("<left>" . windmove-left))
  :config
  (windmove-default-keybindings)
  (setq windmove-wrap-around t))

;;; For manipulating stacks of windows.
(use-package winner
  :ensure nil  ; Built-in package
  :config
  (winner-mode 1))

;;; Load a host-specific file, if one exists.
(let ((host-file (format "~/.emacs.d/%s.el" system-name)))
  (if (file-exists-p host-file)
      (load host-file)))
