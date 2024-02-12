(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq package-install-upgrade-built-in t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

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

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package bazel
  :hook ((bazel-mode-hook
	  . (lambda () (add-hook 'before-save-hook 'bazel-mode-buildifier)))))

(use-package better-defaults
  :config
  ;;; Don't flash on bell, after all.
  (setq visible-bell nil))

(use-package compile
  :config
  (setq compilation-ask-about-save nil)
  (setq compilation-always-kill t))

(use-package desktop
  :config
  (desktop-save-mode 1)
  (add-to-list 'desktop-globals-to-save 'kill-ring))

(use-package dired-subtree
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)))

(use-package emacs
  :hook
  ((find-file-hook . (lambda () (setq buffer-save-without-query t)))))

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

(use-package format-all
  :config
  (add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
  (setq-default format-all-formatters
                '(
                  ("Bazel" (buildifier))
                  ("Emacs Lisp" (emacs-lisp))
                  ("JSON" (deno))
		  ("Markdown" (prettier "--prose-wrap=always"))
                  ("Python" (black))
                  ("JavaScript" (deno))
                  ("TypeScript" (deno))
                  ("YAML" (prettier))
                  ))
  (add-hook 'before-save-hook 'format-all-buffer))

(use-package full-ack)

(use-package gptel)

(use-package graphviz-dot-mode
  :init
  (setq graphviz-dot-indent-width 2))

(use-package grep
  :bind ("C-c r" . rgrep))

(use-package helm
  :config
  (helm-mode 1))
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
         ("C-c C-l" . helm-minibuffer-history)
	 ("C-c C-l" . helm-minibuffer-history)
         ("C-x C-f" . helm-find-files)
         ([tab] . helm-execute-persistent-action)
	 :map minibuffer-local-map
	 ("C-c C-l" . helm-minibuffer-history))
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

(use-package savehist
  :config
  (setq savehist-save-minibuffer-history 1)
  (setq savehist-additional-variables
        '(kill-ring search-ring regexp-search-ring compile-history log-edit-comment-ring)
        savehist-file "~/.emacs.d/savehist")
  (savehist-mode t))

(use-package sort
  :bind ("C-c s" . sort-lines))

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

;; Load a separate file containing all global settings
(load "~/.emacs.d/init-settings.el")
