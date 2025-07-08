;;; ~/.emacs-exwm.d/init.el -*- lexical-binding: t; -*-

;; Bootstrap straight.el
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

;; Disable package.el
(setq package-enable-at-startup nil)

;; Integrate use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq package-install-upgrade-built-in t)

;; Optional: auto-update all packages on Emacs startup
(add-hook 'emacs-startup-hook #'straight-pull-all)

;; ========== EXWM Setup ==========
(use-package exwm
  :config
  ;; Disable menu-bar, tool-bar and scroll-bar to increase the usable space.
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  ;; Also shrink fringes to 1 pixel.
  (fringe-mode 1)

  ;; Turn on `display-time-mode' if you don't use an external bar.
  (setq display-time-default-load-average nil)
  (display-time-mode t)

  ;; You are strongly encouraged to enable something like `icomplete-vertical-mode' to alter
  ;; the default behavior of 'C-x b', or you will take great pains to switch
  ;; to or back from a floating frame (remember 'C-x 5 o' if you refuse this
  ;; proposal however).
  (icomplete-vertical-mode 1)

  ;; Emacs server is not required to run EXWM but it has some interesting uses
  ;; (see next section).
  (setq server-name "exwm")
  (server-start)

  ;; Load EXWM.
  (require 'exwm)

  ;; Set the initial number of workspaces (they can also be created later).
  (setq exwm-workspace-number 4)

  ;; All buffers created in EXWM mode are named "*EXWM*". You may want to
  ;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
  ;; are run when a new X window class name or title is available.  Here's
  ;; some advice on this topic:
  ;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
  ;; + For applications with multiple windows (e.g. GIMP), the class names of
  ;;    all windows are probably the same.  Using window titles for them makes
  ;;   more sense.
  ;; In the following example, we use class names for all windows except for
  ;; Java applications and GIMP.
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                          (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-class-name))))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (when (or (not exwm-instance-name)
                        (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-title))))

  ;; Make EXWM completely transparent - ONLY s-t prefixed commands
  (setq exwm-input-global-keys
        `(
          ;; Universal prefix s-t for EXWM management (Dvorak-friendly)
          (,(kbd "s-t r") . exwm-reset)
          (,(kbd "s-t w") . exwm-workspace-switch)
          (,(kbd "s-t &") . (lambda (command)
                             (interactive (list (read-shell-command "$ ")))
                             (start-process-shell-command command nil command)))
          (,(kbd "s-t x") . (lambda (command)
                             (interactive (list (read-shell-command "M-x ")))
                             (start-process-shell-command command nil command)))
          (,(kbd "s-t l") . (lambda ()
                             (interactive)
                             (start-process "" nil "/usr/bin/slock")))
          
          ;; Workspace switching with s-t prefix
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-t %d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  ;; To add a key binding only available in line-mode, simply define it in
  ;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
  (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

  ;; Simulation keys for line-mode (when EXWM has control)
  (setq exwm-input-simulation-keys
        '(
          ;; movement
          ([?\C-b] . [left])
          ([?\M-b] . [C-left])
          ([?\C-f] . [right])
          ([?\M-f] . [C-right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])
          ;; cut/paste.
          ([?\C-w] . [?\C-x])
          ([?\M-w] . [?\C-c])
          ([?\C-y] . [?\C-v])
          ;; search
          ([?\C-s] . [?\C-f])))

  ;; Make terminals start in char-mode by default
  (setq exwm-manage-configurations
        '(((string= exwm-class-name "Alacritty")
           char-mode t)))

  ;; You can hide the minibuffer and echo area when they're not used, by
  ;; uncommenting the following line.
                                        ;(setq exwm-workspace-minibuffer-position 'bottom)

  ;; Do not forget to enable EXWM. It will start by itself when things are
  ;; ready.  You can put it _anywhere_ in your configuration.
  (exwm-enable))
