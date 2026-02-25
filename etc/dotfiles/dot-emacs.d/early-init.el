;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;; Disable package.el in favor of Elpaca
(setq package-enable-at-startup nil)

;; Suppress tree-sitter font-lock mismatch warnings (grammar vs major mode
;; version skew in Emacs 31 dev builds). The affected font-lock features
;; degrade gracefully; this just prevents the *Warnings* buffer from popping up.
(setq warning-suppress-types '((treesit-font-lock-rules-mismatch)))

;;; early-init.el ends here
