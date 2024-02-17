;;; Save all buffers, no confirmation (doesn't seem to work).
(defun save-all-buffers-no-confirmation (orig-func &rest args)
  "Save all buffers without confirmation."
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t))
            ((symbol-function 'yes-or-no-p) (lambda (prompt) t)))
    (apply orig-func args)))

(advice-add 'save-some-buffers :around #'save-all-buffers-no-confirmation)

;;; This one seems to work; how to make it work with recompile, etc.?
(defun save-all-file-buffers ()
  "Save all buffers with file names that are modified, without confirmation."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (buffer-modified-p))
        (save-buffer)))))

;;; Subword mode
(global-subword-mode 1)

(defun select-next-subword ()
  "Extend selection to the next subword, or select the next subword if none is selected."
  (interactive)
  (if (use-region-p)
      (progn
        ;; If a region is selected, move to the end of the selection
        (goto-char (region-end))
        ;; Extend the selection to the next subword
        (subword-right 1))
    (progn
      ;; If no region is selected, start a new selection
      (set-mark (point))
      (subword-right 1))))

;;; Send backups to alternative location.
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq vc-make-backup-files t)
(setq kept-old-versions 5)
(setq kept-new-versions 5)

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

;;; Typed text replaces selection.
(delete-selection-mode t)

;;; Show the column-number in addition to the row-number in the
;;; status-bar.
(setq column-number-mode t)

;;; Set the location of the custom file
(setq custom-file "~/.emacs.d/custom.el")

;;; Load the custom file. Does nothing if the file doesn't exist.
(when (file-exists-p custom-file)
  (load custom-file))

;;; Load a host-specific file, if one exists.
(let ((host-file (format "~/.emacs.d/%s.el" system-name)))
  (if (file-exists-p host-file)
      (load host-file)))

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

;;; Let's scroll to the bottom instead of first-error.
(setq compilation-scroll-output t)

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
 ("C-c C-o" . multi-occur-in-matching-buffers)
 ("C-c O" . multi-occur-with-this-extension)
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
 ("C-c w" . select-next-subword)
 ("C-h" . kill-whole-line)
 ("C-o" . smart-open-line-above)
 ("C-x C-f" . helm-find-files)
 ("C-x C-r" . revert-buffer)
 ("C-x TAB" . indent-rigidly)
 ("M-%" . query-replace-regexp)
 ("M-;" . comment-dwim)
 ("M-o" . smart-open-line)
 )

(defalias 'yes-or-no-p 'y-or-n-p)

;;; https://emacs.stackexchange.com/a/17277
(defun y-or-n-p-with-return (orig-func &rest args)
  (let ((query-replace-map (copy-keymap query-replace-map)))
    (define-key query-replace-map (kbd "RET") 'act)
    (apply orig-func args)))

(advice-add 'y-or-n-p :around #'y-or-n-p-with-return)

(put 'upcase-region 'disabled nil)

(setq visible-bell nil)

(add-hook 'sh-mode-hook
          (lambda () (add-hook 'before-save-hook #'whitespace-cleanup nil :local)))
