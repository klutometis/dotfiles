;;; surfraw.el --- Surfraw

;; Author: Peter Danenberg <pcd@roxygen.org>
;; URL: http://github.com/klutometis/surfraw-emacs
;; Version: 0.1
;; Package-Requires: ((cl-lib "0.1")
;;                    (thingatpt+ "0"))

;;; Code:

(require 'cl-lib)
(require 'thingatpt+)

(defvar surfraw-region
  #'tap-region-or-non-nil-symbol-name-nearest-point
  "Function that describes how surfraw extracts the current
  region")

(defun surfraw-elvi ()
  "Extract a list of current elvi."
  (cl-mapcar
   (lambda (line)
     (car (split-string line)))
   (cl-remove-if (lambda (line)
                   (string-prefix-p " " line))
                 (process-lines "surfraw" "-elvi"))))

(defun surfraw-start-elvis (elvis)
  "Start surfraw with a given elvis."
  (apply-partially 'start-process "surfraw" nil "surfraw" elvis))

(defmacro defun-surfraw-elvi ()
  "Define a function `surfraw-<elvis>' for every elvis, which
operates on a list of search terms as a string."
  `(progn
     ,@(cl-mapcar
        (lambda (elvis)
          `(defun ,(intern (format "surfraw-%s" elvis)) (terms)
             (interactive ,(format "ssurfraw %s: " elvis))
             (apply (surfraw-start-elvis ,elvis) (split-string-and-unquote terms))))
        (surfraw-elvi))))

(defmacro defun-surfraw-region-elvi ()
  "Define a function `surfraw-<elvis>-region' for every elvis,
which operates on the region or non-nil symbol-name nearest
point."
  `(progn
     ,@(cl-mapcar
        (lambda (elvis)
          `(defun ,(intern (format "surfraw-%s-region" elvis)) ()
             (interactive)
             (,(intern (format "surfraw-%s" elvis))
              (funcall surfraw-region))))
        (surfraw-elvi))))

(defun surfraw-init ()
  "Initialize the surfraw-elvi and surfraw-region-elvi
functions."
  (defun-surfraw-elvi)
  (defun-surfraw-region-elvi))

(provide 'surfraw)

;;; surfraw.el ends here
