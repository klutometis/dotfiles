;;; surfraw.el --- Surfraw

;;; Code:

(require 'cl)

;;; Use this to populate a list of surfraw-functions à la stumpwm; see
;;; e.g. <https://github.com/stumpwm/stumpwm-contrib/blob/master/util/surfraw/surfraw.lisp>.
(mapcar (lambda (line)
          (car (split-string line)))
        (remove-if (lambda (line)
                     (string-prefix-p " " line))
                   (process-lines "surfraw" "-elvi")))

(provide 'surfraw)

;;; surfraw.el ends here
