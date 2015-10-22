;;; surfraw.el --- Surfraw

;;; Code:

(require 'cl)

;;; Use this to populate a list of surfraw-functions
(mapcar (lambda (line)
          (car (split-string line)))
        (remove-if (lambda (line)
                     (string-prefix-p " " line))
                   (process-lines "surfraw" "-elvi")))

(provide 'surfraw)

;;; surfraw.el ends here
