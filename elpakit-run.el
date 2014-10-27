;;; elpakit-run.el --- run elpakit from the command line

;; run with:
;;
;;  MARMALADE=1 emacs -batch -l elpakit-run.el package-to-install
;;
;; or:
;;
;;  MELPA=1 emacs -batch -l elpakit-run.el package-to-install
;;

(when (member
       "elpakit-run.el"
       (mapcar 'file-name-nondirectory command-line-args))
  (let ((package-name (car-safe (reverse command-line-args)))
        (package-user-dir (make-temp-name "elpakit-run")))
    (package-initialize)
    (when (getenv "MARMALADE")
      (add-to-list
       'package-archives
       '("marmalade" . "http://marmalade-repo.org/packages/")))
    (when (getenv "MELPA")
      (add-to-list
       'package-archives
       '("melpa" . "http://melpa.org/packages/")))
    (package-refresh-contents)
    (if (assoc (intern package-name) package-archive-contents)
        (package-install (intern package-name))
        (error "no package found: %s" package-name))))

;;; elpakit-run.el ends here
