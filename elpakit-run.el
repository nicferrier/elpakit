;;; elpakit-run.el --- run elpakit from the command line

;; run with:
;;
;;  emacs -batch -l elpakit-run.el 

(when (member
       "elpakit-run.el"
       (mapcar 'file-name-nondirectory command-line-args))
  (let ((elpakit-command (car-safe (reverse command-line-args)))
        (package-user-dir (make-temp-name "/tmp/elpakit-run")))
    (package-initialize)
    (add-to-list
     'package-archives
     '("marmalade" . "http://marmalade-repo.org/packages/"))
    (package-refresh-contents)
    (package-install 'elpakit)
    (elpakit-make-multi ".")))

;;; elpakit-run.el ends here
