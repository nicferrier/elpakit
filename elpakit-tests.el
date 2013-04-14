;;; Tests for elpakit

(require 'ert)

(ert-deftest elpakit/mematch ()
  (should
   (equal
    (list "README.md")
    (elpakit/mematch
     "README\\..*"
     (list "some.el" "files.txt" "README.md"))))
  (should-not
   (elpakit/mematch
    "README\\..*"
    (list "some.el" "files.txt"))))

(ert-deftest elpakit/find-recipe ()
  (should
   (equal
    "/home/nferrier/work/emacs-db/recipes/db"
    (elpakit/find-recipe "~/work/emacs-db"))))

(ert-deftest elpakit/package-files ()
  (should
   (equal
    '("/home/nferrier/work/emacs-db/db.el")
    (elpakit/package-files
     (elpakit/get-recipe "~/work/emacs-db")))))

(ert-deftest elpakit/make-pkg-lisp ()
  (condition-case nil
      (delete-directory "/tmp/elpakittest/db-0.0.1" t)
    (error nil))
  (make-directory "/tmp/elpakittest/db-0.0.1" t)
  (with-current-buffer (find-file-noselect "~/work/emacs-db/db.el")
    (elpakit/make-pkg-lisp
     "/tmp/elpakittest/db-0.0.1"
     (save-excursion (package-buffer-info))))
  (should
   (equal
    (concat "(define-package \"db\" \"0.0.1\" "
            "\"A database for EmacsLisp\" (quote ((kv \"0.0.9\"))))\n")
    (with-current-buffer
        (find-file-noselect "/tmp/elpakittest/db-0.0.1/db-pkg.el")
      (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest elpakit/build-single ()
  (condition-case nil
      (delete-directory "/tmp/elpakittest" t)
    (error nil))
  (elpakit/build-single "/tmp/elpakittest" "~/work/emacs-db/db.el")
  ;;; (should (file-exists-p "/tmp/elpakittest/db-0.0.1/db-pkg.el"))
  ;;; (should (file-exists-p "/tmp/elpakittest/db-0.0.1/db-autoloads.el"))
  (should (file-exists-p "/tmp/elpakittest/db-0.0.1/db.el")))

(ert-deftest elpakit/do ()
  "Test that we can do a single file package."
  (condition-case nil
      (delete-directory "/tmp/elpakittest" t)
    (error nil))
  (should
   (equal
    ["db"
     ((kv (0 0 9)))
     "A database for EmacsLisp"
     "0.0.1"]
    ;; Return the package info WITHOUT the README
    (substring
     (elpakit/do "/tmp/elpakittest" "~/work/emacs-db")
     0 4)))
  ;; Not doing either autoloads OR pkg files for singles now
  ;;; (should (file-exists-p "/tmp/elpakittest/db-0.0.1/db-autoloads.el"))
  ;;; (should (file-exists-p "/tmp/elpakittest/db-0.0.1/db-pkg.el"))
  (should (file-exists-p "/tmp/elpakittest/db-0.0.1/db.el")))

(ert-deftest elpakit/recipe->package-decl ()
  (let ((recipe
         '(elnode
           :version "0.9.9.1"
           :doc "The Emacs webserver."
           :files ("elnode.el"
                   "elnode-tests.el"
                   "elnode-rle.el"
                   "elnode-wiki.el"
                   "default-wiki-index.creole"
                   "default-webserver-test.html"
                   "default-webserver-image.png"
                   "README.creole"
                   "COPYING")
           :requires((web "0.1.4")
                     (db "0.0.1")
                     (kv "0.0.9")
                     (fakir "0.0.14")
                     (creole "0.8.14")))))
    ;; FIXME test the README stuff - elnode is a crap example because
    ;; it has a massive readme
    (should
     (equal
      '(define-package "elnode" "0.9.9.1"
        "The Emacs webserver."
        ((web "0.1.4")
         (db "0.0.1")
         (kv "0.0.9")
         (fakir "0.0.14")
         (creole "0.8.14")))
      (elpakit/recipe->package-decl recipe)))))

(ert-deftest elpakit/build-multi ()
  (condition-case nil
      (delete-directory "/tmp/elpakittest" t)
    (error nil))
  (should
   (equal
    ["elnode"
     ((web (0 1 4))
      (db (0 0 1))
      (kv (0 0 9))
      (fakir (0 0 14))
      (creole (0 8 14)))
     "The Emacs webserver."
     "0.9.9.6.1"]
    ;; Return the package info WITHOUT the README
    (substring
     (elpakit/build-multi
      "/tmp/elpakittest"
      (elpakit/get-recipe 
       "~/work/elnode-auth")) 0 4)))
  ;; Test the interveening files
  (should (file-exists-p "/tmp/elnode-0.9.9.6.1/elnode.el"))
  (should (file-exists-p "/tmp/elnode-0.9.9.6.1/elnode-pkg.el"))
  ;; Test the tar ball
  (should (file-exists-p "/tmp/elpakittest/elnode-0.9.9.6.1.tar")))

(ert-deftest elpakit/do-multi ()
  ;; Test the do for a multi-file package
  (should
   (equal
    ["elnode"
     ((web (0 1 4))
      (db (0 0 1))
      (kv (0 0 9))
      (fakir (0 0 14))
      (creole (0 8 14)))
     "The Emacs webserver."
     "0.9.9.6.1"]
    ;; Return the package info WITHOUT the README
    (substring
     (elpakit/do "/tmp/elpakittest" "~/work/elnode-auth")
     0 4))))

(ert-deftest elpakit/recipe->pkg-info ()
  "Test turning recipe into the internal vector package type."
  (let ((recipe
         '(elnode
           :version "0.9.9.1"
           :doc "The Emacs webserver."
           :files ("elnode.el"
                   "elnode-tests.el"
                   "elnode-rle.el"
                   "elnode-wiki.el"
                   "default-wiki-index.creole"
                   "default-webserver-test.html"
                   "default-webserver-image.png"
                   "README.creole"
                   "COPYING")
           :requires ((web "0.1.4")
                      (db "0.0.1")
                      (kv "0.0.9")
                      (fakir "0.0.14")
                      (creole "0.8.14")))))
    (should
     (equal
      ["elnode"
       ((web (0 1 4))
        (db (0 0 1))
        (kv (0 0 9))
        (fakir (0 0 14))
        (creole (0 8 14)))
       "The Emacs webserver."
       "0.9.9.1"]
      ;; We do take off the README
      (subseq 
       (elpakit/recipe->pkg-info recipe)
       0 4)))))

(ert-deftest elpakit/pjg-info->versioned-name ()
  "Test turning recipe into the internal vector package type."
  (let ((recipe
         '(elnode
           :version "0.9.9.1"
           :doc "The Emacs webserver."
           :files ("elnode.el"
                   "elnode-tests.el"
                   "elnode-rle.el"
                   "elnode-wiki.el"
                   "default-wiki-index.creole"
                   "default-webserver-test.html"
                   "default-webserver-image.png"
                   "README.creole"
                   "COPYING")
           :requires ((web "0.1.4")
                      (db "0.0.1")
                      (kv "0.0.9")
                      (fakir "0.0.14")
                      (creole "0.8.14")))))
    (should
     (equal "elnode-0.9.9.1"
            (elpakit/pkg-info->versioned-name
             (elpakit/recipe->pkg-info recipe))))))

;;; elpakit-tests.el ends here
