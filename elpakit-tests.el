;;; Tests for elpakit -*- lexical-binding: t -*-

(require 'ert)
(require 'shadchen)
(require 'noflet)

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
  "Test whether we can find a recipt file of not"
  (should
   (equal
    (concat
     (file-name-directory
      (or
       (buffer-file-name)
       load-file-name
       default-directory))
     "emacs-kv/recipes/kv")
    (elpakit/find-recipe "emacs-kv"))))

(ert-deftest elpakit/infer-files ()
  "Test the file inferring."
  (should
   (noflet ((elpakit/git-files (package-dir) ; fake the git-files for the fake package
              (directory-files "emacs-db" nil "^[^.#].*[^#~]$")))
     (equal
       (match
         (elpakit/infer-files "emacs-db")
         ((plist :elisp-files elisp :test-files tests)
          (list elisp tests)))
       '(("db.el" "db-tests.el")
         ("db-tests.el"))))))

(ert-deftest elpakit/file->package ()
  "Test turning a file into a package and the access API."
  ;; This is really fucking difficult to test with the different emacs versions.
  (should
   (with-elpakit-new-package-api 
     (package-desc-p (elpakit/file->package "db.el"))
     (vectorp (elpakit/file->package "db.el"))))
  ;; This should always be the same - pull a bunch of things
  (should
   (equal
    '("0.0.6" "db")
    (elpakit/file->package "db.el" :version :name))))

(ert-deftest elpakit/package-files ()
  (should
   (equal
    (list (concat
           (file-name-directory
            (or
             (buffer-file-name)
             load-file-name
             default-directory)) "emacs-db/db.el"))
    (elpakit/package-files
     (elpakit/get-recipe "emacs-db")))))

(ert-deftest elpakit/make-pkg-lisp ()
  (condition-case nil
      (delete-directory "/tmp/elpakittest/db-0.0.6" t)
    (error nil))
  (make-directory "/tmp/elpakittest/db-0.0.6" t)
  (with-temp-buffer
    (insert-file-contents "emacs-db/db.el")
    (elpakit/make-pkg-lisp
     "/tmp/elpakittest/db-0.0.6"
     (save-excursion (package-buffer-info))))
  (should
   (equal
    (concat "(define-package \"db\" \"0.0.6\" "
            "\"A database for EmacsLisp\" (quote ((kv \"0.0.11\"))))\n")
    (with-temp-buffer
      (insert-file-contents "/tmp/elpakittest/db-0.0.6/db-pkg.el")
      (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest elpakit/build-single ()
  (condition-case nil
      (delete-directory "/tmp/elpakittest" t)
    (error nil))
  (elpakit/build-single "/tmp/elpakittest" "emacs-db/db.el")
  ;;; (should (file-exists-p "/tmp/elpakittest/db-0.0.1/db-pkg.el"))
  ;;; (should (file-exists-p "/tmp/elpakittest/db-0.0.1/db-autoloads.el"))
  (should (file-exists-p "/tmp/elpakittest/db-0.0.6/db.el")))

(ert-deftest elpakit/do ()
  "Test that we can do a single file package."
  (condition-case nil
      (delete-directory "/tmp/elpakittest" t)
    (error nil))
  (should
   (equal
    (elpakit/package-info
     (cdr (car (elpakit/do "/tmp/elpakittest" "emacs-db")))
     :name :reqs :summary :version)
    `("db"
      ((kv (0 0 11)))
      "A database for EmacsLisp"
      "0.0.6")))
  ;; Not doing either autoloads OR pkg files for singles now
  ;;; (should (file-exists-p "/tmp/elpakittest/db-0.0.1/db-autoloads.el"))
  ;;; (should (file-exists-p "/tmp/elpakittest/db-0.0.1/db-pkg.el"))
  (should (file-exists-p "/tmp/elpakittest/db-0.0.6/db.el")))

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
    '("elnode"
     ((web (0 4 3))
      (dash (1 1 0))
      (noflet (0 0 7))
      (s (1 5 0))
      (creole (0 8 14))
      (fakir (0 1 6))
      (db (0 0 5))
      (kv (0 0 17)))
     "The Emacs webserver."
      "0.9.9.8.7")
    ;; Return the package info WITHOUT the README
    (elpakit/package-info
     (elpakit/build-multi "/tmp/elpakittest" (elpakit/get-recipe "elnode-auth"))
     :name :reqs :summary :version)))
  ;; Test the interveening files
  (should (file-exists-p "/tmp/elnode-0.9.9.8.7/elnode.el"))
  (should (file-exists-p "/tmp/elnode-0.9.9.8.7/elnode-pkg.el"))
  ;; Test the tar ball
  (should (file-exists-p "/tmp/elpakittest/elnode-0.9.9.8.7.tar")))

(ert-deftest elpakit/do-multi ()
  ;; Test the do for a multi-file package
  (should
   (equal
    '("elnode"
      ((web (0 4 3))
       (dash (1 1 0))
       (noflet (0 0 7))
       (s (1 5 0))
       (creole (0 8 14))
       (fakir (0 1 6))
       (db (0 0 5))
       (kv (0 0 17)))
      "The Emacs webserver."
      "0.9.9.8.7")
    ;; Return the package info WITHOUT the README
    (elpakit/package-info
     (cdar (elpakit/do "/tmp/elpakittest" "elnode-auth"))
     :name :reqs :summary :version))))

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
      '("elnode"
        ((web (0 1 4))
         (db (0 0 1))
         (kv (0 0 9))
         (fakir (0 0 14))
         (creole (0 8 14)))
        "The Emacs webserver."
        "0.9.9.1")
      ;; We do take off the README
      (elpakit/package-info
       (elpakit/recipe->pkg-info recipe)
       :name :reqs :summary :version)))))

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
