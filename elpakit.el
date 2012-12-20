;;; elpakit.el --- package archive builder

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Maintainer: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: lisp
;; Package-Requires: ((anaphora "0.0.6")(dash "1.0.3"))
;; Version: 0.0.6

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A package archive builder.

;; This is for building file based package archives. This is useful
;; for deploying small sets of under-development-packages in servers;
;; for example elnode packages.

;; An example:

;; (elpakit
;;  "/tmp/shoesoffsaas"
;;  '("~/work/elnode-auth"
;;    "~/work/emacs-db"
;;    "~/work/shoes-off"
;;    "~/work/rcirc-ssh"))

;; This should make a package archive in /tmp/shoesoffsaas which has
;; the 4 packages in it.

;;; Code:

(require 'package)
(require 'dash)
(require 'cl)
(require 'anaphora)

(defun elpakit/file-in-dir-p (filename dir)
  "Is FILENAME in DIR?"
  (file-in-directory-p
   (concat
    (file-name-as-directory dir)
    filename)
   dir))

(defun elpakit/find-recipe (package-dir)
  "Find the recipe in PACKAGE-DIR."
  (car (directory-files
        (concat
         (file-name-as-directory package-dir)
         "recipes") t "^[^#.]")))

(defun elpakit/get-recipe (package-dir)
  "Returns the recipe for the PACKAGE-DIR.

Ensure the file list is sorted."
  (let* ((recipe-filename (elpakit/find-recipe package-dir))
         (recipe (with-current-buffer (find-file-noselect recipe-filename)
                   (goto-char (point-min))
                   (read (current-buffer)))))
    (plist-put
     (cdr recipe)
     :files
     (mapcar
      (lambda (f)
        (expand-file-name
         (concat (file-name-as-directory package-dir) f)))
      ;;(sort (plist-get (cdr recipe) :files) 'string<)
      (plist-get (cdr recipe) :files)))
    recipe))

(defun elpakit/package-files (recipe)
  "The list of files specified by the RECIPE.

The list is returned sorted and with absolute files."
  (plist-get (cdr recipe) :files))

(defun elpakit/mematch (pattern lst)
  "Find the PATTERN in the LST."
  (loop for elt in lst
     if (string-match pattern elt)
     collect elt))

(defun elpakit/make-pkg-lisp (dest-dir pkg-info)
  "Write the package declaration lisp for PKG-INFO into DEST-DIR."
  ;; This is ripped out of melpa pb/write-pkg-file
  (let* ((name (aref pkg-info 0))
         (filename (concat
                    (file-name-as-directory dest-dir)
                    (format "%s-pkg.el" name)))
         (lisp
          `(define-package
               ,name
               ,(aref pkg-info 3)
             ,(aref pkg-info 2)
             ',(mapcar
                (lambda (elt)
                  (list (car elt)
                        (package-version-join (cadr elt))))
                (aref pkg-info 1)))))
    (with-current-buffer
        (let (find-file-hook)
          (find-file-noselect filename))
      (erase-buffer)
      (insert (format "%S" lisp))
      (write-file filename))))

(defun elpakit/autoloads (name destdir)
  "Do all the necessary hacking to make autoloads save."
  (let (find-file-hook
        (delete-old-versions t)
        (ffn (symbol-function 'find-file-noselect)))
    (flet ((yes-or-no-p (prompt) t)
           (y-or-n-p (prompt) t)
           (ask-user-about-supersession-threat (fn) t)
           (find-file-noselect (filename)
             (awhen (get-buffer (file-name-nondirectory filename))
               (kill-buffer it))
             (funcall ffn filename t)))
      (package-generate-autoloads name destdir))))

(defun elpakit/copy (source dest)
  "Copy the file."
  (when (file-exists-p dest)
    (if (file-directory-p dest)
        (delete-directory dest t)
        (delete-file dest)))
  (message "elpakit/copy %s to %s" source dest)
  (copy-file source dest))

(defun elpakit/build-single (destination single-file &optional readme)
  "Build a single file package into DESTINATION."
  (with-current-buffer (find-file-noselect single-file)
    (let* ((package-info (save-excursion
                           (package-buffer-info)))
           (name (elt package-info 0))
           (version (elt package-info 3))
           (qname (format "%s-%s" name version)))
      (unless (file-exists-p destination)
        (make-directory destination t))
      ;; Copy the actual lisp file
      (elpakit/copy
       single-file
       (concat
        (file-name-as-directory destination) qname ".el"))
      ;; Write the package file - we don't need this?
      ;;; (elpakit/make-pkg-lisp destination package-info)
      ;; And now the autoloads - stopped doing this coz it doesn't work
      ;;; (elpakit/autoloads name destdir)
      ;; Return the info
      package-info)))

(defun elpakit/recipe->package-decl (recipe)
  "Convert a recipe into a package declaration."
  (destructuring-bind (name
                       ;; FIXME we need all the MELPA recipe things here
                       &key version doc requires files test) recipe
    `(define-package
         ,(symbol-name name)
         ,version
       ,(if (not (equal doc ""))
            doc
            (aif (elpakit/mematch "README\\..*" files)
                (with-current-buffer (find-file-noselect (car it))
                  (buffer-substring-no-properties (point-min)(point-max)))
              "This package has no documentation."))
       ,requires)))

(defun elpakit/readme (recipe)
  "Return the README for RECIPE or an empty string."
  (let ((readme
         (elpakit/mematch
          "README\\..*"
          (plist-get (cdr recipe) :files))))
    (if (and
         (listp readme)
         (> (length readme) 0))
        (with-current-buffer (find-file-noselect (car readme))
          (buffer-substring-no-properties (point-min)(point-max)))
        "")))

(defun elpakit/require-versionify (require-list)
  (mapcar
   (lambda (elt)
     (list
      (car elt)
      (version-to-list (cadr elt))))
   require-list))

(defun elpakit/build-multi (destination recipe)
  "Build a multi-file package into DESTINATION.

RECIPE specifies the package in a plist s-expression form."
  (let* ((package-info
          (package-read-from-string
           (format "%S" (elpakit/recipe->package-decl recipe))))
         (files (elpakit/package-files recipe))
         (name (nth 1 package-info))
         (version (nth 2 package-info))
         (docstring (nth 3 package-info))
         (require-list (nth 4 package-info))
         (requires (elpakit/require-versionify require-list))
         (readme (elpakit/readme recipe))
         (qname (format "%s-%s" name version))
         (destdir (concat
                   "/tmp/" ;;(file-name-as-directory destination)
                   (file-name-as-directory qname)))
         (package-info
          (vector name requires docstring version readme)))
    ;; Now copy everything to the destination
    (unless (file-exists-p destdir)
      (make-directory destdir t))
    ;; Copy the actual package files
    (loop for file in files
       do (let ((dest-file
                 (concat destdir (file-name-nondirectory file))))
            (elpakit/copy file dest-file)))
    ;; Write the package file
    (elpakit/make-pkg-lisp destdir package-info)
    ;; Check we have the package archive destination
    (unless (file-exists-p destination)
      (make-directory destination t))
    ;; Now we make the destination tarball
    (shell-command-to-string
     (format "tar cf /tmp/%s.tar -C /tmp %s" qname qname))
    ;; Now copy to the destination
    (elpakit/copy (format "/tmp/%s.tar" qname)
                  (concat (file-name-as-directory destination)
                          (format "%s.tar" qname)))
    ;; Return the info
    package-info))

(defun elpakit-make-multi (package-dir)
  "Make a multi-package.

If the directory you are in is a package then it is built,
otherwise this asks you for a package directory.

Opens the directory the package has been built in."
  (interactive
   (list
    (if (directory-files default-directory nil "^recipes$")
        default-directory
        (read-directory-name "Package-dir: " default-directory))))
  (let* ((dir-flag t)
         (package-name
          (file-name-sans-extension
           (file-name-nondirectory package-dir)))
         (dest (make-temp-file package-name dir-flag "elpakit")))
    (elpakit/build-multi dest (elpakit/get-recipe package-dir))
    (find-file dest)))

(defun elpakit/do-eval (package-dir)
  "Just eval the elisp files in the packag in PACKAGE-DIR."
  (assert (file-directory-p package-dir))
  (let ((package-name
         (file-name-sans-extension
          (file-name-nondirectory package-dir))))
    (if (elpakit/file-in-dir-p "recipes" package-dir)
        ;; Find the list of files from the recipe
        (let* ((recipe (elpakit/get-recipe package-dir))
               (files (elpakit/package-files recipe)))
          (loop for file in files
             if (equal (file-name-extension file) "el")
             do
               (with-current-buffer
                   (find-file-noselect
                    (expand-file-name file package-dir))
                 (eval-buffer))))
        ;; Else could we work out what the package file is?
        )))

(defun elpakit/with-recipe (destination package-dir)
  "Handle an elpakit directory that has a recipe."
  (let* ((recipe (elpakit/get-recipe package-dir))
         (files (elpakit/package-files recipe))
         (readme (elpakit/mematch "README\\..*" files))
         ;; FIXME allow single file packages to have a tests or a
         ;; package-name-tests.el file
         (l (length files)))
    (cond
      ((and
        (equal 2 l)
        readme)
       ;; Single file package with a README
       (cons 'single (elpakit/build-single destination package-dir)))
      ((or
        (>= l 2)
        (plist-get (cdr recipe) :version))
       ;; it MUST be a multi-file package
       (cons 'tar (elpakit/build-multi
                   destination
                   (elpakit/get-recipe package-dir))))
      (t
       ;; Single file package
       (cons 'single (elpakit/build-single destination (car files)))))))

(defun elpakit/package-elisp-files (package-dir)
  "Make some guesses about files in PACKAGE-DIR.

Return just the elisp files that aren't matching a test pattern."
  (let* ((package-name
          (file-name-sans-extension
           (file-name-nondirectory package-dir)))
         (files (directory-files package-dir t "^[.#].*"))
         (elisp-files (directory-files package-dir nil "^[^.#].*\\.el$")))
    (if (equal (length elisp-files) 1)
        elisp-files
        ;; Else it's more than one elisp file but is it one file
        ;; and a test? Try and remove test files
        (let ((diff (-difference
                     (-difference
                      elisp-files
                      (elpakit/mematch "test.*\\.el$" elisp-files))
                     (elpakit/mematch ".*test\\.el$" elisp-files))))
          diff))))

(defun elpakit/do (destination package-dir)
  "Put the package in PACKAGE-DIR in DESTINATION.

Build the package if necessary.

Return, a cons of the type (`single' or `tar') and a list, the
information necessary to build the archive-contents file."
  (assert (file-directory-p package-dir))
  (if (elpakit/file-in-dir-p "recipes" package-dir)
      ;; Find the list of files from the recipe
      (elpakit/with-recipe destination package-dir)
      ;; Else we don't have a recipe - can we infer one?
      (let* ((package-name
              (file-name-sans-extension
              (file-name-nondirectory package-dir)))
             (elisp-files (elpakit/package-elisp-files package-dir)))
        ;; If we have just 1 elisp file it's easy - it's a single package
        (if (equal (length elisp-files) 1)
            (elpakit/build-single destination (car elisp-files))
            ;; Else it's a multi-package and we can't do it yet
            ;;
            ;; FIXME - we could infer a lot about what to put in a
            ;; package:
            ;;
            ;; ** multiple, non-test lisp files could just be packaged
            ;; ** include any README and licence file
            ;; ** include any dir file and info files?
            (error
             "elpakit - cannot infer package for %s - add a recipe description"
             package-dir)))))

;;;###autoload
(defun elpakit-eval (package-list)
  "Eval all the elisp files in PACKAGE-LIST."
  (loop for package in package-list
     do (elpakit/do-eval package)))

;;;###autoload
(defun elpakit (destination package-list)
  "Make a package archive at DESTINATION from PACKAGE-LIST."
  (let* ((packages-list
          (loop for package in package-list
             collect
               (elpakit/do destination package)))
         (archive-list
          (loop for (type . package)  in packages-list
             collect
               (cons
                (intern (elt package 0)) ; name
                (vector (version-to-list (elt package 3)) ; version list
                        (elt package 1) ; requirements
                        (elt package 4) ; doc string
                        type))))
         (archive-dir (file-name-as-directory destination)))
    (unless (file-exists-p archive-dir)
      (make-directory archive-dir t))
    (with-current-buffer (find-file-noselect
                          (concat archive-dir "archive-contents"))
      (erase-buffer)
      (insert (format "%S" (cons 1 archive-list)))
      (save-buffer))))

(provide 'elpakit)


;;; elpakit.el ends here
