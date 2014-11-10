;;; elpakit.el --- package archive builder

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Maintainer: Nic Ferrier <nferrier@ferrier.me.uk>
;; URL: http://github.com/nicferrier/elpakit
;; Keywords: lisp

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

;; This is for building file based package archives.  This is useful
;; for deploying small sets of under-development-packages in servers;
;; for example elnode packages.

;; An example:

;;  (elpakit
;;   "/tmp/shoesoffsaas"
;;   '("~/work/elnode-auth"
;;     "~/work/emacs-db"
;;     "~/work/shoes-off"
;;     "~/work/rcirc-ssh"))

;; This should make a package archive in /tmp/shoesoffsaas which has
;; the 4 packages in it.

;; Package building

;; elpakit can also be used to build multi-package elpa packages, go
;; to the directory containing the package source files and then:

;;   M-x elpakit-make-multi

;; will try and make a multi-file tar package in the current
;; directory.

;; elpakit will make a best effort with multi-file packages but it
;; won't always get it right without direction.  The right direction
;; can be supplied in the form of a package recipe.  Elpakit itself is
;; a package that can only be built with a recipe.

;; You can upload a multi-file package to marmalade or install it
;; manually with:

;;   M-x package-install-file

;; elpakit can also be used to run tests on packages and do other
;; things with packages.  See the official README file for more
;; information.


;;; Code:

(require 'package)
(require 'dash)
(require 'cl)
(require 's)
(require 'shadchen)
(require 'tabulated-list)
(require 'server)

(defgroup elpakit nil
  "The ELPA packaging utility."
  :group 'maintenance)

(defcustom elpakit-do-melpa-on-multi-file-package nil
  "Shall we make a MELPA branch for packages?

When a multi-file package is made we can make a `melpa' branch in
the source tree."
  :group 'elpakit
  :type 'boolean)

(defun elpakit/file-in-dir-p (filename dir)
  "Is FILENAME in DIR?"
  (let ((file (concat
               (file-name-as-directory dir)
               filename)))
    (and
     (file-in-directory-p file dir)
     (file-exists-p file))))

(defun elpakit/find-recipe (package-dir)
  "Find the recipe in PACKAGE-DIR."
  (car (directory-files
        (concat
         (file-name-as-directory package-dir)
         "recipes") t "^[^#.]")))

(defun elpakit/absolutize-file-name (package-dir file-name)
  (expand-file-name
   (concat (file-name-as-directory package-dir) file-name)))


(defmacro with-elpakit-new-package-api (consequent &rest alternate)
  "Abstract the package API."
  (declare (debug (form &rest form))
           (indent 0))
  `(if (version<=
        "24.4"
        (package-version-join
         (list emacs-major-version emacs-minor-version)))
       ,consequent
       ;; Else
       ,@alternate))

(defun elpakit/package-info (package-info &rest selector)
  (with-elpakit-new-package-api
    (if selector
        (--map
         (case it
           (:version (package-version-join (package-desc-version package-info)))
           (:name (let ((name (package-desc-name package-info)))
                    (if (symbolp name) (symbol-name name) name)))
           (:summary (package-desc-summary package-info))
           (:reqs (package-desc-reqs package-info))
           (t package-info))
         selector)
        ;; Else we just want the package
        package-info)
        (if selector
            (--map
             (case it
               (:version (elt package-info 3))
               (:summary (elt package-info 2))
               (:reqs (elt package-info 1))
               (:name (elt package-info 0))
               (t package-info))
             selector)
            ;; Else
            package-info)))

(defun elpakit/infer-files-old (package-dir)
  "Infer the important files from PACKAGE-DIR.

Returns a plist with `:elisp-files', `:test-files' and
`:non-test-elisp' filename lists.

`:non-test-elisp' is absolutized, but the others are not."
  (let* ((elisp-files (directory-files package-dir nil "^[^.#].*\\.el$"))
         (test-files (elpakit/mematch
                      "\\(test.*\\|.*test[s]*\\)\\.el$"
                      elisp-files))
         (non-test-elisp
          (mapcar
           (lambda (f)
             (elpakit/absolutize-file-name package-dir f))
           (-difference elisp-files test-files))))
    (list
     :elisp-files elisp-files
     :test-files test-files
     :non-test-elisp non-test-elisp)))

(defun elpakit/git-files (package-dir)
  (let ((default-directory
         (expand-file-name
          (file-name-as-directory package-dir))))
    (split-string (shell-command-to-string "git ls-files"))))

(defun elpakit/infer-files (package-dir)
  "Infer the important files from PACKAGE-DIR.

Returns a plist with `:elisp-files', `:test-files',
`:other-files' and `:non-test-elisp' filename lists.

`:non-test-elisp' is absolutized, but the others are not."
  (let* (non-test-elisp
         elisp-files
         test-files
         other-files)
    (--each (or (elpakit/git-files package-dir)
                (directory-files package-dir nil "^[^.]+.*[^~#]$"))
      (cond
        ((string-match-p "\\(test\\(s\\)*.el\\)\\|\\(.*-test\\(s\\)*.el\\)$" it)
         (push it elisp-files)
         (push it test-files))
        ((equal (file-name-extension it) "el")
         (push it elisp-files)
         (push it non-test-elisp))
        ((and (not (string-match-p "^\\..*" it)))
         (push it other-files))))
    (list :elisp-files elisp-files
          :test-files test-files
          :non-test-elisp non-test-elisp
          :other-files other-files)))

(defun elpakit/infer-recipe (package-dir)
  "Infer a recipe from the PACKAGE-DIR.

We currently can't make guesses about multi-file packages so
unless we can work out if the package is a single file package
this function just errors.

Test package files are guessed at to produce the :test section of
the recipe.

Files mentioned in the recipe are all absolute file names."
  (match-let
      (((plist :elisp-files elisp-files
               :test-files test-files
               :non-test-elisp non-test-elisp
               :other-files other-files)
        (elpakit/infer-files package-dir)))
    ;; Now choose what sort of recipe to build?
    (if (equal (length non-test-elisp) 1)
        (let ((name (car (elpakit/file->package (car non-test-elisp) :name))))
          ;; Return the recipe with optional test section
          (list* ; FIXME replace this with dash's -cons*
           name
           :files non-test-elisp
           (when test-files
             ;;  (--map (elpakit/absolutize-file-name package-dir it) test-files)
             (list :test (list :files test-files)))))
        ;; Else we have a multi-file package... try and find the
        ;; package-info in an elisp-file
        (let ((info
               (car-safe
                (--keep
                 (condition-case err
                     (with-temp-buffer
                       (insert-file-contents (expand-file-name it package-dir))
                       (emacs-lisp-mode)
                       (package-buffer-info))
                   (error nil))
                 elisp-files))))
          (if (not info)
              (error
               "elpakit - cannot infer package for %s - add a recipe description"
               package-dir)
              ;; Else try and construct the package
              ;;(list (car info))
              (list*
               (symbol-name (package-desc-name info))
               :version (package-version-join (package-desc-version info))
               :doc (package-desc-summary info)
               :files
               (append
                non-test-elisp
                other-files) ; (--map (elpakit/absolutize-file-name package-dir it) other-files)
               (when test-files
                 ;; (let ((full (--map (elpakit/absolutize-file-name package-dir it) test-files))))
                 (list :test (list :files test-files)))))))))

(defun elpakit/get-recipe (package-dir)
  "Return the recipe for the PACKAGE-DIR.

Ensure the file list is sorted and consisting of absolute file
names.  Adds a `:base-files' key containing the original,
un-absoluted files.

If the recipe is not found as a file then we infer it from the
PACKAGE-DIR with `elpakit/infer-recipe'."
  (let* ((recipe
          (if (elpakit/file-in-dir-p "recipes" package-dir)
              (let ((recipe-filename (elpakit/find-recipe package-dir)))
                (with-temp-buffer
                  (insert-file-contents recipe-filename)
                  (goto-char (point-min))
                  (read (current-buffer))))
              ;; Else infer the recipe
              (elpakit/infer-recipe package-dir))))
    (plist-put
     (cdr recipe)
     :base-files
     (plist-get (cdr recipe) :files))
    (plist-put
     (cdr recipe)
     :files
     (--map
      (if (file-name-absolute-p it)
          it
          (elpakit/absolutize-file-name package-dir it))
      (plist-get (cdr recipe) :files)))
    recipe))

(defun elpakit/package-files (recipe)
  "The list of files specified by the RECIPE.

The list is returned sorted and with absolute files."
  (plist-get (cdr recipe) :files))

(defun elpakit/package-base-files (recipe)
  "The list of base file names specified by the RECIPE."
  (plist-get (cdr recipe) :base-files))

(defun elpakit/mematch (pattern lst)
  "Find the PATTERN in the LST."
  (loop for elt in lst
     if (string-match pattern elt)
     collect elt))

(defun elpakit/make-pkg-lisp (dest-dir pkg-info)
  "Write the package declaration lisp for PKG-INFO into DEST-DIR."
  (match-let (((list name reqs summary version)
               (elpakit/package-info pkg-info :name :reqs :summary :version)))
    (let* ((filename (concat
                      (file-name-as-directory dest-dir)
                      (format "%s-pkg.el" name)))
           (lisp
            `(define-package ; add the URL to extra-properties
                 ,name ,version ,summary
                 (quote ,(--map
                          (cons
                           (car it)
                           (list
                            (package-version-join (cadr it)))) reqs)))))
      (with-temp-buffer
        (insert (format "%S" lisp))
        (write-file filename)))))

(defun elpakit/copy (source dest)
  "Copy SOURCE file to DEST."
  (when (file-exists-p dest)
    (if (file-directory-p dest)
        (delete-directory dest t)
        (delete-file dest)))
  ;;(message "elpakit/copy %s to %s" source dest)
  (copy-file source dest))

(defun elpakit/file->package (file &rest selector)
  "Convert FILE to package-info.

Optionaly get a list of SELECTOR which is `:version', `:name',
`:requires' or t for everything."
  (with-current-buffer (find-file-noselect file)
    (let* ((package-info
            (save-excursion
              (package-buffer-info))))
      (apply 'elpakit/package-info package-info selector))))


(defun elpakit/build-single (destination single-file &optional recipe)
  "Build SINGLE-FILE package into DESTINATION.

The package is built as a directory of:

  package-name-version

with the package file inside.  The RECIPE is used if supplied."
  (destructuring-bind
        (package-info name version)
      (condition-case nil
          (elpakit/file->package single-file t :name :version)
        (error
         (when recipe
           (let* ((recipe-plist (cdr recipe))
                  (package-name (symbol-name (car recipe)))
                  (version (plist-get recipe-plist :version)))
             (list
              (vector package-name
                      (elpakit/require-versionify
                       (plist-get recipe-plist :requires)) ;; fixme - add base
                      (concat "A package derived from " package-name)
                      version
                      "") ; commentary
                    package-name
                    version)))))
    (let* ((filename
            (s-format "${dir}${name}-${version}/${name}.el"
                      'aget
                      `(("dir" . ,(file-name-as-directory destination))
                        ("name" . ,name)
                        ("version" . ,version))))
           (dest (file-name-directory filename)))
      (unless (file-exists-p dest)
        (make-directory dest t))
      ;; Copy the actual lisp file
      (elpakit/copy single-file filename)
      ;; Return the info
      package-info)))

(defun elpakit/recipe->package-decl (recipe)
  "Convert RECIPE into a package declaration."
  (match-let (((cons
                name
                (plist
                 :version version
                 :doc doc
                 :requires requires
                 :base-files base-files
                 :files files
                 :test test)) recipe))
    `(define-package
         ,(if (symbolp name) (symbol-name name) name)
         ,version
       ,(if (not (equal doc ""))
            doc
            (let ((matched (elpakit/mematch "README\\..*" files)))
              (when matched
                (with-current-buffer (find-file-noselect (car matched))
                  (buffer-substring-no-properties (point-min)(point-max))))
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
  "Make a proper version list from the REQUIRE-LIST."
  (mapcar
   (lambda (elt)
     (list
      (car elt)
      (version-to-list (cadr elt))))
   require-list))

(defun elpakit/strip-file-local-vars (header-line-string)
  "Strip any file local variables from HEADER-LINE-STRING."
  (replace-regexp-in-string "-\\*- .* -\\*-" "" header-line-string))

(defun elpakit/recipe->pkg-info (recipe)
  "Convert the RECIPE to a package info vector."
  (let* ((package-decl
          (package-read-from-string
           (format "%S" (elpakit/recipe->package-decl recipe)))))
    (with-elpakit-new-package-api
      (package-desc-create
       :name (intern (nth 1 package-decl))
       :version (version-to-list (nth 2 package-decl))
       :summary (elpakit/strip-file-local-vars (nth 3 package-decl))
       :reqs (elpakit/require-versionify (nth 4 package-decl)))
      ;; Else
      (let* ((name (nth 1 package-decl))
             (version (nth 2 package-decl))
             (docstring (elpakit/strip-file-local-vars (nth 3 package-decl)))
             (require-list (nth 4 package-decl))
             (requires (elpakit/require-versionify require-list))
             (readme (elpakit/readme recipe))
             (package-info
              (vector name requires docstring version readme)))
        package-info))))

(defun elpakit/pkg-info->versioned-name (pkg-info)
  "Make the versioned package name from the PKG-INFO vector.

Returns eg: `package-name-1.0.0'."
  (match-let
      (((list name requires docstring version)
        (elpakit/package-info pkg-info :name :reqs :summary :version)))
    (format "%s-%s" name version)))


(defun elpakit/file-name-base-mask (file-name mask)
  "Return the parts of FILE-NAME which follow a match for MASK.

Examples:

 (elpakit/file-name-mask \"/one/two/three\" \"/one/two\")  => \"three\"
 (elpakit/file-name-mask \"/one/two/three/four\" \"/one/two\") => \"three/four\"
 (elpakit/file-name-mask \"/one/two\" \"/\") => \"one/two\"
 (elpakit/file-name-mask \"/two/three/four/five\" \"/one\") => nil

In the final example there is no match with the MASK so we return
nil."
  (let* ((mask-parts (split-string mask "/" t))
         (file-name-parts (split-string file-name "/" t))
         (unmatched
          (loop for part in file-name-parts
             if mask-parts
             unless (equal part (pop mask-parts)) return nil end
             else
             collect part)))
    (when unmatched
      (mapconcat 'identity unmatched "/"))))


(defun elpakit/build-multi (destination recipe)
  "Build a multi-file package into DESTINATION.

RECIPE specifies the package in a plist s-expression form.

The files should be tar'd and the tar placed into DESTINATION."
  (let* ((files (elpakit/package-files recipe))
         (base-files (elpakit/package-base-files recipe))
         (pkg-info (elpakit/recipe->pkg-info recipe))
         (qname (elpakit/pkg-info->versioned-name pkg-info)))
    (let* ((destdir (concat
                     "/tmp/"
                     (file-name-as-directory qname))))
      ;; Now copy everything to the destination
      (when (file-exists-p destdir)
        (delete-directory destdir t))
      (make-directory destdir t)
      ;; Copy the actual package files
      (when base-files
        (loop for file in files
           do
             (let ((dest-file
                    (concat
                     destdir
                     (pop base-files))))
               (make-directory (file-name-directory dest-file) t)
               (elpakit/copy file dest-file))))
      ;; Write the package file
      (elpakit/make-pkg-lisp destdir pkg-info)
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
      pkg-info)))

(defun elpakit/melpaize (repo tarball)
  "Make a `melpa' branch in the REPO directory from TARBALL.

TARBALL is a package tarball, perhaps generated by
`elpakit-make-multi', which will becomes the MELPA branch by
unpacking."
  (with-current-buffer (get-buffer-create "*elpakit-melpa-branch*")
    (let* ((default-directory (file-name-as-directory repo))
           (branches
            (split-string
             (shell-command-to-string "git branch -a"))))
      (insert
       (if (member "melpa" branches)
           (shell-command-to-string "git checkout melpa")
           ;; Else create the orphan branch
           (shell-command-to-string
            "git checkout --orphan melpa")))
      (let ((package-contents
             (shell-command-to-string
              (format "tar xvf %s --strip-components=1 " tarball))))
        (insert package-contents)
        (let* ((package-files (split-string package-contents))
               (file-list (mapconcat
                           (lambda (f)
                             (file-name-nondirectory f))
                           package-files " ")))
          ;; Just check we have the files
          (when (file-exists-p (file-name-nondirectory (car package-files)))
            (shell-command-to-string
             (concat "git add " file-list))
            (shell-command-to-string
             (concat
              "git commit -m 'melpa branch created by elpakit' "
              file-list))))))))

;;;###autoload
(defun elpakit-make-multi (package-dir &optional destination-dir)
  "Make a multi-package out of PACKAGE-DIR.

If the directory you are in is a package then it is built,
otherwise this asks you for a package directory.

Optionally, make the package file in the specified
DESTINATION-DIR.

If called interactively, this will default to building the
current directory as a package, if it has a 'recipes' directory.
If no 'recipes' directory can be found it will prompt for the
source directory.

If the prefix argument is used interactively, then this will also
prompt for the destination directory.

If the customization value
`elpakit-do-melpa-on-multi-file-package' is t and the
PACKAGE-DIR is a git repository then an orphan branch called
\"melpa\" is created in the repository and populated with the
files from the package.  This includes the pkg.el file necessary
to a multi-file package, which elpakit builds automatically.

Opens the directory the package has been built in."
  (interactive
   (list
    (if (directory-files default-directory nil "^recipes$")
        default-directory
        (read-directory-name "Package-dir: " default-directory))
    (when current-prefix-arg
      (read-directory-name "Destination dir:" default-directory))))
  (let* ((dest-dir (or destination-dir default-directory))
         (dir-flag t)
         (directory (file-name-as-directory package-dir))
         (package-name
          (file-name-sans-extension
           (car (last (split-string directory "/" t)))))
         (dest
          (if (file-directory-p dest-dir)
              dest-dir
              (make-temp-file package-name dir-flag "elpakit")))
         (package-info
          (elpakit/build-multi dest (elpakit/get-recipe directory))))
    ;; Check whether we should do melpa branch management
    (when (and
           (file-directory-p (concat directory ".git"))
           elpakit-do-melpa-on-multi-file-package)
      (let ((tar (elpakit/pkg-info->versioned-name package-info)))
        (elpakit/melpaize
         directory
         (car (directory-files dest t (concat tar ".tar"))))))
    (find-file dest)))

(defun elpakit/do-eval (package-dir)
  "Just eval the elisp files in the package in PACKAGE-DIR."
  (assert (file-directory-p package-dir))
  (let* ((recipe (elpakit/get-recipe package-dir))
         (files (elpakit/package-files recipe))
         (elisp (elpakit/files-to-elisp files package-dir)))
    (loop for file in elisp
       do
         (with-current-buffer
             (find-file-noselect
              (expand-file-name file package-dir))
           (message "elpakit/do-eval %s - %s" package-dir file)
           (condition-case err
               (eval-buffer)
             (file-error
              (let ((lastcons (last elisp)))
                (setcdr elisp
                        (cons (car lastcons)
                              (append lastcons (list file)))))))))))

(defun elpakit/build-recipe (destination recipe)
  "Build the package with RECIPE into DESTINATION."
  (let* ((recipe-plist (cdr recipe))
         (files (plist-get recipe-plist :files)))
    (if (>= (length files) 2)
        ;; it MUST be a multi-file package
        (cons 'tar (elpakit/build-multi destination recipe))
        ;; Else it's a single file package
        (cons 'single (elpakit/build-single
                       destination (car files) recipe)))))

(defun elpakit/test-plist->recipe (package-dir)
  "Turn the :test plist from PACKAGE-DIR into a proper recipe."
  (destructuring-bind
        (&key elisp-files test-files non-test-elisp)
      (elpakit/infer-files package-dir)
    (let* ((base-recipe (elpakit/get-recipe package-dir))
           (base-plist (cdr base-recipe))
           (test-plist (plist-get base-plist :test))
           (test-files
            (mapcar
             (lambda (f)
               (if (file-name-absolute-p f) f
                   (elpakit/absolutize-file-name package-dir f)))
             (or
              (plist-get test-plist :files)
              test-files)))
           (test-name (intern
                       (file-name-sans-extension
                        (file-name-nondirectory (car test-files)))))
           (test-require (plist-get test-plist :requires))
           (test-version
            (or (plist-get base-plist :version)
                (car (elpakit/file->package
                      (car non-test-elisp) :version)))))
      (list test-name
            :files test-files
            :version test-version
            :requires
            (append
             (list (list (car base-recipe) test-version))
             test-require)))))

(defun elpakit/do (destination package-dir &optional do-tests)
  "Put the package in PACKAGE-DIR in DESTINATION.

The PACKAGE-DIR specifies the package.  It will either have a
recipe directory in it or we'll be able to infer a recipe from
it.

If DO-TESTS is `t' also attempt to build and copy the test
package for the PACKAGE-DIR.  The test package may be inferred or
it may be detailed in a supplied recipe file.

Returns a list of the packages constructed.  A package is a cons
of the type (`single' or `tar') and a vector, the information
necessary to build the archive-contents file.

If DO-TESTS is `nil' then the list returned always has just one
element which is the package cons.  If DO-TESTS is true though
the list MAY contain the package cons AND an additional
test-package cons."
  (assert (file-directory-p package-dir))
  (let* ((recipe (elpakit/get-recipe package-dir))
         (package (elpakit/build-recipe destination recipe))
         (recipe-plist (cdr recipe)))
    ;; Make the return list
    (cons package
          (when (and do-tests
                     (plist-get recipe-plist :test))
            (list
             (elpakit/build-recipe
              destination
              (elpakit/test-plist->recipe package-dir)))))))

;;;###autoload
(defun elpakit-eval (package-list)
  "Eval all the elisp files in PACKAGE-LIST."
  (interactive
   (list
    (let* ((current-kit
            (when (directory-files default-directory "^recipes$")
              (list default-directory)))
           (minibuffer-completing-symbol t)
           (form (read-from-minibuffer
                  "Package list: "
                  (format "%S" current-kit) read-expression-map t
                  'read-expression-history)))
      (if (symbolp form) (symbol-value form) form))))
  (loop for package in package-list
     do
       (message "elpakit-eval (%S) - %s" package-list package)
       (elpakit/do-eval package)))

(defun elpakit/packages-list->archive-list (packages-list)
  "Turn PACKAGES-LIST into an archive list."
  (loop for (type . package) in packages-list
     collect
       (cons
        (intern (elt package 0)) ; name
        (vector (version-to-list (elt package 3)) ; version list
                (elt package 1) ; requirements
                (elpakit/strip-file-local-vars (elt package 2)) ; docstring
                type))))

(defun elpakit/archive-list->elpa-list (archive-list)
  "Make a list of ELPA packages referred to by the archive.

This reduces the ARCHIVE-LIST down to just the dependent packages
that are specified in the archive list.

We use this function to resolve the missing depends from test
packages.."
  (->> (--map
        (-map
         (lambda (x) (cons (car x) (apply 'vector (cadr x))))
         (elt (cdr it) 1)) ; pull the depend spec
        archive-list)
    (-flatten)
    (--sort (string-lessp
             (symbol-name (car it))
             (symbol-name (car other))))
    (--map (car it))
    (-uniq)))

(defun elpakit/archive-fetch-all (archive-list download-dir)
  "Retrieve depends specified in ARCHIVE-LIST.

The depends are pulled from `package-archives' via
`package-download-transaction'.  Instead of unpacking them and
storing them in the `package-user-dir' this stores them in
DOWNLOAD-DIR.

A new copy of `archive-list' is returned with all the additional
packages added to it."
  (let* ((dest (file-name-as-directory download-dir))
         (elpakit-requests (mapcar 'car archive-list))
         (package-depends (elpakit/archive-list->elpa-list archive-list))
         (package-list (--filter (not (memq it elpakit-requests)) package-depends)))
    (noflet ((package-unpack-single (name version desc requires)
               (let* ((pkg (assq (if (symbolp name) name (intern name))
                                 package-archive-contents))
                      (pkg-name (car pkg))
                      (pkg-details (cdr pkg)))
                 ;; The package has the archive source on the end so we strip that off
                 (push (cons pkg-name (subseq pkg-details 0 -1)) archive-list))
               (let ((pkg (buffer-string)))
                 (with-temp-file
                     (format "%s%s-%s.el" dest name version)
                   (insert pkg))))
             (package-unpack (name version)
               (push
                (assq (if (symbolp name) name (intern name))
                      package-archive-contents)
                archive-list)
               (let ((n (symbol-name name))
                     (pkg (buffer-string)))
                 (with-temp-file
                     (format "%s%s-%s.tar" dest n version)
                   (insert pkg)))))
      ;; FIXME - do caching here?
      (package-download-transaction package-list)
      archive-list)))

(defvar elpakit-make-full-archive t
  "Set to t to get the archive resolution stuff working.")

;;;###autoload
(defun elpakit (destination package-list &optional do-tests)
  "Make a package archive at DESTINATION from PACKAGE-LIST.

PACKAGE-LIST is either a list of local directories to package or
a list of two items beginning with the symbol `:archive' and
specifying an existing directory archive in `packages-archives'.

If PACKAGE-LIST is not an `:archive' reference then the package
directories specified are turned into packages in the
DESTINATION.

If PACKAGE-LIST is an `:archive' reference then the specified
archive is copied to DESTINATION.

For example, if `package-archives' is:

 '((\"local\" . \"/tmp/my-elpakit\")(\"gnu\" . \"http://gnu.org/elpa/\"))

and `elpakit' is called like this:

 (elpakit \"/tmp/new-elpakit\" (list :archive \"local\"))

then /tmp/my-elpakit will be copied to /tmp/new-elpakit."
  (if (eq :archive (car package-list))
      (copy-directory
       (aget package-archives (cadr package-list))
       destination
       nil t t)
      ;; Else do the package build
      (let* ((packages-list
              (loop for package in package-list
                 append
                   (elpakit/do destination package do-tests)))
             (archive-list (elpakit/packages-list->archive-list packages-list))
             (archive-dir (file-name-as-directory destination)))
        (unless (file-exists-p archive-dir)
          (make-directory archive-dir t))
        (when elpakit-make-full-archive
          (setq archive-list
                (elpakit/archive-fetch-all archive-list archive-dir)))
        (with-current-buffer
            (find-file-noselect
             (concat archive-dir "archive-contents"))
          (erase-buffer)
          (insert (format "%s" (pp-to-string (cons 1 archive-list))))
          (save-buffer)))))

(defconst elpakit/processes (make-hash-table :test 'equal)
  "List of elpakit processes, either emphemeral or daemons.

Each process should be a list where the first item is the name,
the second item is the process type either `:daemon' or
`:batch'.  Other items may be present.")

(defun elpakit/process-add (name type process &rest rest)
  "Add PROCESS to the central list keyed by NAME."
  (puthash name (append (list name type process) rest) elpakit/processes))

(defun elpakit/process-list ()
  "List all the processes."
  (let (res)
    (maphash
     (lambda (k v)
       (destructuring-bind (proc-name proc-type &rest rest) v
         (when (or
                (eq proc-type :batch)
                (and (eq proc-type :daemon)
                     (server-running-p k)))
           (setq res (append (list v) res)))))
     elpakit/processes)
    res))

(defun elpakit/process-del (name)
  "Remove the process with NAME from the central list."
  (remhash name elpakit/processes))

(defun elpakit/process-list-entries ()
  "List of processes in `tabulated-list-mode' format."
  (loop for (proc-name proc-type &rest rest) in (elpakit/process-list)
     collect
       (list proc-name
             (vector proc-name
                     (case proc-type
                       (:daemon "daemon")
                       (:batch "batch"))
                     (if rest (format "%S" rest) "")))))

(defun elpakit/eval-at-server (name form)
  "Evaluate FORM at server NAME for side effects only."
  (if (functionp 'server-eval-at)
      (server-eval-at name form)
      (with-temp-buffer
        (print form (current-buffer))
        (call-process-shell-command
         (format "emacsclient -s %s --eval '%s'" name (buffer-string)) nil 0))))

(defun elpakit-process-kill (process-id &optional interactive-y)
  "Kill the specified process with PROCESS-ID."
  (interactive
   (list
    (buffer-substring-no-properties
     (line-beginning-position)
     (save-excursion
       (goto-char (line-beginning-position))
       (- (re-search-forward " " nil t) 1))) t))
  (destructuring-bind (name type process &rest other)
      (gethash process-id elpakit/processes)
    ;; Check that we're interactive and the user really wants it
    ;; ... or we're not interactive.
    (when (or
           (and
            interactive-y
            (y-or-n-p (format "elpakit: delete process %s?" name)))
           (not interactive-y))
      ;; First kill the buffer
      (let ((buf (process-buffer process)))
        (when (bufferp buf)
          (kill-buffer buf)))
      (when (process-live-p process)
        (delete-process process))
      (when (eq type :daemon)
        (condition-case err
            (elpakit/eval-at-server
             name
             '(kill-emacs))
          ((file-error error)
           (if (not
                (string-match-p "^\\(No such server\\|Make client process\\)"
                                (cadr err)))
               ;; Rethrow if anything else
               (signal (car err) (cdr err))))))
      ;; Now safely delete the process from elpakit's list
      (elpakit/process-del name)
      ;; Update the list if necessary
      (when (equal (buffer-name (current-buffer))
                   "*elpakit-processes*")
        (tabulated-list-print)))))

(defun elpakit/process-list-process-id ()
  "Get the process-id from the process-list buffer."
  (let ((process-id (buffer-substring-no-properties
                     (line-beginning-position)
                     (save-excursion
                       (goto-char (line-beginning-position))
                       (- (re-search-forward " " nil t) 1)))))
    (let ((process (gethash process-id elpakit/processes)))
       (destructuring-bind (proc-name proc-type &rest rest) process
         (if (or
              (eq proc-type :batch)
              (and (eq proc-type :daemon)
                   (server-running-p process-id)))
             process-id
             ;; Else...
             (elpakit/process-del process-id)
             (revert-buffer)
             (error "Server no longer running"))))))

(defun elpakit-process-open-emacsd (process-id)
  "Open the .emacs.d directory for the specified PROCESS-ID."
  (interactive (list (elpakit/process-list-process-id)))
  (destructuring-bind (name type process &rest other)
      (gethash process-id elpakit/processes)
    (find-file-other-window (format "/tmp/%s.emacsd" name))))

(defun elpakit-process-open-emacs-init (process-id)
  "Open the init file for the specified PROCESS-ID."
  (interactive (list (elpakit/process-list-process-id)))
  (destructuring-bind (name type process &rest other)
      (gethash process-id elpakit/processes)
    (find-file-other-window (format "/tmp/%s.emacs-init.el" name))))

(defun elpakit-process-show-buffer (process-id)
  "Show the buffer for the proc with PROCESS-ID."
  (interactive (list (elpakit/process-list-process-id)))
  (destructuring-bind (name type process &rest other)
      (gethash process-id elpakit/processes)
    (let ((buf (process-buffer process)))
      (if (or (not (bufferp buf))
              (not (buffer-live-p buf)))
          (message "elpakit process %s has no buffer?" process-id)
          ;; Else switch to the buffer
          (switch-to-buffer buf)))))

(defun elpakit-process-open-shell (process-id)
  (interactive (list (elpakit/process-list-process-id)))
  (destructuring-bind (name type process &rest other)
      (gethash process-id elpakit/processes)
    (let ((errm "elpakit-procss-open-shell"))
      (unless (eq type :daemon)
        (error "%s needs a daemon - %s" errm process-id))
      (unless (featurep 'isea)
        (error "%s needs isea - M-x package-install isea ?" errm))
      (funcall 'isea process-id))))

(define-derived-mode
    elpakit-process-list-mode tabulated-list-mode "Elpakit process list"
    "Major mode for listing Elpakit processes."
    (setq tabulated-list-entries 'elpakit/process-list-entries)
    (setq tabulated-list-format
          [("Process Name" 30 nil)
           ("Batch or Daemon" 20 nil)
           ("Args" 30 nil)])
    (define-key
        elpakit-process-list-mode-map (kbd "K")
      'elpakit-process-kill)
    (define-key
        elpakit-process-list-mode-map (kbd "L")
      'elpakit-process-show-buffer)
    (define-key
        elpakit-process-list-mode-map (kbd "F")
      'elpakit-process-open-emacsd)
    (define-key
        elpakit-process-list-mode-map (kbd "f")
      'elpakit-process-open-emacs-init)
    (define-key
        elpakit-process-list-mode-map (kbd "I")
      'elpakit-process-open-shell)
    (tabulated-list-init-header))

;;;###autoload
(defun elpakit-list-processes ()
  "List running elpakit processes.

Uses `elpakit-process-list-mode' to display the currently running
elpakit processes from batch tests and daemons."
  (interactive)
  (with-current-buffer (get-buffer-create "*elpakit-processes*")
    (elpakit-process-list-mode)
    (tabulated-list-print)
    (switch-to-buffer (current-buffer))))

(defalias 'list-elpakit-processes 'elpakit-list-processes)


;; Process spawning

(defun elpakit/sentinel (process event)
  "Sentintel to kill the elpakit PROCESS when necessary."
  (if (or (string-equal event "finished\n")
          (string-equal event "exited abnormally with code exitcode\n"))
      (elpakit/process-del (process-name process))))

(defun* elpakit/emacs-process (archive-dir
                               install test
                               &key extra-lisp pre-lisp)
  "Start an Emacs test process with the ARCHIVE-DIR repository.

Install the package INSTALL and then run batch tests on TEST.

The current Emacs process' `package-archives' variable is used
with the ARCHIVE-DIR being an additional repository called
\"local\".

The ARCHIVE-DIR was presumably built with elpakit, though it
could have come from anywhere."
  (let* ((unique (make-temp-name "elpakit-emacs-batchtests"))
         (elpa-dir (concat "/tmp/" unique ".emacsd"))
         (emacs-bin (concat invocation-directory invocation-name))
         (boot-file (concat "/tmp/" unique ".emacs-init.el"))
         (args
          (progn
            (with-temp-file boot-file
              (insert (format
                       (concat
                        "(progn"
                        "%s"
                        "(setq package-archives (quote %S))"
                        "(setq package-user-dir %S)"
                        "(package-initialize)"
                        "(package-refresh-contents)"
                        "(package-install (quote %S))"
                        "(load-library \"%S\")"
                        "%s"
                        "(ert-run-tests-batch \"%s.*\"))")
                       (if pre-lisp (format "%S" pre-lisp) "")
                       (acons "local" archive-dir package-archives)
                       elpa-dir ;; where packages will be installed to
                       install
                       install
                       (if extra-lisp
                           (mapconcat
                            (lambda (a) (format "%S" a))
                            extra-lisp "\n")
                           "")
                       test)))
            ;; And now the actual args
            (list "--batch" "-Q" "-l" boot-file)))
         (name (generate-new-buffer-name
                (concat "elpakit-" (symbol-name install))))
         (proc (apply 'start-process name (format "*%s*" name) emacs-bin args)))
    (elpakit/process-add name :batch proc boot-file)
    (set-process-sentinel proc 'elpakit/sentinel)
    (with-current-buffer (process-buffer proc)
      (compilation-mode))
    proc))

(defun elpakit/emacs-server (archive-dir install &optional extra-lisp pre-lisp)
  "Start an Emacs server process with the ARCHIVE-DIR repository.

The server started is a daemon.  The process that starts the
daemon should die once the initialization has finished.  The
daemon remains of course.

INSTALL is a package name symbol for a package from the
ARCHIVE-DIR that will be installed.

Optionally you can specify a quoted lisp form in EXTRA-LISP and
this will be included in the daemon initialization.  This is
useful for testing, for example running batch tests could be done
like:

 '(ert-run-tests-batch \"my-package.*\")

This function returns a cons of the initialization process and the
emacs server identifier.  The emacs server identifier can be used
to contact the emacs daemon directly, thus:

  emacsclient -s /tmp/emacs$UID/$emacs-server-identifier

presuming that you're trying to start it from the same user."
  (let* ((unique (make-temp-name "elpakit-emacs-server"))
         (emacs-dir (concat "/tmp/" unique ".emacsd/"))
         (elpa-dir (concat emacs-dir "elpa/"))
         (emacs-bin (concat invocation-directory invocation-name))
         (boot-file (concat "/tmp/" unique ".emacs-init.el"))
         (args
          (list (concat "--daemon=" unique)
                "-Q" "-l" boot-file)))
    (with-temp-file boot-file
      (insert (format
               (concat
                "(progn"
                "%s"
                "(setq package-archives (quote %S))"
                "(setq package-user-dir %S)"
                "(package-initialize)"
                "(package-refresh-contents)"
                "(package-install (quote %S))"
                "%s)")
               (if pre-lisp (format "%S" pre-lisp) "")
               (acons "local" archive-dir package-archives)
               elpa-dir ;; where packages will be installed to
               install
               (if extra-lisp
                   (mapconcat
                    (lambda (a) (format "%S" a))
                    extra-lisp "\n")
                   ""))))
    (let ((proc (apply 'start-process
                       unique (format "*%s*" unique)
                       emacs-bin args)))
      (with-current-buffer (process-buffer proc)
        (compilation-mode))
      (elpakit/process-add
       unique :daemon proc ; then the extra stuff
       install :pre-lisp pre-lisp :extra-lisp extra-lisp)
      (cons proc unique))))


(defvar elpakit-unique-handle nil)
(make-variable-buffer-local 'elpakit-unique-handle)

;;;###autoload
(defun* elpakit-start-server (package-list
                              install
                              &key test pre-lisp extra-lisp)
  "Start a server with the PACKAGE-LIST.

If TEST is `t' then we run tests in the daemon.

If EXTRA-LISP is a list then that is passed into the daemon to be
executed as extra initialization.  If EXTRA-LISP is specified
then automatic requiring of the INSTALL is not performed.  If
EXTRA-LISP and TEST is specified then tests are done *after*
EXTRA-LISP.  EXTRA-LISP must do the require in that case.

If PRE-LISP is a list then it is passed into the daemon as Lisp
to be executed before initialization.  This is where any
customization that you need should go.

You can manage running servers with the `elpakit-list-processes'
command."
  (let ((archive-dir (make-temp-file "elpakit-archive" t)))
    ;; First build the elpakit with tests
    (elpakit archive-dir package-list t)
    (let* ((test-stuff
            (if test
                `((ert-run-tests-batch ,(format "%s.*" install)))))
           (extra
            (if extra-lisp
                (cons extra-lisp test-stuff)
                ;; else do the require automatically
                (cons
                 `(require (quote ,install))
                 test-stuff)))
           (daemon-value
            (elpakit/emacs-server archive-dir install extra pre-lisp)))
      (with-current-buffer (get-buffer-create "*elpakit-daemon*")
        (setq elpakit-unique-handle (cdr daemon-value))
        daemon-value))))

(defvar elpakit/test-ert-selector-history nil
  "History variable for `elpakit-test'.")

;;;###autoload
(defun* elpakit-test (package-list install test &key pre-lisp extra-lisp)
  "Run tests on package INSTALL of the specified PACKAGE-LIST.

TEST is an ERT test selector.

If EXTRA-LISP is a list then that is passed into the test-process
to be executed as extra initialization.  If EXTRA-LISP and TEST
is specified then tests are done *after* EXTRA-LISP.  EXTRA-LISP
must do the require in that case.

If PRE-LISP is a list then it is passed into the test-process as
Lisp to be executed before initialization.  This is where any
customization that you need should go.

You can manage running processes with the `elpakit-list-processes'
command."
  (interactive
   (let ((test-recipe (elpakit/test-plist->recipe default-directory)))
     (assert (listp test-recipe) "there's no package in the current dir?")
     ;; deliver the interctive args
     (list (list default-directory)
           (car test-recipe)
           (read-from-minibuffer
            "ERT selector: " nil nil nil elpakit/test-ert-selector-history))))
  (let ((archive-dir (make-temp-file "elpakit-archive" t))
        process)
    ;; First build the elpakit with tests
    (elpakit archive-dir package-list t)
    (setq process (elpakit/emacs-process
                   archive-dir install test
                   :pre-lisp pre-lisp :extra-lisp extra-lisp))
    (when (called-interactively-p 'interactive)
      (switch-to-buffer-other-window (process-buffer process)))))

(defun elpakit/new-file-name-predicate (file-name)
  "Check FILE-NAME is a new elisp file in an existing directory."
  (equal "el" (file-name-extension file-name)))

(defun elpakit-make-elpa-package (file-name)
  "Make FILE-NAME ELPA package requiring the specified packages.

The FILE-NAME must have an existing directory and a be a new
file.

This is useful for making a package with a set of depends for
packages you have so you can distribute it amongst a team or
colleagues who want to share some or all of the same packages as
you."
  (interactive
   (list
    (read-file-name
     "New Emacs-Lisp file name: " nil nil nil nil
     'elpakit/new-file-name-predicate)))
  ;; Check we have a new file
  (assert (elpakit/new-file-name-predicate file-name))
  ;; First split the lines from the existing buffer, this is the
  ;; required package list.
  (let* ((lines (split-string (buffer-string) "\n"))
         (elpa-requires-list
          (loop for line in lines
             if (get-text-property 0 :name line)
             collect
               (list
                (make-symbol (get-text-property 0 :name line))
                (get-text-property 0 :version line))))
         (elpa-requires
          (format ";; Package-Requires: %S\n" elpa-requires-list))
        (new-package-buf (find-file file-name)))
    (switch-to-buffer new-package-buf)
    (save-excursion
      (goto-char (point-min))
      (let ((code-start
             (save-excursion
               (re-search-forward "^\\(;;; Code:\\|(\\)" nil t))))
        (if (re-search-forward "^;; Package-Requires: \\(.*\\)$" code-start t)
            (replace-match (format "%S" elpa-requires-list) nil nil nil 1)
            ;; Else try and find the place to put the header
            (if (re-search-forward "^;; Author: " nil t)
                (progn
                  (forward-line)
                  (insert elpa-requires))
                ;; Else raise an error
                (progn
                  (kill-new elpa-requires)
                  (signal
                   'file-error
                   (list
                    "no package header? insert requires manually with `yank'")))))))))

(defun elpakit/make-package-list ()
  "Make a list of your currently installed packages."
  (let ((package-regex "\\([a-zA-Z-]+\\)-\\([0-9.]+\\)"))
    (loop for entry-lst
       in (-group-by
           (lambda (a) (get-text-property 0 :name a))
           (mapcar
            (lambda (e)
              (string-match package-regex e)
              (propertize e
                          :name (match-string 1 e)
                          :version (match-string 2 e)))
            (directory-files package-user-dir nil package-regex)))
       collect
         (sort
          (cdr entry-lst)
          (lambda (a b)
            (let ((va
                   (progn
                     (string-match package-regex a)
                     (match-string 2 a)))
                  (vb
                   (progn
                     (string-match package-regex b)
                     (match-string 2 b))))
              (version< vb va)))))))

(defun elpakit-elpa-list-kill ()
  "Kill an item from the ELPA list."
  (interactive)
  (let (buffer-read-only)
    (save-excursion
      (delete-region (line-beginning-position) (+ 1 (line-end-position))))))

(define-derived-mode elpakit-elpa-list-mode fundamental-mode
  "Elpakit elpa package list"
  "Major mode for listing currently installed ELPA packages."
  (setq buffer-read-only t)
  (define-key elpakit-elpa-list-mode-map (kbd "k")
    'elpakit-elpa-list-kill)
  (define-key elpakit-elpa-list-mode-map (kbd "q")
    'kill-buffer)
  (define-key elpakit-elpa-list-mode-map (kbd "M")
    'elpakit-make-elpa-package))

;;;###autoload
(defun elpakit-package-list-buf ()
  "Make a buffer with the package list in it."
  (interactive)
  (let ((package-list (elpakit/make-package-list)))
    (with-current-buffer (get-buffer-create "*elpakit-elpa-list*")
      (let (buffer-read-only)
        (erase-buffer)
        (save-excursion
          (loop for package-entry in package-list
             do (progn
                  (insert (car package-entry))
                  (newline)))))
      (elpakit-elpa-list-mode)
      (switch-to-buffer (current-buffer)))))


;;; Other tools on top of elpakit

(defun elpakit/files-to-elisp (list-of-files package-dir)
  "Return just the package elisp files from LIST-OF-FILES.

The package elisp files are those that are in the root of the
PACKAGE-DIR."
  (let* ((dir (file-name-as-directory (expand-file-name package-dir)))
         (re (rx-to-string
              `(and ,dir (1+ (not (any "/"))) ".el"))))
    (-filter (lambda (e) (string-match-p re e)) list-of-files)))

(defun elpakit-multi-occur (buffer thing)
  "Multi-occur the current symbol using the current Elpakit.

All lisp files in the current elpakit are considered.

`elpakit-isearch-hook-jack-in' can be used to connect this up to
`isearch'.  Alternately or additionally bind it to another key in
`emacs-lisp-mode'."
  (interactive
   (list
    (current-buffer)
    (thing-at-point 'symbol)))
  (let* ((files (elpakit/package-files (elpakit/get-recipe ".")))
         (elisp (elpakit/files-to-elisp files "."))
         (elisp-buffers
          (loop for filename in elisp
             collect (find-file-noselect filename))))
    (multi-occur elisp-buffers thing)))

(defvar elpakit/isearch-keymap nil
  "The keymap containing extra things we enable during isearch.")

(defun elpakit/isearch-hook-jack-in ()
  "To be called by the isearch hook to connect our keymap."
  (unless elpakit/isearch-keymap
    (setq elpakit/isearch-keymap (make-sparse-keymap))
    (set-keymap-parent elpakit/isearch-keymap isearch-mode-map)
    (define-key elpakit/isearch-keymap (kbd "M-o")
      'elpakit-multi-occur)
  (setq overriding-terminal-local-map elpakit/isearch-keymap)))

;;;###autoload
(defun elpakit-isearch-hook-jack-in ()
  "Jack in Elpakit to isearch.  Call from `elisp-mode-hook'.

Adds `elpakit-multi-occur' to `isearch' with `M-o'.

Use something like:

 (add-hook 'emacs-lisp-mode-hook 'elpakit-isearch-hook-jack-in)

in your configuration file to make it happen."
  (add-hook 'isearch-mode-hook 'elpakit/isearch-hook-jack-in t t))

(provide 'elpakit)

;;; elpakit.el ends here
