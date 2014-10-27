;;; elnode-js-tests.el --- tests for elnode-js -*- lexical-binding: t -*-

(require 'fakir)
(require 'elnode)

(ert-deftest elnode-js/browserify-bin ()
  (should
   (equal
    (let ((default-directory (expand-file-name "node-test-dir/src")))
      (elnode-js/browserify-bin "."))
    (expand-file-name "node-test-dir/node_modules/.bin/browserify"))))

(with-elnode-mock-httpcon :httpcon
    (:elnode-http-method "GET")
  (elnode-js/browserify-send-func
   :httpcon
   (expand-file-name "src/example.js" "node-test-dir"))
  (with-current-buffer (fakir-get-output-buffer)
    (buffer-string)))

(provide 'elnode-js-tests)

;;; elnode-js-tests.el ends here
