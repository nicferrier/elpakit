;;; example proxy stuff -*- lexical-binding: t -*-

(defun elnode-proxy-example (httpcon)
  "A function that will be proxied if it's on the wrong port."
  (elnode-send-json httpcon '(:data)))


;;; marmalade example

(elnode-start
 (lambda (httpcon)
   (elnode/router httpcon
    `(("^[^/]*//-/\\(.*\\)$" ,marmalade/webserver)
      ("^[^/]*//packages/new$" marmalade/upload-page)
      
      ("^[^/]*//packages/archive-contents$"
       marmalade-archive-contents-handler :service archive)
      ("^[^/]*//packages/archive-contents/\\([0-9]+\\)"
       ,marmalade-archive-cache-webserver :service archive)
      ("^[^/]*//packages/archive-contents/update$"
       marmalade-archive-update :service archive)
      
      ;; We don't really want to send 404's for these if we have them
      ("^[^/]+//packages/.*-readme.txt" elnode-send-404)
      ("^[^/]+//packages/\\(.*\\.\\(el\\|tar\\)\\)" marmalade/package-handler)
      ("^[^/]+//packages/\\([^/]+\\)" marmalade/package-blurb)
      ;; we have GET /packages/ and / be the same right now - probably not right
      ("^[^/]+//packages/$" marmalade/packages-index)
      
      ("^[^/]+//$" marmalade/packages-index))))
 :port 9000
 :service-mappings '((archive . 9001)))

;; Starts two servers, one on 9000 and one on 9001. 

;;; elnode-proxy-test.el ends here
