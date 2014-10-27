;;; -*- lexical-binding: t -*-

(defconst table
  (routing-table ;; would this help??
   ("^[^/]+//test/1" handler1 :resource :test)
   ("^[^/]+//test/2" handler2)
   ("^[^/]+//test/3" handler3)
   ("^[^/]+//test/4" handler4 :resource :test2)))

(ert-deftest elnode-service-route ()
  (elnode-resolve httpcon table)
  )

;; the problem is that any functional transformation of this breaks being able to edit it

;;; elnode-service-tests.el ends here
