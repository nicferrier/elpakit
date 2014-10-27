;;; scratch working of various elpakits

(elpakit
 "/tmp/elpatest1"
 '("~/work/emacs-db"
   "~/work/emacs-db-pg"
   "~/work/pg"
   "~/work/emacs-kv"))

(elpakit-test
 '("~/work/emacs-db"
   "~/work/emacs-db-pg"
   "~/work/pg"
   "~/work/emacs-kv")
 'db-pg-tests
 'db)

(elpakit-test
 '("~/work/elnode-auth"
   "~/work/emacs-db"
   "~/work/emacs-kv")
 'elnode-tests
 'elnode)

(elpakit-test
 '("~/work/emacs-web")
 'web-test
 'web)

(elpakit-test
 '("~/work/mongo-el")
 'mongo-tests
 'bson)


;;; end of scratch.el
