(asdf:defsystem "transforms"
   :serial t
   :version "1.0"
   :author "Dylan Ball <arathnim@gmail.com>"
   :maintainer "Dylan Ball <arathnim@gmail.com>"
   :description "small syntax extensions"
   :depends-on (alexandria anaphora iterate destructuring-match provide-toplevel)
   :components ((:file "transforms")))
