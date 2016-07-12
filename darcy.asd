(asdf:defsystem #:darcy
  :description "Darcy model simulator"
  :author "Alexey Cherkaev"
  :license "GPLv3"
  :depends-on (#:cl-numerics)
  :components ((:module
                "src"
                :serial t
                :components ((:file "package")
                             (:file "utils")
                             (:file "conductivity")
                             (:file "unsaturated")
                             (:file "darcy")))))

