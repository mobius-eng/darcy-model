(asdf:defsystem #:darcy
  :description "Darcy model simulator"
  :author "Alexey Cherkaev"
  :license "GPLv3"
  :depends-on (#:cl-numerics)
  :components ((:module
                "src"
                :components ((:file "package")
                             (:file "unsaturated")
                             (:file "conductivity")
                             (:file "darcy")
                             (:file "utils")))))

