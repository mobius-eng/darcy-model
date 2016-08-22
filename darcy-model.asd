(asdf:defsystem #:darcy-model
  :description "Darcy model simulator"
  :author "Alexey Cherkaev"
  :license "GPLv3"
  :depends-on (#:cl-numerics #:closer-mop #:alexandria)
  :components ((:module
                "src"
                :serial t
                :components ((:file "package")
                             (:file "utils")
                             (:file "conductivity")
                             (:file "unsaturated")
                             (:file "inlet-discharge")
                             (:file "darcy")))))

