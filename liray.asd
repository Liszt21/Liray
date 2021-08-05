(defsystem "liray"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("dexador" "cl-json")
  :components ((:module "src"
                :components
                ((:file "liray"))))
  :description ""
  :in-order-to ((test-op (test-op "liray/tests"))))

(defsystem "liray/tests"
  :author ""
  :license ""
  :depends-on ("liray"
               "fiveam")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for liray"
  :perform (test-op (op c) (symbol-call :fiveam :run! :liray)))
