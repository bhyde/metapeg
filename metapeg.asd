(defsystem metapeg
  :description "metapeg PEG parser generator"
  :version "20070504"
  :author "John Leuner"
  :licence "MIT License"
  :components (	(:file "package")
		(:file "libmetapeg" :depends-on ("package"))
;               (:file "metapeg" :depends-on ("libmetapeg"))
)
  :depends-on ())


