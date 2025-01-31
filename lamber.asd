;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(in-package :asdf)

(defsystem "lamber"
  :description "Lamber is a minimalist functional language compiling to pure Lambda Calculus."
  :author "Artyom Bologov"
  :homepage "https://github.com/aartaka/lamber"
  :bug-tracker "https://github.com/aartaka/lamber/issues"
  :source-control (:git "https://github.com/aartaka/lamber.git")
  :license  "BSD-2 Clause"
  :version "0.0.0"
  :serial t
  :build-operation "program-op"
  :build-pathname "lamber"
  :entry-point "lamber::entry-point"
  :components ((:module "lamber"
                :pathname "source/"
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "reader")
                 (:file "inferrer")
                 (:file "compiler")
                 (:file "evaluator")
                 (:file "printer")
                 (:file "cli")))))
