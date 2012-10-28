;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ASDF; -*-
(in-package #:asdf)

(defsystem #:aobench
  :name "aobench"
  :description "aobench (http://code.google.com/p/aobench/) in CommonLisp"
  :author "Yousuke Ushiki <citrus.yubeshi@gmail.com>"
  :maintainer "Yousuke Ushiki <citrus.yubeshi@gmail.com>"
  :version "1.0"
  :license "New BSD License"
  :components ((:file "aobench")))


(defmethod perform ((o test-op) (c (eql (find-system '#:aobench))))
  (time (funcall (find-symbol (symbol-name '#:render)
			      (find-package '#:aobench))
		 256 256 2)))
