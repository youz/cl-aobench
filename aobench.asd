;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ASDF; -*-
(in-package #:asdf)

(defsystem #:aobench
  :name "aobench"
  :description "aobench (http://code.google.com/p/aobench/) in CommonLisp"
  :author "Yousuke Ushiki <citrus.yubeshi@gmail.com>"
  :maintainer "Yousuke Ushiki <citrus.yubeshi@gmail.com>"
  :version "1.0"
  :license "New BSD License"
  :depends-on (:lparallel)
  :components ((:file "aobench")))


(defmethod perform ((o test-op) (c (eql (find-system '#:aobench))))
  (let ((pkg (find-package '#:aobench)))
    (format t "AO SAMPLES: 8~%SUBSAMPLING: 2~%WIDTH: 256~%HEIGHT: 256~%THREADS: 2~%")
    (time (funcall (intern (string '#:render) pkg) 256 256 2 2))
    (format t "ok.")))
