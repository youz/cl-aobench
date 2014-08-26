(declaim (optimize (speed 3) (debug 0) (safety 0)))

(defpackage #:aobench
  (:use :cl)
  (:export #:*image-width*
	   #:*image-height*
	   #:*nsubsamples*
	   #:*nao-samples*
	   #:run)
  (:shadow #:dotimes))

(in-package #:aobench)

(defvar *pgm-filename* "ao.pgm")
(defvar *image-width*  256)
(defvar *image-height* 256)
(defvar *nsubsamples* 2)
(defvar *nao-samples* 8)

(deftype int () '(unsigned-byte 16))
(deftype num () 'double-float)
(deftype vec () '(simple-array num (3)))

(declaim (type int *image-width* *image-height* *nsubsamples* *nao-samples*))

(defmacro dotimes ((idx n) &body body)
  `(loop :for ,idx :of-type int :from 0 :below ,n :do ,@body))

(declaim
 (inline vec vadd vsub vcross vdot vlength vnormalize!)
 (ftype (function (num num num) vec) vec)
 (ftype (function (vec vec) vec) vadd)
 (ftype (function (vec vec) vec) vsub)
 (ftype (function (vec vec) vec) vcross)
 (ftype (function (vec vec) num) vdot)
 (ftype (function (vec) num) vlength)
 (ftype (function (vec) vec) vnormalize!))

(defun vec (x y z)
  (make-array 3 :element-type 'num
	      :initial-contents (list x y z)))

(defmacro vx (v) `(aref ,v 0))
(defmacro vy (v) `(aref ,v 1))
(defmacro vz (v) `(aref ,v 2))
(defmacro tonum (n) `(coerce ,n 'num))

(defun vadd (a b)
  (vec (+ (vx a) (vx b))
       (+ (vy a) (vy b))
       (+ (vz a) (vz b))))

(defun vsub (a b)
  (vec (- (vx a) (vx b))
       (- (vy a) (vy b))
       (- (vz a) (vz b))))

(defun vcross (a b)
  (vec (- (* (vy a) (vz b)) (* (vz a) (vy b)))
       (- (* (vz a) (vx b)) (* (vx a) (vz b)))
       (- (* (vx a) (vy b)) (* (vy a) (vx b)))))

(defun vdot (a b)
  (+ (* (vx a) (vx b))
     (* (vy a) (vy b))
     (* (vz a) (vz b))))

(defun vlength (v)
  (sqrt (vdot v v)))

(defun vnormalize! (a)
  (let ((l (vlength a)))
    (when (> l 1d-17)
      (setf (vx a) (/ (vx a) l)
	    (vy a) (/ (vy a) l)
	    (vz a) (/ (vz a) l)))
    a))


(defstruct
  (isect
   (:predicate isectp))
  (dist 1d6 :type num)
  (hit nil :type boolean)
  (p nil :type vec)
  (n nil :type vec))

(defstruct ray
  (org nil :type vec)
  (dir nil :type vec))

(declaim (ftype (function (vec num) function) sphere)
	 (ftype (function (vec vec) function) plane))

(defun sphere (center radius)
  #'(lambda (r is)
      (let* ((ro (ray-org r))
	     (rd (ray-dir r))
	     (rs (vsub ro center))
	     (b (vdot rs rd))
	     (c (- (vdot rs rs) (* radius radius)))
	     (d (- (* b b) c)))
	(when (> d 0d0)
	  (let ((dist (- (- b) (sqrt d))))
	    (when (< 0d0 dist (isect-dist is))
	      (setf (isect-dist is) dist
		    (isect-hit is) t
		    (isect-p is) (vec (+ (vx ro) (* dist (vx rd)))
				      (+ (vy ro) (* dist (vy rd)))
				      (+ (vz ro) (* dist (vz rd))))
		    (isect-n is) (vnormalize! (vsub (isect-p is) center)))))))))

(defun plane (p n)
  #'(lambda (r is)
      (let* ((ro (ray-org r))
	     (rd (ray-dir r))
	     (d (- (vdot p n)))
	     (v (vdot rd n)))
	(when (> (abs v) 1d-17)
	  (let ((dist (/ (- (+ (vdot ro n) d)) v)))
	    (when (< 0d0 dist (isect-dist is))
	      (setf (isect-dist is) dist
		    (isect-hit is) t
		    (isect-n is) n
		    (isect-p is)
		    (vec (+ (vx ro) (* dist (vx rd)))
			 (+ (vy ro) (* dist (vy rd)))
			 (+ (vz ro) (* dist (vz rd)))))))))))

(declaim (ftype (function (num) (unsigned-byte 8)) clamp))
(defun clamp (f)
  (let ((i (* f 255.5d0)))
    (cond ((< i 0) 0)
	  ((> i 255) 255)
	  (t (round i)))))

(declaim (ftype (function (vec) (values vec vec vec)) ortho-basis))
(defun ortho-basis (n)
  (let* ((tmp (cond ((< -0.6 (vx n) 0.6) #0=(vec 1d0 0d0 0d0))
		    ((< -0.6 (vy n) 0.6) (vec 0d0 1d0 0d0))
		    ((< -0.6 (vz n) 0.6) (vec 0d0 0d0 1d0))
		    (t #0#)))
	 (b0 (vnormalize! (vcross tmp n))))
    (values b0 (vnormalize! (vcross n b0)) n)))


(declaim (type simple-vector *scene*))
(defvar *scene*
  (vector (sphere (vec -2d0 0d0 -3.5d0) 0.5d0)
	  (sphere (vec -0.5d0 0d0 -3d0) 0.5d0)
	  (sphere (vec 1d0 0d0 -2.2d0) 5d-1)
	  (plane (vec 0d0 -0.5d0 0d0) (vec 0d0 1d0 0d0))))


(declaim (ftype (function (isect) num) ambient-occlusion))
(defun ambient-occlusion (is)
  (multiple-value-bind (b0 b1 b2) (ortho-basis (isect-n is))
    (let* ((ntheta *nao-samples*)
	   (nphi *nao-samples*)
	   (eps 1d-4)
	   (occlusion 0d0)
	   (ray (make-ray :org (vec (+ (vx (isect-p is)) (* eps (vx (isect-n is))))
				    (+ (vy (isect-p is)) (* eps (vy (isect-n is))))
				    (+ (vz (isect-p is)) (* eps (vz (isect-n is)))))))
	   (occ-isect (make-isect)))
      (declare (type int ntheta nphi)
	       (type num occlusion))
      (dotimes (j nphi)
	(dotimes (i ntheta)
	  (let* ((r (random 1d0))
		 (phi (* 2d0 pi (random 1d0)))
		 (sqr (sqrt r))
		 (x (* (cos phi) sqr))
		 (y (* (sin phi) sqr))
		 (z (sqrt (- 1d0 r))))
	    (declare (type num x y z))
	    (setf (ray-dir ray) (vec (+ (* x (vx b0)) (* y (vx b1)) (* z (vx b2)))
				     (+ (* x (vy b0)) (* y (vy b1)) (* z (vy b2)))
				     (+ (* x (vz b0)) (* y (vz b1)) (* z (vz b2))))
		  (isect-hit occ-isect) nil
		  (isect-dist occ-isect) 1d6)
	    (loop for f across *scene* do
	      (funcall (the (function (ray isect)) f) ray occ-isect))
	    (when (isect-hit occ-isect)
	      (incf occlusion)))))
      (- 1 (/ occlusion ntheta nphi)))))


(declaim (ftype (function (int int int) (array (unsigned-byte 8))) render))
(defun render (w h nsubs)
  (let ((image (make-array (list w h) :element-type '(unsigned-byte 8)))
	(hw (/ w 2d0))
	(hh (/ h 2d0)))
    (dotimes (y h)
      (dotimes (x w)
	(let ((rad 0d0))
	  (declare (type num rad))
	  ;; subsampling
	  (dotimes (v nsubs)
	    (dotimes (u nsubs)
	      (let* ((px (/ (+ x (/ u nsubs) (- hw)) hw))
		     (py (- (/ (+ y (/ v nsubs) (- hh)) hh)))
		     (eye (vnormalize! (vec px py -1d0)))
		     (ray (make-ray :org (vec 0d0 0d0 0d0) :dir eye))
		     (isect (make-isect)))
		(loop for f across *scene* do
		  (funcall (the (function (ray isect)) f) ray isect))
		(when (isect-hit isect)
		  (incf rad (ambient-occlusion isect))))))
	  (setf (aref image x y) (clamp (/ rad nsubs nsubs))))))
    image))



(defun write-pnm (file image w h)
  (declare (type int w h))
  (with-open-file (s file :direction :output
		     :if-exists :overwrite :if-does-not-exist :create)
    (declare (type stream s))
    (format s "P2~%~D ~D~%255~%" w h)
    (dotimes (y h)
      (dotimes (x w)
	(let ((p (aref image x y)))
	  (format s "~D~%" p)))))
  t)


(defun run (&key (file *pgm-filename*) (w *image-width*) (h *image-height*))
  (format t "AO SAMPLES: ~D~%SUBSAMPLING: ~D~%WIDTH: ~D~%HEIGHT: ~D~%"
	  *nao-samples* *nsubsamples* w h)
  (format t "writing ~A" file)
  (let ((img (time (render w h *nsubsamples*))))
    (write-pnm file img w h))
  (format t "~&done.")
  t)


