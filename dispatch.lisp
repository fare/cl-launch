(defpackage :cl-launch/dispatch
  (:use :uiop :cl)
  (:export #:dispatch-entry #:get-entry
	   #:register-name/entry #:register-entry
	   #:dispatch-entry-error #:get-name #:basename #:all-entry-names
	   #:dispatcher #:not-found))

(in-package :cl-launch/dispatch)

(defvar *default-behavior* 'dispatcher
  "the default behavior if the argv0 invocation name is not recognized.")

(defvar *entries* (make-hash-table :test 'equal)
  "a table of entries, associating strings (basename of a command) to entry")

(define-condition dispatch-entry-error (simple-error) ())

(defun dispatch-entry-error (format &rest arguments)
  (error 'dispatch-entry-error :format-control format :format-arguments arguments))

;; TODO: shall we default the name of the entry point to main?
(defun split-name/entry (name/entry &optional (package *package*) (default-entry "main"))
  "split name and entry from a name-entry specification"
  (flet ((f (name entry)
           (values (and (not (emptyp name)) name)
                   (ensure-function entry :package package))))
    (if-let ((slash (position #\/ name/entry)))
      (f (subseq name/entry 0 slash) (subseq name/entry (1+ slash)))
      (f name/entry default-entry))))

(defun register-entry (name entry)
  (if name
      (setf (gethash name *entries*) entry)
      (setf *default-behavior* entry)))

(defun register-name/entry (name/entry &optional (package *package*))
  (multiple-value-call 'register-entry (split-name/entry name/entry package)))

(defun get-entry (name)
  "Given a string NAME, return the dispatch entry registered for that NAME.
If NAME is NIL, return the value of *DEFAULT-BEHAVIOR*."
  (if name
      (gethash name *entries*)
      *default-behavior*))

(defun basename (name)
  (let ((base
	 (if-let ((slash (position #\/ name :from-end t)))
	    (subseq name (1+ slash))
	    name)))
    (and (not (emptyp base)) base)))

(defun get-name ()
  (basename (uiop:argv0)))

(defun all-entry-names ()
  (sort (loop :for k :being :the :hash-keys :of *entries* :collect k) 'string<))

(defun dispatcher (argv)
  (if (null argv)
      (die 2 "~A available commands: ~{~A~^ ~}"
	    (get-name) (all-entry-names))
      (dispatch-entry (rest argv) (first argv))))

(defun not-found (argv)
  (declare (ignore argv))
  (if-let ((name (get-name)))
    (die 3 "~A command not found." (get-name))
    (die 4 "could not determine command name")))

(defun dispatch-entry (argv &optional (name (get-name)))
  (funcall (or (get-entry name) *default-behavior*) argv))

;; The below function is NOT exported, but you can use :import-from so -Ds works trivially with some system of yours:
(defun main (argv)
  (dispatcher argv))

(when (null *image-entry-point*)
  (setf *image-entry-point* #'dispatcher))
