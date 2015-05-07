#| -*- Lisp -*-
#!/usr/bin/cl -sm cl-launch/release
exec "$(dirname $0)/cl-launch.sh" -X --system-main cl-launch/release -- "$0" "$@" ; exit
|#
(defpackage :cl-launch/release
  (:use :cl :uiop :asdf :inferior-shell :optima :optima.ppcre)
  ;; Note: these exports are also the list of available commands.
  (:export #:rep #:clean #:manpage
           #:source #:quickrelease
           #:get-date-from-git #:check-manual
           #:debian-package #:publish-debian-package #:debian-package-all))

(in-package :cl-launch/release)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((check-system-version (name version)
           (let ((system (find-system name)))
             (unless (version-satisfies system version)
               (die 2 "~A requires ~A at least ~A but only got version ~A"
                    (argv0) name version (component-version system))))))
    (check-system-version "asdf" "3.1.2") ;; for package-inferred-system, uiop:argv0
    (check-system-version "inferior-shell" "2.0.3"))) ;; for default run outputs, on-error error.

(defvar *cl-launch-directory* ;; interactive users may want to override that.
  (truenamize
   (pathname-directory-pathname
    (ensure-absolute-pathname
     (or (argv0) *compile-file-pathname* *load-pathname* (nil-pathname)) #'getcwd))))

(defun pn (&optional x)
  (subpathname *cl-launch-directory* x))

(defun rep (argument)
  ;; read-eval-print, no loop.
  (eval-input (strcat "(in-package :cl-launch/release) " argument)))

(defun script-version ()
  (match (read-file-line (pn "cl-launch.sh") :at 2)
    ((ppcre "^CL_LAUNCH_VERSION='([0-9]+([.][0-9]+)+)'$" version) version)))

(defun debian-version ()
  (match (read-file-line (pn "debian/changelog") :at 0)
    ((ppcre "^[^(]*\\(([-0-9.]+)\\)" x) x)))

(defun debian-version-tag (&optional (version (debian-version)))
  (first (split-string version :separator "-")))

(defun get-version ()
  (let ((sv (script-version))
        (dv (debian-version-tag)))
    (unless (equal sv dv)
      (error "version mismatch: cl-launch.sh says ~A whereas the debian/changelog says ~A" sv dv))
    sv))

(defun debian-arch ()
  (run/ss `(dpkg --print-architecture)))

(defun git-tag (&optional (pattern "4.*"))
  (with-current-directory ((pn))
    (run/ss `(git describe --tags --match ,pattern))))

(defun clean ()
  (with-current-directory ((pn))
    (run '(git clean -xfd)))
  (values))

(defun debian-package ()
  (let* ((debian-version (debian-version))
         (version (get-version))
         (origtarball (pn (strcat "../cl-launch_" version ".orig.tar.gz")))
         (home (user-homedir-pathname))
         (cl-launch-version (strcat "cl-launch-" version))
         (cldir (subpathname home "files/cl-launch/")))
    (check-manual)
    (with-current-directory ((pn))
      (run `(pwd) :show t)
      (clean)
      (delete-file-if-exists origtarball)
      (run `(git-buildpackage --git-debian-branch=master --git-upstream-branch=master (--git-upstream-tag= ,version) --git-tag --git-retag --git-ignore-branch) :show t)
      (run `(lintian --dont-check-part standards-version
                     --fail-on-warnings --profile debian
                     (../cl-launch_ ,debian-version _ ,(debian-arch) .changes)) :show t)
      (clean)
      (run `(pwd) :show t)
      (run `(./cl-launch.sh --include "." "-B" install_path) :show t)
      (run `(./cl-launch.sh --no-include -o cl-launch "-B" install_bin) :show t)
      (run `(cp ./cl-launch.sh (,cldir cl-launch.sh)) :show t))
    (with-current-directory ((pn "../"))
      (run `(pwd) :show t)
      (run `(rm -f ,cl-launch-version) :show t)
      (run `(ln -s cl-launch ,cl-launch-version) :show t)
      (run `(tar zcfh (,cldir ,cl-launch-version .tar.gz)
                 --exclude .git (,cl-launch-version /)) :show t)
      (run `(mv ,@(directory (merge-pathnames* (strcat "cl-launch_" version "*.*") (pn "../")))
                ,cldir) :show t)
      (run `(rm -f ,cl-launch-version) :show t))
    (with-current-directory (cldir)
      (run `(pwd) :show t)
      (run/interactive `(gpg -b -a (cl-launch- ,version .tar.gz)) :show t)
      (run `(ln -sf (,cl-launch-version .tar.gz) cl-launch.tar.gz) :show t)
      (run `(ln -sf (,cl-launch-version .tar.gz.asc) cl-launch.tar.gz.asc) :show t))))

(defun publish-debian-package ()
  (let* ((debian-version (debian-version))
         (home (user-homedir-pathname))
         (cldir (subpathname home "files/cl-launch/")))
    (with-current-directory (cldir)
      (run `(dput mentors (cl-launch_ ,debian-version _ ,(debian-arch) .changes)) :show t)
      (run `(rsync -av --delete ,cldir "common-lisp.net:/project/xcvb/public_html/cl-launch/") :show t)))
  (values))

(defun source ()
  (with-current-directory ((pn))
    (run `(./cl-launch.sh --include ,(getcwd) "-B" install_path))))

(defparameter *months* #("January" "February" "March" "April" "May" "June"
                         "July" "August" "September" "October" "November" "December"))

(defun get-date-from-manual () ;; Note: we don't put the date there anymore.
  (with-current-directory ((pn))
    (match (first (run/lines `(./cl-launch.sh --help)))
      ((ppcre "\"[(]([A-Z][a-z]+) ([0-9]+)[)]\"" month year)
       (if-let (pos (position month *months* :test 'equal))
         (list (parse-integer year) (1+ pos))
         (error "Invalid month ~a" month)))
      (_ (error "Can't extract month from manual")))))

(defun get-date-from-git (&optional tag)
  (with-current-directory ((pn))
    (match (first (run/lines `(git log -1 "--pretty=format:%cI" ,tag)))
      ((ppcre "^([0-9]+)-([0-9]+)-([0-9]+)T" year month day)
       (list (parse-integer year) (parse-integer month) (parse-integer day))))))

(defun check-manual-git-dates-match ()
  (let ((date-from-manual (get-date-from-manual))
        (date-from-git (subseq (get-date-from-git) 0 2)))
    (assert (equal date-from-manual date-from-git) ()
            "Manual says it's from ~{~D-~2,'0D~} but git commit is from ~{~D-~2,'0D~}"
            date-from-manual date-from-git)))

(defun manpage ()
  ;;(check-manual-git-dates-match)
  (with-current-directory ((pn))
    (run `(ln -sf cl-launch.sh cl))
    (run `(env ("PATH=.:" ,(getenv "PATH")) cl --more-help (> cl-launch.1.md)))
    (run `(ronn "--roff" "--manual=Shell Scripting with Common Lisp"
                "--organization=Francois-Rene Rideau"
                ,(format nil "--date=~{~D-~2,'0D-~2,'0D~}" (get-date-from-git))
                cl-launch.1.md (> 2 /dev/null)))))

(defun check-manual ()
  ;;(check-manual-git-dates-match)
  (manpage)
  (with-current-directory ((pn))
    (run `(cmp ./cl-launch.1 ./debian/cl-launch.1))))

(defun quickrelease ()
  (let* ((version (script-version)) ;; no need to compare with the debian version
         (link (strcat "cl-launch-" version))
         (tarball (strcat link ".tar.gz")))
    (check-manual)
    (with-current-directory ((pn))
      (clean)
      (source))
    (with-current-directory ((pn "../"))
      (run `(pwd) :show t)
      (run `(rm -f ,link))
      (run `(ln -s cl-launch ,link))
      (run `(tar zcfh ,tarball --exclude .git ,link))
      (run `(rsync -av cl-launch/cl-launch.sh ,tarball
                    "common-lisp.net:/project/xcvb/public_html/cl-launch/"))
      (run `(ssh common-lisp.net ln -sf ,tarball /project/xcvb/public_html/cl-launch/cl-launch.tar.gz))
      (run `(rm -f ,link ,tarball))))
  (values))

(defun debian-package-all ()
  (debian-package)
  (publish-debian-package))

(defun valid-commands ()
  (sort (while-collecting (c) (do-external-symbols (x :cl-launch/release) (c x))) #'string<))

(defun main (argv)
  (multiple-value-bind (command status)
      (find-symbol (string-upcase (first argv)) :cl-launch/release)
    (if (eq status :external)
        (format t "~@[~{~S~^ ~}~%~]" (multiple-value-list (apply command (rest argv))))
        (die 2 "~A ~:[requires a command~;doesn't recognize command ~:*~A~].~%Try one of: ~(~{~A~^ ~}~)~%"
             (argv0) (first argv) (valid-commands))))
  t) ;; success according to uiop:restore-image.
