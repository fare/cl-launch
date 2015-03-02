#!/bin/sh
#| -*- Lisp -*-
#!/usr/bin/cl -s inferior-shell -s optima.ppcre -E cl-launch-release::main
exec "$(dirname $0)/cl-launch.sh" \
  --system inferior-shell --system optima.ppcre \
  -X --package cl-launch-release --entry main -- "$0" "$@" ; exit
|#
(defpackage :cl-launch-release
  (:use :cl :uiop :asdf :inferior-shell :optima :optima.ppcre)
  ;; Note: the exports are the list of available commands.
  (:export #:rep #:clean
           #:source #:quickrelease
           #:debian-package #:publish-debian-package #:debian-package-all))

(in-package :cl-launch-release)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'argv0) (defun argv0 () "release.lisp")) ;; only in ASDF 3.1.2 or later
  (flet ((check-system-version (name version)
           (let ((system (find-system name)))
             (unless (version-satisfies system version)
               (die 2 "~A requires ~A at least ~A but only got version ~A"
                    (argv0) name version (component-version system))))))
    (check-system-version "inferior-shell" "2.0.0"))) ;; for default run outputs

(defvar *cl-launch-directory* ;; interactive users may want to override that.
  (truenamize
   (pathname-directory-pathname
    (ensure-absolute-pathname
     (or (argv0) *load-pathname* (nil-pathname)) #'getcwd))))

(defun pn (&optional x)
  (subpathname *cl-launch-directory* x))

(defun rep (argument)
  ;; read-eval-print, no loop.
  (eval-input (strcat "(in-package :cl-launch-release) " argument)))

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
  (with-current-directory ()
    (run `(./cl-launch.sh --include ,(getcwd) "-B" install_path))))

(defun quickrelease ()
  (let* ((version (script-version)) ;; no need to compare with the debian version
         (link (strcat "cl-launch-" version))
         (tarball (strcat link ".tar.gz")))
    (with-current-directory ()
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
  (sort (while-collecting (c) (do-external-symbols (x :cl-launch-release) (c x))) #'string<))

(defun main (argv)
  (multiple-value-bind (command status)
      (find-symbol (string-upcase (first argv)) :cl-launch-release)
    (if (eq status :external)
        (format t "~@[~{~S~^ ~}~%~]" (multiple-value-list (apply command (rest argv))))
        (die 2 "~A ~:[requires a command~;doesn't recognize command ~:*~A~].~%Try one of: ~(~{~A~^ ~}~)~%"
             (argv0) (first argv) (valid-commands))))
  t) ;; success according to uiop:restore-image.
