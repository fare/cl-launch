#!/bin/sh
#| -*- Lisp -*-
exec "$(dirname $0)/cl-launch.sh" \
  --system inferior-shell --system optima.ppcre --load "$0" \
  --init "(in-package :cl-launch-release) (setf *0* (pop *command-line-arguments*))" \
  --entry cl-launch-release::main \
  -- "$0" "$@" ; exit
|#
(defpackage :cl-launch-release
  (:use :cl :uiop :asdf :inferior-shell :optima :optima.ppcre)
  (:export #:rep #:clean
           #:debian-package #:debian-package-all #:quickrelease))

(in-package :cl-launch-release)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *0* "cl-launch/release.lisp")

  (flet ((check-system-version (name version)
           (let ((system (find-system name)))
             (unless (version-satisfies system version)
               (die 2 "~A requires ~A at least ~A but only got version ~A"
                    *0* name version (component-version system))))))
    (check-system-version "inferior-shell" "2.0.0")))

(defun rep (argument) (eval-input argument)) ;; read-eval-print, no loop.

(defun debian-version ()
  (match (read-file-line "debian/changelog" :at 0)
    ((ppcre "^[^(]*\\(([-0-9.]+)\\)" x) x)))

(defun debian-version-tag (&optional (version (debian-version)))
  (first (split-string version :separator "-")))

(defun debian-arch ()
  "amd64")

(defun git-tag (&optional (pattern "4.*"))
  (run/ss `(git describe --tags --match ,pattern)))

(defun clean ()
  (run '(git clean -xfd))
  (values))

(defun debian-package ()
  (clean)
  (let* ((version (debian-version))
         (tag (debian-version-tag version))
         (up (pathname-parent-directory-pathname (getcwd)))
         (origtarball (subpathname up (strcat "cl-launch_" tag ".orig.tar.gz"))))
    (delete-file-if-exists origtarball)
    (run `(git-buildpackage --git-debian-branch=master --git-upstream-branch=master (--git-upstream-tag= ,tag) --git-tag --git-retag --git-ignore-branch))
    (run `(lintian -c --fail-on-warnings (../cl-launch_ ,version _ ,(debian-arch) .changes))))
  (clean))

(defun debian-package-all ()
  (let* ((version (debian-version))
         (tag (debian-version-tag version))
         (up (pathname-parent-directory-pathname (getcwd)))
         (home (user-homedir-pathname))
         (cl-launch-tag (strcat "cl-launch-" tag))
         (cldir (subpathname home "files/cl-launch/")))
    (debian-package)
    (run `(./cl-launch.sh --include "." -B install_path))
    (run `(./cl-launch.sh --no-include -o cl-launch -B install_bin))
    (with-current-directory (up)
      (run `(rm -f ,cl-launch-tag))
      (run `(ln -s cl-launch ,cl-launch-tag))
      (run `(tar zcfh (,cldir ,cl-launch-tag .tar.gz)
                  --exclude .git ,cl-launch-tag))
      (run `(cp cl-launch/cl-launch.sh (,cldir cl-launch.sh)))
      (run (format nil "mv cl-launch_~A* ~A" tag cldir))
      (run `(rm -f ,cl-launch-tag)))
    (with-current-directory (cldir)
      (run `(ln -sf (cl-launch ,tag .tar.gz) cl-launch.tar.gz))
      (run/interactive `(gpg -b -a (cl-launch- ,tag .tar.gz)))
      (run `(ln -sf (cl-launch- ,tag .tar.gz) cl-launch.tar.gz))
      (run `(ln -sf (cl-launch- ,tag .tar.gz.asc) cl-launch.tar.gz.asc))
      (run `(dput mentors (cl-launch_ ,version _ ,(debian-arch) .changes)))
      (run `(rsync -av --delete ,cldir "common-lisp.net:/project/xcvb/public_html/cl-launch/")))
    (values)))

(defun cl-launch-version ()
  (match (read-file-line "cl-launch.sh" :at 2)
    ((ppcre "^CL_LAUNCH_VERSION='([0-9]+([.][0-9]+)+)'$" version) version)))

(defun quickrelease ()
  (let* ((version (cl-launch-version))
         (link (strcat "cl-launch-" version))
         (tarball (strcat link ".tar.gz")))
    (with-current-directory ("../")
      (run `(rm -f ,link))
      (run `(ln -s cl-launch ,link))
      (run `(tar zcfh ,tarball --exclude .git ,link))
      (run `(rsync -av cl-launch/cl-launch.sh ,tarball
                    "common-lisp.net:/project/xcvb/public_html/cl-launch/"))
      (run `(ssh common-lisp.net ln -sf ,tarball /project/xcvb/public_html/cl-launch/cl-launch.tar.gz))
      (run `(rm -f ,link ,tarball))))
  (values))

(defun valid-commands ()
  (sort (while-collecting (c) (do-external-symbols (x) (c x))) #'string<))

(defun main (argv)
  (multiple-value-bind (command status)
      (find-symbol (string-upcase (first argv)) :cl-launch-release)
    (if (eq status :external)
        (format t "~@[~{~S~^ ~}~%~]" (multiple-value-list (apply command (rest argv))))
        (die 2 "~A ~:[requires a command~;doesn't recognize command ~:*~A~].~%Try one of: ~(~{~A~^ ~}~)~%"
             *0* (first argv) (valid-commands)))))
