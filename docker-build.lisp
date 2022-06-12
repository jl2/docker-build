;; docker-build.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :docker-build)

(defun host-lisp ()
  (cons (lisp-implementation-type)
        (lisp-implementation-version)))

(defun make-container (package
                       &key
                         (lisp (host-lisp))
                         (container-name package)
                         (dockerfile (asdf:system-relative-pathname package "Dockerfile"))
                         (evaluate-form 'main)
                         (linux-distro "debian")
                         (extra-lisp-arguments)
                         (port-map)
                         (os-extra-package-list)
                         (lisp-extra-package-list))
  "Create a Dockerfile for running a Lisp package in Docker."
  )

(defun start (package &key (eval nil))
  "Start package in a previously build Docker container."
  )

(defmacro eval-in-docker ((&rest rest
                           &key
                             (package :docker-package)
                             (lisp (host-lisp))
                             (container-name package)
                             (dockerfile (asdf:system-relative-pathname package "Dockerfile"))
                             (linux-distro "debian")
                             (extra-lisp-arguments)
                             (port-map)
                             (os-extra-package-list)
                             (lisp-extra-package-list)
                           )
                          &body body)
  "Evaluate @,body in a specific Common Lisp running in a Docker container.
Creates a Docker container for the Lisp if necessary"

  ;; These are forwarded to make-container via the &rest parameter
  (declare (ignorable lisp container-name dockerfile linux-distro
                      extra-lisp-arguments port-map os-extra-package-list
                      lisp-extra-package-list))

  (let ((args-without-package (remove package (remove :package rest :test #'equal) :test #'equal)))
    (apply #'make-container package args-without-package)
    (start package :eval
           `(list
             (in-package ,package)
             @,body))))
