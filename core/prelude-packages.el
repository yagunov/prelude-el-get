;;; prelude-packages.el --- Emacs Prelude: default package selection.
;;
;; Copyright © 2011-2013 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://batsov.com/emacs-prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Takes care of the automatic installation of all the packages required by
;; Emacs Prelude.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; put el-get into Prelude's directory
(setq el-get-dir (file-name-as-directory (expand-file-name "el-get" prelude-dir)))
(add-to-list 'load-path (file-name-as-directory (expand-file-name "el-get" el-get-dir)))
;; bootstrap el-get (version from the master branch)
(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (end-of-buffer)
      (eval-print-last-sexp)))
  (require 'el-get))

;; additional recipes
(setq el-get-sources
      '(
        (:name ack-and-a-half
               :description "Yet another front-end for ack."
               :type github :pkgname "jhelwig/ack-and-a-half")
        (:name exec-path-from-shell
               :description "A GNU Emacs library to setup environment variables from the user's shell."
               :type github :pkgname "purcell/exec-path-from-shell")
        (:name erlang :type elpa)
        (:name flycheck
               :description "Flymake done right."
               :type github :pkgname "lunaryorn/flycheck")
        (:name melpa :type elpa)
        (:name package
               :after (progn
                        (add-to-list 'package-archives
                                     '("melpa" . "http://melpa.milkbox.net/packages/") t)
                        ;; required because of a package.el bug
                        (setq url-http-attempt-keepalives nil)))
        (:name projectile
               :description "Manage and navigate projects in Emacs easily."
               :type github :pkgname "bbatsov/projectile")
        (:name ruby-tools
               :description "Collection of handy functions for ruby-mode."
               :type github :pkgname "rejeep/ruby-tools")
        (:name scala-mode2
               :description "A new scala-mode for emacs."
               :type github :pkgname "hvesalai/scala-mode2")
        )
      )

(defvar prelude-packages
  '(ack-and-a-half elisp-slime-nav exec-path-from-shell expand-region
                   flycheck gist
                   guru-mode helm magit magithub melpa projectile
                   rainbow-mode s volatile-highlights yasnippet)
  "A list of packages to ensure are installed at launch.")

(el-get 'sync prelude-packages)

(defmacro prelude-auto-install (extension package mode)
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (el-get-package-is-installed ',package)
                                   (el-get 'sync '(,package)))
                                 (,mode)))))

(defvar prelude-auto-install-alist
  '(("\\.clj\\'" clojure-mode clojure-mode)
    ("\\.coffee\\'" coffee-mode coffee-mode)
    ("\\.css\\'" css-mode css-mode)
    ("\\.erl\\'" erlang erlang-mode)
    ("\\.feature\\'" feature-mode feature-mode)
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hs\\'" haskell-mode haskell-mode)
    ("\\.latex\\'" auctex LaTeX-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.markdown\\'" markdown-mode markdown-mode)
    ("\\.md\\'" markdown-mode markdown-mode)
    ("\\.php\\'" php-mode php-mode)
    ("\\.py\\'" python python-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode2 scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)))

;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually if it's already installed
(when (el-get-package-is-installed 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(-each prelude-auto-install-alist
       (lambda (entry)
         (let ((extension (car entry))
               (package (cadr entry))
               (mode (cadr (cdr entry))))
           (unless (el-get-package-is-installed package)
             (prelude-auto-install extension package mode)))))

(defun prelude-ensure-module-deps (packages)
  (el-get 'sync packages))

(provide 'prelude-packages)

;;; prelude-packages.el ends here
