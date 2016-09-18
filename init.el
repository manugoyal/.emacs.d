;;; init.el --- Where all the magic begins
;;
;;; Commentary:
;; This file loads both org-mode and org-babel. It then loads the rest of our
;; Emacs initialization from Emacs Lisp embedded in literate Org-mode files.
;;
;;; Code:

;; Sets the locations of the .emacs.d directory and other variables
(defconst base-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(defconst downloads-dir (concat base-dir "downloads/"))
(defconst configuration-file (concat base-dir "README.org"))

;; Sets up package
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))
;; Initialize the package system but don't load all the packages
(package-initialize nil)
(setq package-enable-at-startup nil)
;; Refresh the package list if it isn't there
(unless package-archive-contents (package-refresh-contents))

;; We use use-package to organize our package loading and initialization, so
;; load that here
(unless (package-installed-p 'use-package) (package-install 'use-package))
(require 'use-package)

;; Loads custom.el. The custom.el file should only contain settings that are
;; long lists of things or contain data too long or unwieldy to type out
;; manually.
(setq custom-file (concat base-dir "custom.el"))
(load custom-file)

;; Load up Org Mode and Org Babel for elisp embedded in
;; configuration-file
(require 'org-id)
(require 'org-element)
(require 'org)
;; Load our configuration file
(org-babel-load-file configuration-file)

(provide 'init)
(put 'downcase-region 'disabled nil)
