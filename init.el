;;; init.el --- Where all the magic begins
;;
;;; Commentary:
;; This file loads both
;; - Org-mode : http://orgmode.org/ and
;; - Org-babel: http://orgmode.org/worg/org-contrib/babel/org-babel.php#library-of-babel
;;
;; It then loads the rest of our Emacs initialization from Emacs Lisp
;; embedded in literate Org-mode files.
;;
;;; Code:

;; Sets the locations of the .emacs.d directory and the downloads directory
(defconst dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(defconst downloads-dir (concat dotfiles-dir "downloads/"))

;; Loads CEDET before loading anything else. We configure it in
;; configuration.org
(load-file (concat downloads-dir "cedet/cedet-devel-load.el"))
(require 'ede)
(require 'semantic)
(require 'eieio)
(require 'srecode)

;; Sets up package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Loads custom.el
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file)

;; Load up Org Mode and Org Babel for elisp embedded in
;; configuration.org
(require 'org-id)
(require 'org-element)
(require 'org)
(org-babel-load-file (concat dotfiles-dir "configuration.org"))

(provide 'init)
;; init.el ends here
