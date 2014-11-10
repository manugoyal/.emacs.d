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
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(package-refresh-contents)

;; We keep a list of installed packages here. We go through this list, and for
;; any package that isn't installed, we install it. To obtain this list on a
;; full installation, get the value of the 'package-activated-list variable.
(defconst installed-packages
  '(ace-jump-mode ag async auctex autopair base16-theme cider queue pkg-info epl dash clojure-mode clojure-mode dired+ dired-sort-menu elpy yasnippet pyvenv idomenu highlight-indentation find-file-in-project company emacs-eclim s exec-path-from-shell f dash s find-file-in-project flx-ido flx flycheck pkg-info epl dash framemove fuzzy go-autocomplete auto-complete popup go-mode haskell-mode highlight-indentation ido-ubiquitous ido-vertical-mode idomenu iedit jedi python-environment deferred auto-complete popup epc ctable concurrent deferred js2-mode less-css-mode leuven-theme magit-push-remote magit git-rebase-mode git-commit-mode magit-tramp magit git-rebase-mode git-commit-mode markdown-mode monokai-theme multi-term multi-web-mode multiple-cursors paredit pkg-info epl polymode popup python-environment deferred pyvenv queue rainbow-mode request s scss-mode smex sml-mode solarized-theme dash sql-indent tuareg caml undo-tree virtualenv web-mode websocket wgrep-ag wgrep wrap-region dash yasnippet zenburn-theme)
  "A list of packages that should be installed at start-up."
  )
(dolist (package installed-packages)
  (if (not (package-installed-p package))
      (progn
        (message "Installing package: %s" package)
        (package-install package)))
  )

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
(org-babel-load-file configuration-file)

(provide 'init)
;;; init.el ends here
