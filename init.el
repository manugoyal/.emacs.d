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

;; Loads the cl library, since that's important and should come before
;; everything else
(require 'cl)

;; We keep a list of installed packages here. We go through this list, and for
;; any package that isn't installed, we install it. To obtain this list on a
;; full installation, get the value of the 'package-activated-list variable.
(defconst installed-packages
  '(ace-jump-mode ag s dash auctex autopair cider queue pkg-info
  epl clojure-mode dired+ dired-sort-menu elpy yasnippet pyvenv
  idomenu highlight-indentation find-file-in-project company
  exec-path-from-shell f flx-ido flx flycheck framemove fuzzy
  go-autocomplete auto-complete popup go-mode haskell-mode helm
  async iedit jedi python-environment deferred epc ctable
  concurrent js2-mode less-css-mode magit-push-remote magit
  git-rebase-mode git-commit-mode magit-tramp markdown-mode
  multi-term multi-web-mode multiple-cursors paredit rainbow-mode
  request scss-mode smex sml-mode solarized-theme sql-indent
  tuareg caml undo-tree virtualenv web-mode websocket wgrep-ag
  wgrep wrap-region zenburn-theme)
  )

;; Sets up package
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("elpy" . "http://jorgenschaefer.github.io/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(dolist (package installed-packages)
  (unless (package-installed-p package) (package-install package)))

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
