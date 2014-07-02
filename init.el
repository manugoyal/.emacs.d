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

;; Sets the locations of the .emacs.d directory and other variables
(defconst dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(defconst downloads-dir (concat dotfiles-dir "downloads/"))
(defconst configuration-file (concat dotfiles-dir "README.org"))

;; Sets up package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; We keep a list of installed packages here. We go through this list, and for
;; any package that isn't installed, we install it.
(defconst installed-packages
  '(
    ace-jump-mode autopair ag async company dired+ dired-sort-menu emacs-eclim
    exec-path-from-shell find-file-in-project flycheck f framemove fuzzy
    go-autocomplete go-mode haskell-mode highlight-indentation idomenu iedit
    jedi auto-complete epc ctable concurrent less-css-mode magit-push-remote
    magit-tramp magit git-rebase-mode git-commit-mode markdown-mode multi-term
    multiple-cursors org pkg-info epl popup python-environment deferred pyvenv
    request s scss-mode smex sml-mode solarized-theme sql-indent tuareg caml
    undo-tree virtualenv websocket wgrep-ag wgrep wrap-region dash yasnippet
    )
  "A list of packages that should be installed at startup"
  )
(dolist (package installed-packages)
  (if (not (package-installed-p package))
      (progn
        (message "Installing package: %s" package)
        (package-install package)))
  )

;; Loads custom.el
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file)

;; Load up Org Mode and Org Babel for elisp embedded in
;; configuration-file
(require 'org-id)
(require 'org-element)
(require 'org)
(org-babel-load-file configuration-file)

(provide 'init)
;; init.el ends here
