;;; early-init.el --evaluated before init.el

(setq package-archives        '(("melpa"        . "https://melpa.org/packages/")
                                ("melpa-stable" . "https://stable.melpa.org/packages/")
                                ("gnu"          . "https://elpa.gnu.org/packages/")
                                )
      package-pinned-packages '((async       . "melpa")
                                (magit       . "melpa-stable")
                                (magit-popup . "melpa-stable")
                                (git-commit  . "melpa-stable")
                                (with-editor . "melpa-stable")
                                (undo-tree . "melpa")
                                (realgud . "melpa-stable"))
      package-check-signature  nil)
