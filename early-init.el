;;; early-init.el --- evaluated before init.el -*- lexical-binding: t -*-

(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-deferred-compilation t
        native-comp-async-query-on-exit t
        native-comp-async-jobs-number 4
        native-comp-async-report-warnings-errors 'silent))

(advice-add 'emacs-repository-get-version :override #'ignore)
(advice-add 'emacs-repository-get-branch :override #'ignore)
(add-hook 'after-init-hook (lambda ()
                             (advice-remove 'emacs-repository-get-version #'ignore)
                             (advice-remove 'emacs-repository-get-branch #'ignore)))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(setq package-load-list '(all))
;; (setq package-load-list '((helm nil) (helm-core nil) (async nil) (wfnames nil) all))
