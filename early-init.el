;;; early-init.el --evaluated before init.el

(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-deferred-compilation t
        native-comp-async-query-on-exit t
        native-comp-async-jobs-number 4
        native-comp-async-report-warnings-errors 'silent))

(setq inhibit-startup-echo-area-message "thierry")

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(setq package-load-list '((all-the-icons t) all))
