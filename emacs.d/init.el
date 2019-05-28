;; Not a fan of backup files
(setq make-backup-files nil)

;; Permit narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; Setup package.el
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure flymake for Python
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

;; Set as a minor mode for Python
(add-hook 'python-mode-hook '(lambda () (flymake-mode)))

;; Configure to wait a bit longer after edits before starting
(setq-default flymake-no-changes-timeout '1)

;; Keymaps to navigate to the errors
(add-hook 'flymake-mode-hook '(lambda () (define-key python-mode-map "\C-cn" 'flymake-goto-next-error)))
(add-hook 'flymake-mode-hook '(lambda () (define-key python-mode-map "\C-cp" 'flymake-goto-prev-error)))

;; To avoid having to mouse hover for the error message, these functions make flymake error messages
;; appear in the minibuffer
(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the message in the minibuffer"
  (require 'cl)
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
      (let ((err (car (second elem))))
        (message "%s" (flymake-ler-text err)))))))

(add-hook 'post-command-hook 'show-fly-err-at-point)

;; Column number mode
(setq column-number-mode t)

;; ORG mode
(require 'cl-lib) ;; include common-lisp facilities for `sequence`
(require 'use-package)
(use-package org)
(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(i)" "RUNNING(r)" "WAITING(w)" "HOLD(h)" "|" "DONE(d)" "ABANDONED(a)")))

(org-defkey org-mode-map (kbd "M-C") 'org-table-insert-column)
(org-defkey org-mode-map (kbd "M-R") 'org-table-insert-row)
(org-defkey org-mode-map (kbd "C-M-K") 'org-table-delete-column)
(org-defkey org-mode-map (kbd "C-c |") 'org-table-convert-region)
(org-defkey org-mode-map (kbd "C-M-n") 'org-table-copy-down)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t) (python . t) (latex . t)))

(setq org-src-fontify-natively t)

(setq org-agenda-files (quote ("~/.emacs.d/agenda.org")))

(setq org-startup-indented t) ; Enable `org-indent-mode' by default
(add-hook 'org-mode-hook #'visual-line-mode)
