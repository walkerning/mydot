;;;;; --- Start ---
;;;; -- Setup package.el, and bootstrap `use-package' --
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
;; Q: should we use always-ensure?

;;;; -- View settings --
;; Show line number at left side
(cond ((version<= emacs-version "26.0.0")
       (global-linum-mode 1))
      (t (global-display-line-numbers-mode)))

;; Column number mode (show column number at the mode line)
(setq column-number-mode t)

;; Disable tool bar, scroll bar
(tool-bar-mode -1)
(toggle-scroll-bar -1)


;;;; -- Misc --
;; Not a fan of backup files
(setq make-backup-files nil)

;; Permit narrow-to-region
(put 'narrow-to-region 'disabled nil)


;;;; -- Flymake for Python --
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
(load "flymake")
(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the message in the minibuffer"
  (use-package cl)
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
      (let ((err (car (second elem))))
        (message "%s" (flymake-ler-text err)))))))

(add-hook 'post-command-hook 'show-fly-err-at-point)


;;;; -- ORG mode --
(use-package cl-lib) ;; include common-lisp facilities for `sequence`
(use-package org)

;;; key bindings
(define-key org-mode-map (kbd "M-C") 'org-table-insert-column)
(define-key org-mode-map (kbd "M-R") 'org-table-insert-row)
(define-key org-mode-map (kbd "C-M-K") 'org-table-delete-column)
(define-key org-mode-map (kbd "C-c |") 'org-table-convert-region)
(define-key org-mode-map (kbd "C-M-n") 'org-table-copy-down)

;;; org mode files
(setq org-directory (expand-file-name "~/org"))
(setq org-default-notes-file (concat org-directory "/captured.org"))
(setq org-agenda-files (quote ("~/org")))

;;; basic settings
(setq org-startup-indented t) ; Enable `org-indent-mode' by default
(setq org-cycle-separator-lines 1)
(add-hook 'org-mode-hook #'visual-line-mode) ; autowarp
;; make the section marked by ** use foreground red
(add-to-list 'org-emphasis-alist
             '("*" (:foreground "red")
               ))

;;; Export
(setq org-export-coding-system 'utf-8)

;;; TODO keywords, tags settings
(setq org-todo-keywords
      '((sequence "IDEA(i)" "TODO(t)" "STARTED(s)" "RUNNING(r)" "WAITING(w)" "HOLD(h)" "|" "DONE(d)" "ABANDONED(a)")))

(setq org-tag-persistent-alist
      '((:startgroup . nil) ;; todo type
	("learning" . ?l)
	("research" . ?r)
	("fun" . ?f)
	("arranging" . ?a)
	("work" . ?w)
	("other" . ?o)
	(:endgroup . nil)
	(:startgroup . nil) ;; more concrete task
	("paper-reading" . ?p)
	("coding" . ?c)
	(:endgroup . nil)
	(:startgroup . nil) ;; difficulty
	("EASY" . ?e)
        ("MEDIUM" . ?m)
        ("HARD" . ?h)
	(:endgroup . nil)
        ("URGENT" . ?u) ;; importance
	))

(setq org-tag-faces
      '(
	("learning" . (:foreground "GoldenRod" :weight bold))
        ("research" . (:foreground "GoldenRod" :weight bold))
	("fun" . (:foreground "GoldenRod" :weight bold))
	("arranging" . (:foreground "GoldenRod" :weight bold))
	("work" . (:foreground "GoldenRod" :weight bold))
        ("other" . (:foreground "GoldenRod" :weight bold))

        ("paper-reading" . (:foreground "IndianRed1" :weight bold))   
        ("coding" . (:foreground "IndianRed1" :weight bold))

        ("URGENT" . (:foreground "Red" :weight bold))  

        ("EASY" . (:foreground "OrangeRed" :weight bold))  
        ("MEDIUM" . (:foreground "OrangeRed" :weight bold))  
        ("HARD" . (:foreground "OrangeRed" :weight bold))  
        )
      )

;;; Code blocks
;; evaluable codes in org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (emacs-lisp . t)
   (org . t)
   (sh . t)
   (C . t)
   (python . t)
   (gnuplot . t)
   (octave . t)
   (R . t)
   (dot . t)
   (awk . t)
   ))

(setq org-src-fontify-natively t) ; fontify code in code blocks
(setq org-src-tab-acts-natively t) ; tab in a block work as in the language major mode buffer


;;; ORG Capture
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)

;; set a bookmark of current location and jump to the latest captured item
;; NOTE: need to be interactive command
(define-key global-map "\C-cj" (lambda () (interactive)
				 (bookmark-set "org-capture-last-jump-from" t)
				 (org-capture '(16))
				 ))
;; jump back after editing the captured item
(define-key global-map "\C-cb" (lambda () (interactive)
				 (bookmark-jump "org-capture-last-jump-from")
				 (bookmark-delete "org-capture-last-jump-from")
				 ))

;(defun org-capture-set-tags()
;(completing-read "Tag: " (mapcar #'first org-tag-persistent-alist) nil t))

(setq org-capture-templates
      '(("t"               ; hotkey
	 "TODO list item"  ; name
	 entry             ; type
	 (file+datetree "~/org/captured.org")
	 (file "~/.emacs.d/org-templates/todo.orgcaptmpl"))
	;; %a means Annotation (org-store-link); %i active region; %? where cursor ends up
	("j"
	 "Journal entry"
	 entry
	 (file+datetree "~/org/journal.org")
	 (file "~/.emacs.d/org-templates/journal.orgcaptmpl")) ;; template in file
	("n"
	 "Note entry"
	 entry
	 (file+datetree "~/org/notes.org")
	 (file "~/.emacs.d/org-templates/note.orgcaptmpl")) ;; template in file
	))


;;;; -- Configure pdf-tools (only on emacs on display grpahic window system) --
;; note that pdf-tools choke emacs with (global-linum-mode 1)
;; so only use it when version > 26.0.0
;; when `global-display-line-numbers-mode` is available
(cond ((and (display-graphic-p) (version<= "26.0.0" emacs-version))
       (use-package pdf-tools
	 :ensure t ;; auto-download from elpa
	 :pin manual ;; manually update
	 :config
	 ;; initialise
	 (pdf-tools-install)
	 ;; open pdfs scaled to fit page
	 (setq-default pdf-view-display-size 'fit-page)
	 ;; automatically annotate highlights
	 (setq pdf-annot-activate-created-annotations t)
	 use normal isearch
	 (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))
       ))

;;;;; --- End ---

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (tango-dark)))
 '(package-selected-packages (quote (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
