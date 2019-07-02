;;;;; --- Start ---
;;;; -- Setup package.el, and bootstrap `use-package' --
(require 'package)
(setq package-enable-at-startup nil)

;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; https://mirror.tuna.tsinghua.edu.cn/help/elpa/
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("marmalade" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/marmalade/")))

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
(when (display-graphic-p) (toggle-scroll-bar -1)) ;; available only on graphic display window system


;;;; -- Misc --
;; Not a fan of backup files
(setq make-backup-files nil)

;; Permit narrow-to-region
(put 'narrow-to-region 'disabled nil)


;;;; -- Flymake for Python: external tool: pylint --
;; pip install pylint first: https://docs.pylint.org/en/1.6.0/ide-integration.html
;; flymake is completely refactored since Emacs 26.0.
;; and flymake-proc is compatible with the old API. a lot of names are changed for the legacy backend (add `-proc-')
;; can check the code for the changes
;; https://github.com/emacs-mirror/emacs/blob/d0e2a341dd9a9a365fd311748df024ecb25b70ec/lisp/progmodes/flymake-proc.el
;; https://github.com/emacs-mirror/emacs/blob/ee3e432300054ca488896e39fca57b10d733330a/lisp/progmodes/flymake.el
(use-package "flymake")
(load "flymake")
(cond ((version<= "26.0.0" emacs-version)
       (defun flymake-pylint-init () ;; return the cmdline to execute
	 (let* ((temp-file (flymake-proc-init-create-temp-buffer-copy
			    'flymake-proc-create-temp-inplace))
		(local-file  (file-relative-name
			      temp-file
			      (file-name-directory buffer-file-name))))
	   (list "epylint" (list local-file)))) ;; call `epylint' external cmdline
       (add-to-list 'flymake-proc-allowed-file-name-masks
		    '("\\.py\\'" flymake-pylint-init))
       ;; `flymake-diagnostic-at-point-mode' will use `post-command-hook' to displaying flymake diagnostics at point
       (add-hook 'flymake-mode-hook '(lambda () (flymake-diagnostic-at-point-mode)))
       ;; display diagnostic messages in minibuffer rathert than pop out
       (setq flymake-diagnostic-at-point-display-diagnostic-function
	     'flymake-diagnostic-at-point-display-minibuffer)

       ;; legacy is the default backend, do not need to specificy
       ;; (setq flymake-diagnostic-functions '(flymake-proc-legacy-flymake))
       )
      (t ;; before version 26.0, the old flymake implementation!
       (defun flymake-pylint-init ()
	 (let* ((temp-file (flymake-init-create-temp-buffer-copy
			    'flymake-create-temp-inplace))
		(local-file (file-relative-name
			     temp-file
			     (file-name-directory buffer-file-name))))
	   (list "epylint" (list local-file)))) ;; call `epylint' external cmdline
       (add-to-list 'flymake-allowed-file-name-masks
		    '("\\.py\\'" flymake-pylint-init))
       ;; To avoid having to mouse hover for the error message, these functions make flymake error messages
       ;; appear in the minibuffer
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
       ))

;; Set as a minor mode for Python
(add-hook 'python-mode-hook '(lambda () (flymake-mode)))

;; Configure to wait a bit longer after edits before starting
(setq-default flymake-no-changes-timeout '1)

;; Keymaps to navigate to the errors
(add-hook 'flymake-mode-hook '(lambda () (define-key python-mode-map "\C-cn" 'flymake-goto-next-error)))
(add-hook 'flymake-mode-hook '(lambda () (define-key python-mode-map "\C-cp" 'flymake-goto-prev-error)))

;;;; -- Python jedi autocompletion --
;; `C-c .` goto-definition; `C-c d` show-doc
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'jedi:setup)

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
;; make the section marked by ** use foreground red; // use foreground yellow
(add-to-list 'org-emphasis-alist
             '("*" (:foreground "red"))
	     )
(add-to-list 'org-emphasis-alist
             '("/" (:foreground "yellow"))
	     )

;;; Export
(setq org-export-coding-system 'utf-8)
(setq org-export-html-validation-link nil)

;;; TODO keywords, tags settings
(setq org-todo-keywords
      '((sequence "IDEA(i)" "TODO(t)" "STARTED(s)" "RUNNING(r)" "WAITING(w)" "HOLD(h)" "LONG-TERM(l)" "|" "DONE(d)" "ABANDONED(a)")))

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
	("discussion" . ?d)
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
	("discussion" . (:foreground "IndianRed1" :weight bold))

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
   ;; (sh . t) ; strange, cannot find ob-shell
   (C . t)
   (python . t)
   (dot . t)
   (awk . t)
   ))

(setq org-src-fontify-natively t) ; fontify code in code blocks
(setq org-src-tab-acts-natively t) ; tab in a block work as in the language major mode buffer


;;; ORG Capture
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)
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

;(defun org-capture-set-tags ()
;(completing-read "Tag: " (mapcar #'first org-tag-persistent-alist) nil t))

;; ask for a paper pdf file, use `pdfinfo` shell command to get the tile and authors info
;; used in paper_reading capture template
;; Note: many pdf files's title/author metadata cannot be found by pdfinfo... let's try exiftool
(defun ask-for-paper-pdf ()
  (interactive)
  (let*
      ((filename (read-file-name "Paper pdf:"
				 "/Users/foxfi/Documents/research/papers/"
				 nil
				 nil
				 "/Users/foxfi/Documents/research/papers/"))
       (title (string-remove-suffix "\n" (shell-command-to-string
	       (concat "pdfinfo " (shell-quote-argument filename)
		       " | grep -i title | awk 'BEGIN {FS = \":[ ]+\"} {print $NF}'"))))
       (authors (string-remove-suffix "\n" (shell-command-to-string
		 (concat "pdfinfo " (shell-quote-argument filename)
			 " | grep -i author | awk 'BEGIN {FS = \":[ ]+\"} {print $NF}'")))))
    (concat "Title: " title "\n/Authors/: " authors "\n[[file:" filename "]]")))

;; to conveniently insert paper-reading capture template at point (for paper reading)
(defun org-paper-reading-at-point ()
  "Insert an paper-reading capture template at point."
  (interactive)
  (org-capture 0 "p"))

(define-key org-mode-map (kbd "C-c p") 'org-paper-reading-at-point)

(setq org-capture-templates
      '(("t"               ; hotkey
	 "TODO list item"  ; name
	 entry             ; type
	 (file+datetree "~/org/captured.org")
	 (file "~/.emacs.d/org-templates/todo.orgcaptmpl") ;; template in file
	 :empty-lines 1) ;; empty lines to insert before/after new item
	;; %a means Annotation (org-store-link); %i active region; %? where cursor ends up
	("j"
	 "Journal entry"
	 entry
	 (file+datetree "~/org/journal.org")
	 (file "~/.emacs.d/org-templates/journal.orgcaptmpl")
	 :empty-lines 1)
	("r"
	 "Week report"
	 entry
	 (file+datetree "~/org/reports.org")
	 (file "~/.emacs.d/org-templates/reports.orgcaptmpl")
	 :tree-type 'week
	 ;;:headline-levels 1
	 :empty-lines 1)
	("d"
	 "Daily report"
	 entry
	 (file+datetree "~/org/daily.org")
	 (file "~/.emacs.d/org-templates/daily.orgcaptmpl")
	 :empty-lines 1)
	("n"
	 "Note entry"
	 entry
	 (file+datetree "~/org/notes.org")
	 (file "~/.emacs.d/org-templates/note.orgcaptmpl")
	 :empty-lines 1)
	("p"
	 "Paper reading"
	 entry
	 (file+headline "~/org/paper_reading.org" "Papers")
	 (file "~/.emacs.d/org-templates/paper_reading.orgcaptmpl")
	 :empty-lines 1)
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
	 ;; use normal isearch
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
 '(org-confirm-babel-evaluate nil)
 '(package-selected-packages (quote (elpy pylint flymake-cursor use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
