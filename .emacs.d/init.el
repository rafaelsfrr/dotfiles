; Path
(add-to-list 'load-path "~/.emacs.d/powerline")
(add-to-list 'load-path "~/.emacs.d/undo-tree")

; Font size
;; Set default font
(set-face-attribute 'default nil
                    :family "Cascadia Code"
                    :height 180
                    :weight 'normal
                    :width 'normal)

;; Disable menu bar and tool bar
(menu-bar-mode -1)
(tool-bar-mode -1)

; Backup files
(setq backup-directory-alist '(("." . "/tmp"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

;; Reduce the lag
(setq auto-window-vscroll nil)

;; Paren mode
(show-paren-mode 1)
(setq show-paren-delay 0)

; Global keybinds
(global-set-key (kbd "M-p f") 'helm-projectile-find-file)
(global-set-key (kbd "M-p a g") 'helm-projectile-ag)
(global-set-key (kbd "M-g s") 'magit-status)
(global-set-key (kbd "M-g k") 'lispy-raise-sexp)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

; Wind move
(when (fboundp 'windmove-default-keybindings)
(windmove-default-keybindings))

; Straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; packages
(straight-use-package 'helm)
(straight-use-package 'helm-ag)
(straight-use-package 'doom-themes)
(straight-use-package 'use-package)
(straight-use-package 'projectile)
(straight-use-package 'magit)
(straight-use-package 'cider)
(straight-use-package 'lispy)
(straight-use-package 'helm-projectile)
(straight-use-package 'flycheck-clj-kondo)
(straight-use-package 'company)
(straight-use-package 'multiple-cursors)
(straight-use-package 'move-text)
(straight-use-package 'all-the-icons)
(straight-use-package 'emojify)
(straight-use-package 'haskell-mode)
(straight-use-package 'elpy)
(straight-use-package 'yaml-mode)
(straight-use-package 'terraform-mode)
(straight-use-package 'company-terraform)
(straight-use-package 'org-gcal)
(straight-use-package 'org-journal)
(straight-use-package 'olivetti)

;;; Doom-themes
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-solarized-dark t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;; Helm
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))


(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(helm-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
(global-set-key (kbd "C-x C-f") 'helm-find-files)


;;; Powerline
(require 'powerline)
(powerline-default-theme)

;;; Helm-projectile
(require 'helm-projectile)
(helm-projectile-on)

;;; Lispy
(require 'lispy)
(setq lispy-compat '(edebug cider magit-blame-mode))

;; clj-kondo
(require 'flycheck-clj-kondo)

;; Undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;; Multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; Move-text
(require 'move-text)
(move-text-default-bindings)

;; elpy
(setq python-shell-interpreter "python3"
      python-shell-interpreter-args "-i")

;; Org mode
(setq org-todo-keywords
      '((sequence "TODO(a@/!)" "DOING(b@/!)" "REVIEW(c@/!)" "TEST(d@/!)" "|" "CANCELED(e@/!)" "DONE(f@/!)")
	(sequence "TODO(g@)" "DOING(h@)" "|" "CANCELED(i@)" "DONE(j@)")))

(setq org-indent-indentation-per-level 1)
(setq org-adapt-indentation nil)
(setq org-hide-leading-starts 't)
(setq org-hide-emphasis-markers t)
(customize-set-variable 'org-blank-before-new-entry 
                        '((heading . nil)
                          (plain-list-item . nil)))
(setq org-cycle-separator-lines 1)


;; gcal
(require 'org-gcal)
(setq org-gcal-client-id (substitute-in-file-name "$GOOGLE_CLIENT_ID")
      org-gcal-client-secret (substitute-in-file-name "$GOOGLE_CLIENT_SECRET")
      org-gcal-file-alist '(("rafael.sobfer@gmail.com" .  "~/Dropbox/org/gcal.org")))

;; org-journal
(require 'org-journal)
(setq org-journal-dir "~/Dropbox/org/journal")


;; org-capture
(setq org-default-notes-file (concat org-directory "~/Dropbox/org/notes.org"))
(setq org-capture-templates
      '(("t" "General Todo" entry (file+headline "~/Dropbox/org/todo.org" "Tasks")
         "** TODO %? \nSCHEDULED: <%(org-read-date nil nil)>")
	("c" "Project Casa: Todo" entry (file+olp "~/Dropbox/org/todo.org" "Projects" "Casa")
	 "*** TODO %? \nSCHEDULED: <%(org-read-date nil nil)>")
	("w" "Project Trabalho: Todo" entry (file+olp "~/Dropbox/org/todo.org" "Projects" "Trabalho")
	 "*** TODO %? \nSCHEDULED: <%(org-read-date nil nil)>")
	("i" "Inbox" entry (file+headline "~/Dropbox/org/todo.org" "Inbox")
	 "** %?")
	("r" "Daily review" checkitem (file+headline "~/Dropbox/org/dailyreview.org" "Daily Review")
         "%U \n[ ] Sync gcal \n[ ] Review de tarefas do dia anterior \n[ ] Reschedule de tarefas do dia anterior \n[ ] Priorização de tarefas do dia \n[ ] Processamento do inbox \n[ ] Review de tarefas \n[ ] Journal do dia anterior %?")
        ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
         "* %?\nEntered on %U\n  %i\n")))

; Hooks
(add-hook 'clojure-mode-hook
	  (lambda ()
	    (setq cider-prefer-local-resources t
		  cider-repl-pop-to-buffer-on-connect nil)
	    (dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
	      (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))
	    (flycheck-mode)
	    (lispy-mode)
	    (company-mode)
	    (add-hook 'after-save-hook
		      (lambda ()
			(when (string= (file-name-extension buffer-file-name) "clj")
			  (cider-load-buffer))))))
	  
(add-hook 'clojurescript-mode-hook
	  (lambda ()
	    (setq cider-prefer-local-resources t
		  cider-repl-pop-to-buffer-on-connect nil)
	    (dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
	      (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))
	    (flycheck-mode)
	    (lispy-mode)
	    (company-mode)
	    (add-hook 'after-save-hook
		      (lambda ()
	    		(when (string= (file-name-extension buffer-file-name) "cljs")
			  (cider-load-buffer))))))

(add-hook 'cider-repl-mode-hook
	  (lambda ()
	    (company-mode)
	    (lispy-mode)))

(add-hook 'python-mode-hook
	  (lambda ()
	    (company-mode)
	    (elpy-mode)))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (lispy-mode)))

(add-hook 'org-mode-hook
	  (lambda ()
	    (company-mode)
	    (olivetti-mode)
	    (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t)))

(add-hook 'org-agenda-mode-hook (lambda ()
				  (org-gcal-sync)))
(add-hook 'org-capture-after-finalize-hook (lambda ()
					     (org-gcal-sync)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/todo.org" "~/Dropbox/org/gcal.org" "~/Dropbox/org/dailyreview.org")))
 '(safe-local-variable-values
   (quote
    ((eval setq flycheck-clj-kondo-edn-executable
	   (concat
	    (projectile-project-root)
	    "/share/bin/clj-kondo"))
     (eval setq flycheck-clj-kondo-cljc-executable
	   (concat
	    (projectile-project-root)
	    "/share/bin/clj-kondo"))
     (eval setq flycheck-clj-kondo-cljs-executable
	   (concat
	    (projectile-project-root)
	    "/share/bin/clj-kondo"))
     (eval setq flycheck-clj-kondo-clj-executable
	   (concat
	    (projectile-project-root)
	    "/share/bin/clj-kondo"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
