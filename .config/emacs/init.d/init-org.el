;;; init-org.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - Org-mode related options

;;; Code:

(setq org-list-allow-alphabetical t)

(with-eval-after-load 'org
  (setq org-directory "~/Documents/OrgFiles")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-agenda-files (list org-directory))
  (setq org-agenda-file-regexp "\\(school\\|todo\\|work\\)\\.org")
  (setq org-outline-path-complete-in-steps nil)
  (setq org-insert-mode-line-in-empty-file t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-enforce-todo-dependencies t)
  (setq org-log-done 'note)
  (setq org-startup-indented t)
  (setq org-todo-keywords '((sequence "TODO(t)" "WIP(w!)" "|" "DONE(d@!)")
                            (sequence "|" "CANCELED(c@/!)")
                            (sequence "STALLED(s@/!)" "|")
                            (sequence "PENDING(p@/!)" "|")))
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-track-ordered-property-with-tag t)
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?B)
  (setq org-tag-alist ;; use these or set file tags?
	'(("work" . ?w)
          ("emacs" . ?e)
          ("study" . ?s)
          ("mail" . ?m)))
  (setq org-confirm-babel-evaluate t)
  (setq org-log-done 'note)
  (setq org-log-note-clock-out t)
  (setq org-read-date-prefer-future t)
  (setq org-adapt-indentation nil)
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  (setq org-hide-emphasis-markers nil)
  (setq org-hide-leading-stars nil)
  (setq org-catch-invisible-edits 'show)
  (setq org-return-follows-link nil)
  (setq org-loop-over-headlines-in-active-region 'start-level)
  (setq org-imenu-depth 3)
  ;; allow shell execution
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (emacs-lisp . t)))
  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'message-mode-hook 'turn-on-orgtbl)

  (define-key org-mode-map (kbd "C-M-i") 'completion-at-point)

  ;; Use fixed-pitch font to keep tables aligned
  (defun set-buffer-variable-pitch ()
    (interactive)
    (variable-pitch-mode t)
    (setq line-spacing 3)
    (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-block nil :inherit 'fixed-pitch))

  (add-hook 'org-mode-hook 'set-buffer-variable-pitch)
  (add-hook 'eww-mode-hook 'set-buffer-variable-pitch)
  (add-hook 'markdown-mode-hook 'set-buffer-variable-pitch)
  (add-hook 'Info-mode-hook 'set-buffer-variable-pitch)

  ;;  ol
  (setq org-link-keep-stored-after-insertion t)
  (define-key org-mode-map (kbd "C-c L") 'org-toggle-link-display)
  (define-key org-mode-map (kbd "C-c C-y") 'org-insert-last-stored-link)

  ;;  org-capture
  (let ((todo-template (concat "* TODO %^{Title}\n"
                               ":PROPERTIES:\n"
                               ":CAPTURED: %U\n"
                               ":END:\n\n"
                               "%i%l")))
    ;; "* TODO %?\n  %i\n  %a"
    (setq org-capture-templates
	  `(("t" "Todo" entry (file+headline "todo.org" "Tasks")
             ,todo-template)
            ("s" "Study" entry (file+headline "study.org" "Study stuff")
             ,todo-template)
            ("w" "Work tasks" entry (file+headline "work.org" "Work tasks")
             ,todo-template)
            ("n" "Notes" entry (file+datetree "notes.org")
             "* %?\nEntered on %U\n  %i\n  %a")
            ("j" "Journal" entry (file+datetree "journal.org")
             "* %?\nEntered on %U\n  %i\n  %a"))))

  (setq org-capture-templates-contexts
	'(("r" ((in-mode . "gnus-article-mode")
		(in-mode . "gnus-summary-mode")))))

  ;; org-agenda
  (setq org-agenda-span 'week)
  (setq org-agenda-start-on-weekday 1)  ; Monday
  (setq org-agenda-confirm-kill t)
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-show-outline-path nil)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-custom-commands-contexts nil)
  (setq org-agenda-insert-diary-strategy 'date-tree)
  (setq org-agenda-insert-diary-extract-time t)
  (setq org-agenda-include-diary t)
  (setq org-agenda-follow-indirect t)
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-todo-list-sublevels t)
  (setq org-agenda-include-deadlines t)
  (setq org-deadline-warning-days 7)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-agenda-skip-timestamp-if-deadline-is-shown t)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 1)
  (setq org-agenda-time-leading-zero t)
  (setq org-agenda-timegrid-use-ampm nil)

  ;; org-src
  (setq org-src-tab-acts-natively t)
  (setq org-src-window-setup 'current-window)
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-edit-src-content-indentation 0)

  ;; ox
  (setq org-export-with-toc t)
  (setq org-export-headline-levels 3)
  (setq org-export-dispatch-use-expert-ui nil)

  ;; ox-latex
  (setq org-latex-pdf-process
	'("latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f"))

  ;; ox-publish
  (setq org-publish-project-alist
	'(("blog"
           :base-directory "~/Documents/OrgFiles/blog/posts/"
           :base-extension "org"
           :publishing-directory "/ssh:tmy@mars.bittivirhe.fi:public/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :auto-sitemap t)
          ("all" :components ("blog")))))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
	       '("IEEEtran" "\\documentclass[11pt]{IEEEtran}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
	       t)
  (add-to-list 'org-latex-classes
	       '("koma-article" "\\documentclass{scrartcl}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
	       t))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)

(ensure-packages-present '(visual-fill-column))

(setq visual-fill-column-width 110)
(setq visual-fill-column-center-text t)

(ensure-packages-present '(org-roam org-present org-ref))

;; left/right for movement
;; { C-c C-= } for large txt
;; { C-c C-- } for small text
;; { C-c C-q } for quit (which will return you back to vanilla org-mode)
;; { C-c < } and { C-c > } to jump to first/last slide
;; { C-c C-r } for buffer read-only
;; { C-c C-w } for buffer read/write
;; { C-c C-1 } for one big page showing all slides

(with-eval-after-load 'org-present
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-present-big)
              (org-display-inline-images)
              (org-present-hide-cursor)
              (org-present-read-only)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images)
              (org-present-show-cursor)
              (org-present-read-write))))

(defun my/org-present-prepare-slide (buffer-name heading)
  ;; Show only top-level headlines
  (org-overview)

  ;; Unfold the current entry
  (org-show-entry)

  ;; Show only direct subheadings of the slide but don't expand them
  (org-show-children))

(defun my/org-present-start ()
  ;; Tweak font sizes
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.0) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))

  ;; Set a blank header line string to create blank space at the top
  (setq header-line-format " ")

  ;; Display inline images automatically
  (org-display-inline-images)

  ;; Center the presentation and wrap lines
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(defun my/org-present-end ()
  ;; Reset font customizations
  (setq-local face-remapping-alist '((default variable-pitch default)))

  ;; Clear the header line string so that it isn't displayed
  (setq header-line-format nil)

  ;; Stop displaying inline images
  (org-remove-inline-images)

  ;; Stop centering the document
  (visual-fill-column-mode 0)
  (visual-line-mode 0))

;; fixed-pitch font Jetbrains Mono, ligth
;; varible-pitch font Iosevka Aile light
;; colortheme: doom-palenight doom-themes

;; Register hooks with org-present
(add-hook 'org-present-mode-hook 'my/org-present-start)
(add-hook 'org-present-mode-quit-hook 'my/org-present-end)
(add-hook 'org-present-after-navigate-functions 'my/org-present-prepare-slide)

;; Load org-faces to make sure we can set appropriate faces
(require 'org-faces)

;; Resize Org headings
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font *my-variable-font* :weight 'medium :height (cdr face)))

;; Make the document title a bit bigger
(set-face-attribute 'org-document-title nil :font *my-variable-font* :weight 'bold :height 1.3)

;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
(set-face-attribute 'org-block nil :foreground 'unspecified :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(setq org-roam-v2-ack t)

(let ((roam-dir "~/Documents/OrgRoamNotes"))
  (unless (file-directory-p roam-dir)
    (make-directory roam-dir))
  (setq org-roam-directory "~/Documents/OrgRoamNotes"))

(setq org-roam-completion-everywhere t)

(define-key global-map (kbd "C-c n l") 'org-roam-buffer-toggle)
(define-key global-map (kbd "C-c n f") 'org-roam-node-find)
(define-key global-map (kbd "C-c n i") 'org-roam-node-insert)

(require 'org-roam-dailies) ;; Ensure the keymap is available

(define-key global-map (kbd "C-c n d") 'org-roam-dailies-map)
(define-key org-roam-dailies-map "Y" 'org-roam-dailies-capture-yesterday)
(define-key org-roam-dailies-map "T" 'org-roam-dailies-capture-tomorrow)

(org-roam-db-autosync-mode)

(provide 'init-org)

;; init-org.el ends here
