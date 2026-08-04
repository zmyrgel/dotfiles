;;; init-org.el --- Org-mode related setup -*- lexical-binding: t -*-

;; Copyright (c) 2022-2026 Timo Myyrä <timo.myyra@bittivirhe.fi>

;; Author: Timo Myyrä <timo.myyra@bittivirhe.fi>
;; URL: https://github.com/zmyrgel/dotfiles
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; - Org-mode related options
;; - check: https://macowners.club/posts/personal-touch-org-agenda/

;;; Code:

(setopt org-list-allow-alphabetical t)

(with-eval-after-load 'org
  (setopt org-directory "~/Documents/OrgFiles")
  (setopt org-default-notes-file (concat org-directory "/notes.org"))
  (setopt org-agenda-files (list org-directory))
  (setopt org-agenda-file-regexp "\\`[^.].*\\.org\\'")
  (setopt org-outline-path-complete-in-steps nil)
  (setopt org-insert-mode-line-in-empty-file t)
  (setopt org-enforce-todo-checkbox-dependencies t)
  (setopt org-enforce-todo-dependencies t)
  (setopt org-log-done 'note)
  (setopt org-startup-indented t)
  (setopt org-todo-keywords '((sequence "TODO(t)" "WIP(w!)" "|" "DONE(d@!)")
                              (sequence "|" "CANCELED(c@/!)")
                              (sequence "STALLED(s@/!)" "|")
                              (sequence "PENDING(p@/!)" "|")))
  (setopt org-fontify-quote-and-verse-blocks t)
  (setopt org-track-ordered-property-with-tag t)
  (setopt org-highest-priority ?A)
  (setopt org-lowest-priority ?C)
  (setopt org-default-priority ?B)
  (setopt org-tag-alist ;; use these or set file tags?
          '(("work" . ?w)
            ("emacs" . ?e)
            ("research" . ?r)
            ("mail" . ?m)))
  (setopt org-confirm-babel-evaluate t)
  (setopt org-log-done 'note)
  (setopt org-log-note-clock-out t)
  (setopt org-read-date-prefer-future t)
  (setopt org-adapt-indentation nil)
  (setopt org-special-ctrl-a/e t)
  (setopt org-special-ctrl-k t)
  (setopt org-hide-emphasis-markers nil)
  (setopt org-hide-leading-stars nil)
  (setopt org-fold-catch-invisible-edits 'show)
  (setopt org-return-follows-link nil)
  (setopt org-loop-over-headlines-in-active-region 'start-level)
  (setopt org-imenu-depth 3)
  ;; allow shell execution
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (emacs-lisp . t)))
  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'message-mode-hook 'turn-on-orgtbl)

  (keymap-set org-mode-map "C-M-i" 'completion-at-point)

  ;; Use fixed-pitch font to keep tables aligned
  (defun set-buffer-variable-pitch ()
    (interactive)
    (variable-pitch-mode t)
    (setq-local line-spacing 3)
    (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-block nil :inherit 'fixed-pitch))

  (add-hook 'org-mode-hook 'set-buffer-variable-pitch)
  (add-hook 'eww-mode-hook 'set-buffer-variable-pitch)
  (add-hook 'markdown-mode-hook 'set-buffer-variable-pitch)
  (add-hook 'Info-mode-hook 'set-buffer-variable-pitch)

  ;;  ol
  (setopt org-link-keep-stored-after-insertion t)
  (keymap-set org-mode-map "C-c L" 'org-toggle-link-display)
  (keymap-set org-mode-map "C-c C-y" 'org-insert-last-stored-link)

  ;;  org-capture
  (let ((todo-template (concat "* TODO %^{Title}\n"
                               ":PROPERTIES:\n"
                               ":CAPTURED: %U\n"
                               ":END:\n\n"
                               "%i%l")))
    ;; "* TODO %?\n  %i\n  %a"
    (setopt org-capture-templates
            `(("t" "Todo" entry (file+headline "todo.org" "Tasks")
               ,todo-template)
              ("s" "Study" entry (file+headline "research.org" "Research subjects")
               ,todo-template)
              ("w" "Work tasks" entry (file+headline "work.org" "Work tasks")
               ,todo-template)
              ("n" "Notes" entry (file+olp+datetree "notes.org" "Misc Notes")
               "* %?\nEntered on %U\n  %i\n  %a")
              ("j" "Journal" entry (file+olp+datetree "journal.org" "Journal Entries")
               "* %?\nEntered on %U\n  %i\n  %a"))))

  (setopt org-capture-templates-contexts
          '(("r" ((in-mode . "gnus-article-mode")
                  (in-mode . "gnus-summary-mode")))))

  ;; org-agenda
  (setopt org-agenda-span 'week)
  (setopt org-agenda-start-on-weekday 1)  ; Monday
  (setopt org-agenda-confirm-kill t)
  (setopt org-agenda-show-all-dates t)
  (setopt org-agenda-show-outline-path nil)
  (setopt org-agenda-window-setup 'current-window)
  (setopt org-agenda-custom-commands-contexts nil)
  (setopt org-agenda-insert-diary-strategy 'date-tree)
  (setopt org-agenda-insert-diary-extract-time t)
  (setopt org-agenda-include-diary t)
  (setopt org-agenda-follow-indirect t)
  (setopt org-agenda-dim-blocked-tasks t)
  (setopt org-agenda-todo-list-sublevels t)
  (setopt org-agenda-include-deadlines t)
  (setopt org-deadline-warning-days 7)
  (setopt org-agenda-skip-scheduled-if-done t)
  (setopt org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setopt org-agenda-skip-timestamp-if-deadline-is-shown t)
  (setopt org-agenda-skip-deadline-prewarning-if-scheduled 1)
  (setopt org-agenda-time-leading-zero t)
  (setopt org-agenda-timegrid-use-ampm nil)

  ;; org-src
  (setopt org-src-tab-acts-natively t)
  (setopt org-src-window-setup 'current-window)
  (setopt org-src-fontify-natively t)
  (setopt org-src-preserve-indentation t)
  (setopt org-edit-src-content-indentation 0)

  ;; ox
  (setopt org-export-with-toc t)
  (setopt org-export-headline-levels 3)
  (setopt org-export-dispatch-use-expert-ui nil)

  ;; ox-latex
  (setopt org-latex-pdf-process
          '("latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f"))

  ;; ox-publish
  (setopt org-publish-project-alist
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

(keymap-global-set "C-c a" 'org-agenda)
(keymap-global-set "C-c l" 'org-store-link)
(keymap-global-set "C-c c" 'org-capture)

(provide 'init-org)

;;; init-org.el ends here
