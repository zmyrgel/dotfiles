;;; init-text.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - Text-related settings

;;; Code:

;; whitespace mode
(setopt whitespace-line-column 80)
(setopt whitespace-style '(face lines-tail trailing))
(setopt whitespace-global-modes '(not agent-shell-mode magit-status-mode magit-diff-mode))
(global-whitespace-mode)

;; grep
(setopt grep-find-use-xargs 'exec-plus)

;; find-dired results with human readable sizes
;; (find-ls-option '("-exec ls -ldh {} +" . "-ldh"))
(setopt grep-find-ignored-directories
        '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr"
          "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist"))

;; rg stuff for evaluation
(when (and nil (executable-find "rg"))
  (setopt xref-search-program 'ripgrep)
  (grep-apply-setting
   'grep-find-template
   "find <D> <X> -type f <F> -exec rg <C> --no-heading -H  <R> /dev/null {} +")
  (grep-apply-setting
   'grep-template
   "rg --no-heading -H -uu -g <F> <R> <D>")
  (grep-apply-setting
   'grep-find-command
   '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27))
  (grep-apply-setting
   'grep-command
   "rg -nS --no-heading "))

;; TODO: This needed?
;; (add-to-list 'grep-find-ignored-directories "node_modules")
;; (add-to-list 'grep-find-ignored-directories "build")
;; (add-to-list 'grep-find-ignored-directories "dist")

;; | {C-x r t} string-rectangle

;; electric
(setopt electric-pair-skip-whitespace 'chomp)
(add-hook 'after-init-hook 'electric-pair-mode)
(add-hook 'after-init-hook 'electric-indent-mode)

;; use print helper
(when (executable-find "gtklp")
  (setopt lpr-command "gtklp"))

;; spelling
(setopt flyspell-issue-message-flag nil)
(setopt flyspell-issue-welcome-flag nil)
(setopt ispell-program-name
        (or (executable-find "enchant-2")
            (executable-find "aspell")
            (executable-find "ispell")
            (executable-find "hunspell")))
(setopt ispell-dictionary "american")
(setopt flyspell-check-changes t)
(add-hook 'text-mode-hook 'flyspell-mode)

;;; doc-view
(setopt doc-view-mupdf-use-svg t)
(setopt doc-view-resolution 120)

(add-to-list 'magic-mode-alist '("---" . yaml-ts-mode))

;; Any file start with xml will be treat as nxml-mode
(add-to-list 'magic-mode-alist '("<\\?xml" . nxml-mode))
(dolist (p '("\\.plist\\'"
             "\\.rss\\'"
             "\\.svg\\'"
             "\\.xml\\'"
             "\\.xsd\\'"
             "\\.xslt\\'"
             "\\.pom\\'"))
  (add-to-list 'auto-mode-alist `(,p . nxml-mode)))

(defun my/xml-pretty-print (begin end)
  "Pretty-print the XML markup in selected region."
  (interactive "r")
  ;; TODO: doctype causes problem
  (if-let* ((xmlstarlet-cmd (or (and (eq system-type 'berkeley-unix)
                                     (executable-find "xml"))
                                (executable-find "xmlstarlet"))))
      (shell-command-on-region
       begin
       end
       (concat xmlstarlet-cmd " fo -s 2")
       nil
       'no-mark)
    (error "failed to find `xmlstarlet' program, please install it.")))

(setq nxml-slash-auto-complete-flag t)

;;(add-to-list rng-schema-locating-files "~/xml-schemas")

;; { C-c C-s C-w } show current schema
;; { C-c C-s C-f } find schema for file

;; Use nxml-mode instead of sgml, xml or html mode.
(mapc
 (lambda (pair)
   (if (or (eq (cdr pair) 'xml-mode)
           (eq (cdr pair) 'sgml-mode))
       (setcdr pair 'nxml-mode)))
 auto-mode-alist)

(defun pulse-line ()
  "Pulse the current line."
  (interactive)
  (pulse-momentary-highlight-one-line))

;;(add-hook 'window-state-change-hook #'pulse-line)

;;; CONF
(add-to-list 'auto-mode-alist '("\\.env\\'" . conf-mode))

;;; tex
;; | Key chord    | Description     |
;; |--------------+-----------------|
;; | { C-c [ }    | add cite        |
;; | { C-c = }    | show toc        |

(ensure-packages-present 'auctex)
(load "auctex.el" nil t t)
(load "preview.el" nil t t)

(add-to-list 'major-mode-remap-alist '(TeX-mode . latex-mode))

(add-hook 'latex-mode-hook 'auto-fill-mode)
(add-hook 'latex-mode-hook 'reftex-mode)
(add-hook 'tex-mode-hook (lambda () (setq ispell-parser 'tex)))

;; init
;; these two do not match custom type
;; (setopt TeX-view-program-selection
;;         '(((output-dvi has-no-display-manager) "dvi2tty")
;;           ((output-dvi style-pstricks) "dvips and gv")
;;           (output-dvi "xdvi")
;;           (output-pdf "pdf-tools")oooo

(setopt TeX-command-extra-options "-shell-escape -8bit")
(setopt TeX-auto-save t)
(setopt TeX-parse-self t)
(setopt TeX-insert-braces nil)
(setopt TeX-electric-escape t)
(setopt TeX-auto-untabify t)
(setopt TeX-newline-function 'reindent-then-newline-and-indent)
(add-hook 'TeX-after-compilation-finished-functions 'TeX-revert-document-buffer)

;; doc-view / doc-view-presentation
(ensure-packages-present 'pdf-tools)
(require 'pdf-tools nil t)
(add-to-list 'magic-mode-alist '("%PDF" . pdf-view-mode))
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
(add-hook 'pdf-view-mode 'pdf-links-minor-mode)
(add-hook 'pdf-view-mode 'pdf-isearch-minor-mode)
(add-hook 'pdf-view-mode 'pdf-outline-minor-mode)
(add-hook 'pdf-view-mode 'pdf-history-minor-mode)

(setopt pdf-view-display-size 'fit-page)
(pdf-tools-install :no-query :skip-deps :no-error)

(ensure-packages-present 'bibliothek)
(setopt bibliothek-path '("~/Documents"))
(setopt bibliothek-recursive t)

(provide 'init-text)

;; init-text.el ends here
