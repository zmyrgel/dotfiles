;;; init-text.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - Text-related settings

;;; Code:

;; grep
(setq grep-find-use-xargs 'exec-plus)

;; rg stuff for evaluation
(when (and nil (executable-find "rg"))
  (grep-apply-setting
   'grep-find-template
   "find <D> <X> -type f <F> -exec rg <C> --no-heading -H  <R> /dev/null {} +")
  (grep-apply-setting
   'grep-template
   "rg --no-heading -H -uu -g <F> <R> <D>")
  (grep-apply-setting
   'grep-find-command
   '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27)))

;; * {C-c C-e}: Apply the changes to file buffers.
;; * {C-c C-u}: All changes are unmarked and ignored.
;; * {C-c C-d}: Mark as delete to current line (including newline).
;; * {C-c C-r}: Remove the changes in the region (these changes are not
;;              applied to the files. Of course, the remaining
;;              changes can still be applied to the files.)
;; * {C-c C-p}: Toggle read-only area.
;; * {C-c C-k}: Discard all changes and exit.
;; * {C-x C-q}: Exit wgrep mode.
(ensure-packages-present 'wgrep)
(setq wgrep-auto-save-buffer t)
(setq wgrep-change-readonly-file nil)

;; | Key chord           | Description       |
;; |---------------------+-------------------|
;; | C-x r t             | string-rectangle  |

;; electric
(setq electric-pair-preserve-balance t)
(setq electric-pair-delete-adjacent-pairs t)
(setq electric-pair-open-newline-between-pairs t)
(setq electric-pair-skip-whitespace 'nil)
(setq electric-pair-skip-self t)

(add-hook 'after-init-hook 'electric-indent-mode)

;; use print helper
(when (executable-find "gtklp")
  (setq lpr-command "gtklp"))

;; spelling
(setq flyspell-issue-message-flag nil)
(setq flyspell-issue-welcome-flag nil)
(setq ispell-program-name
      (or (executable-find "enchant-2")
          (executable-find "aspell")
          (executable-find "ispell")
          (executable-find "hunspell")))
(setq ispell-dictionary "american")
(setq flyspell-check-changes t)
(add-hook 'text-mode-hook 'flyspell-mode)

;; | Key chord      | Description     |
;; |----------------+-----------------|
;; | { C-c [ }      | add cite        |
;; | { C-c = }      | show toc        |

(ensure-packages-present 'auctex)
(load "auctex.el" nil t t)
(load "preview.el" nil t t)

(add-to-list 'major-mode-remap-alist '(TeX-mode . latex-mode))

(add-hook 'latex-mode-hook 'auto-fill-mode)
(add-hook 'latex-mode-hook 'reftex-mode)
(add-hook 'tex-mode-hook (lambda () (setq ispell-parser 'tex)))

;; init
(setq TeX-view-program-selection
      '(((output-dvi has-no-display-manager)
         "dvi2tty")
        ((output-dvi style-pstricks)
         "dvips and gv")
        (output-dvi "xdvi")
        (output-pdf "pdf-tools")
        (output-html "xdg-open")))
(setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-insert-braces nil)
(setq TeX-electric-escape t)
(setq TeX-electric-macro t)
(setq TeX-auto-untabify t)
(setq TeX-newline-function 'reindent-then-newline-and-indent)
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

(setq pdf-view-display-size 'fit-page)
;;(pdf-tools-install :no-query :skip-deps :no-error)

(setq doc-view-mupdf-use-svg t)

(ensure-packages-present 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(with-eval-after-load 'nov
  (defun my-nov-setup-hook ()
    (when-let ((font (font-spec :name "ETBembo Roman")))
      (face-remap-add-relative 'variable-pitch :family "ETBembo Roman"
                               :height 1.0))
    (set (make-local-variable 'show-trailing-whitespace) nil))
  (add-hook 'nov-mode-hook 'my-nov-setup-hook))

(ensure-packages-present 'x509-mode)

(ensure-packages-present 'markdown-mode)
(setq markdown-command "multimarkdown") ;; init
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(ensure-packages-present 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
(add-to-list 'magic-mode-alist '("---" . yaml-mode))

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
  (if-let ((xmlstarlet-cmd (or (and (eq system-type 'berkeley-unix)
                                    (executable-find "xml"))
                               (executable-find "xmlstarlet"))))
      (shell-command-on-region
       begin
       end
       (concat xmlstarlet-cmd " fo -s 2")
       nil
       'no-mark)
    (error "failed to find `xmlstarlet' program, please install it.")))

;; Use nxml-mode instead of sgml, xml or html mode.
(mapc
 (lambda (pair)
   (if (or (eq (cdr pair) 'xml-mode)
           (eq (cdr pair) 'sgml-mode))
       (setcdr pair 'nxml-mode)))
 auto-mode-alist)

(ensure-packages-present 'plantuml-mode)
(setq plantuml-default-exec-mode 'jar)
(setq plantuml-jar-path
      (car (file-expand-wildcards
            (concat (getenv "HOME") "/java/plantuml-*.jar"))))
(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))

(ensure-packages-present 'vundo)

(provide 'init-text)

;; init-text.el ends here
