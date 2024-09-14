;;; init-text.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - Text-related settings

;;; Code:

;; | Key chord           | Description       |
;; |---------------------+-------------------|
;; | find-dired          | find + pattern    |
;; | find-name-dired     | find + name       |
;; | find-grep-dired     | find + grep       |
;; | find-lisp-find-dired| use emacs regexp  |

;; grep
(setq grep-find-use-xargs 'exec-plus)
;; https://stegosaurusdormant.com/emacs-ripgrep/
;; https://stackoverflow.com/questions/45526670/rgrep-in-emacs-to-use-ripgrep

;; (when (executable-find "rg")
;;   (grep-apply-setting
;;    'grep-find-command
;;    '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27)))

;; electric
(setq electric-pair-preserve-balance t)
(setq electric-pair-delete-adjacent-pairs t)
(setq electric-pair-open-newline-between-pairs t)
(setq electric-pair-skip-whitespace 'nil)
;;(add-to-list 'electric-pair-pairs '(?\{ . ?\}) t)
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
;;(pdf-tools-install :no-query)

(setq doc-view-mupdf-use-svg t) ;; 29

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

;; TODO: is this needed at all, compare with sgml-pretty-print
;; TODO: external app to indent?
(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. The function inserts linebreaks to
separate tags that have nothing but whitespace between them. It then
indents the markup by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n") (setq end (1+ end)))
    (indent-region begin end))
  (message "Ah, much better!"))

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

(provide 'init-text)

;; init-text.el ends here
