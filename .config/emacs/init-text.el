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
(when (version<= "27" emacs-version)
  (setq grep-find-use-xargs 'exec-plus))
;; https://stegosaurusdormant.com/emacs-ripgrep/
;; https://stackoverflow.com/questions/45526670/rgrep-in-emacs-to-use-ripgrep
;; (when (executable-find "rg")
;;   (grep-apply-setting
;;    'grep-find-command
;;    '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27)))

;; electric
(setq electric-pair-preserve-balance t)
(add-to-list 'electric-pair-pairs '(?\{ . ?\}) t)
(setq electric-pair-skip-self t)
(setq electric-pair-skip-whitespace 'nil)
(add-hook 'after-init-hook 'electric-indent-mode)

;; | Key chord      | Description     |
;; |----------------+-----------------|
;; | C-c [          | add cite        |
;; | C-c =          | show toc        |

(ensure-packages-present 'auctex)
(load "auctex.el" nil t t)
(load "preview.el" nil t t)
(add-to-list 'magic-mode-alist '("\\.[tT]e[xX]\\'" . latex-mode))
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
(pdf-tools-install :no-query)

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
(add-to-list 'auto-mode-alist '("\\.yml$\\|\\.yaml$"))
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

(defun bf-pretty-print-xml-region (begin end)
  "Function formats XML elements in region between BEGIN and END."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))

;; Use nxml-mode instead of sgml, xml or html mode.
(mapc
 (lambda (pair)
   (if (or (eq (cdr pair) 'xml-mode)
           (eq (cdr pair) 'sgml-mode))
       (setcdr pair 'nxml-mode)))
 auto-mode-alist)
;;(define-key global-map (kbd "C-c C-f") 'bf-pretty-print-xml-region)

(provide 'init-text)

;; init-text.el ends here
