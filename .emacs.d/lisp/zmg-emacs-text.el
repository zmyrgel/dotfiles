;;; ------------------------------
;;; Text editing
;;; ------------------------------

;; | Key chord           | Description       |
;; |---------------------+-------------------|
;; | find-dired          | find + pattern    |
;; | find-name-dired     | find + name       |
;; | find-grep-dired     | find + grep       |
;; | find-lisp-find-dired| use emacs regexp  |

(use-package grep
  :config
  (when (version<= "27" emacs-version)
    (setq grep-find-use-xargs 'exec-plus)))

(use-package electric
  :config
  (setq electric-pair-preserve-balance t)
  (setq electric-pair-pairs '((34 . 34)
                              (8216 . 8217)
                              (8220 . 8221)
                              (123 . 125)))
  (setq electric-pair-skip-self t)
  (setq electric-pair-skip-whitespace 'chomp)
  :hook ((after-init-hook . electric-indent-mode)))

;; | Key chord      | Description     |
;; |----------------+-----------------|
;; | C-c [          | add cite        |
;; | C-c =          | show toc        |
(use-package tex
  :defer t
  :ensure auctex
  :mode ("\\.[tT]e[xX]\\'" . latex-mode)
  :hook ((latex-mode-hook . auto-fill-mode)
         (latex-mode-hook . reftex-mode)
         (tex-mode-hook . (lambda ()
                            (setq ispell-parser 'tex))))
  :init
  (setq TeX-view-program-selection
        '(((output-dvi has-no-display-manager)
           "dvi2tty")
          ((output-dvi style-pstricks)
           "dvips and gv")
          (output-dvi "xdvi")
        (output-pdf "pdf-tools")
          (output-html "xdg-open")))
  (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-insert-braces nil)
  (setq TeX-electric-escape t)
  (setq TeX-electric-macro t)
  (setq TeX-auto-untabify t)
  (setq TeX-newline-function 'reindent-then-newline-and-indent)
  (add-hook 'TeX-after-compilation-finished-functions 'TeX-revert-document-buffer))

;; doc-view / doc-view-presentation
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :hook
  (pdf-view-mode . pdf-links-minor-mode)
  (pdf-view-mode . pdf-isearch-minor-mode)
  (pdf-view-mode . pdf-outline-minor-mode)
  (pdf-view-mode . pdf-history-minor-mode)
  :config
  (setq pdf-view-display-size 'fit-page)
  (pdf-tools-install :no-query))

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (defun my-nov-setup-hook ()
    (face-remap-add-relative 'variable-pitch :family "ETBembo Roman"
                             :height 1.0)
    (set (make-local-variable 'show-trailing-whitespace) nil))
  (add-hook 'nov-mode-hook 'my-nov-setup-hook))

(use-package x509-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml$\\|\\.yaml$"
  :magic ("---" . yaml-mode))

(use-package nxml-mode
  ;; Any file start with xml will be treat as nxml-mode
  :magic ("<\\?xml" . nxml-mode)
  :mode (("\\.plist\\'" . nxml-mode)
         ("\\.rss\\'"   . nxml-mode)
         ("\\.svg\\'"   . nxml-mode)
         ("\\.xml\\'"   . nxml-mode)
         ("\\.xsd\\'"   . nxml-mode)
         ("\\.xslt\\'"  . nxml-mode)
         ("\\.pom$"     . nxml-mode))
  :config
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
  (define-key nxml-mode-map (kbd "C-c C-f") 'bf-pretty-print-xml-region))

(provide 'zmg-emacs-text)
