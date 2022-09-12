;;; ------------------------------
;;; Text editing
;;; ------------------------------

;; | Key chord           | Description       |
;; |---------------------+-------------------|
;; | find-dired          | find + pattern    |
;; | find-name-dired     | find + name       |
;; | find-grep-dired     | find + grep       |
;; | find-lisp-find-dired| use emacs regexp  |

;; grep
(when (version<= "27" emacs-version)
  (setq grep-find-use-xargs 'exec-plus))

;; electric
(setq electric-pair-preserve-balance t)
(setq electric-pair-pairs '((34 . 34)
                            (8216 . 8217)
                            (8220 . 8221)
                            (123 . 125)))
(setq electric-pair-skip-self t)
(setq electric-pair-skip-whitespace 'chomp)
(add-hook 'after-init-hook 'electric-indent-mode)

;; | Key chord      | Description     |
;; |----------------+-----------------|
;; | C-c [          | add cite        |
;; | C-c =          | show toc        |

(unless (package-installed-p 'auctex)
  (package-install 'auctex))
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
(zmg/with-package 'pdf-tools
  (add-to-list 'magic-mode-alist '("%PDF" . pdf-view-mode))
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
  (add-hook 'pdf-view-mode 'pdf-links-minor-mode)
  (add-hook 'pdf-view-mode 'pdf-isearch-minor-mode)
  (add-hook 'pdf-view-mode 'pdf-outline-minor-mode)
  (add-hook 'pdf-view-mode 'pdf-history-minor-mode)

  (setq pdf-view-display-size 'fit-page)
  (pdf-tools-install :no-query))

(zmg/with-package 'nov
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

  (defun my-nov-setup-hook ()
    (face-remap-add-relative 'variable-pitch :family "ETBembo Roman"
                             :height 1.0)
    (set (make-local-variable 'show-trailing-whitespace) nil))
  (add-hook 'nov-mode-hook 'my-nov-setup-hook))

(zmg/with-package 'x509-mode)


(zmg/with-package 'markdown-mode
  ;; :commands (markdown-mode gfm-mode)
  (setq markdown-command "multimarkdown") ;; init
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode)))

(zmg/with-package 'yaml-mode
  (add-to-list 'auto-mode-alist '("\\.yml$\\|\\.yaml$"))
  (add-to-list 'magic-mode-alist '("---" . yaml-mode)))

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
(define-key global-map (kbd "C-c C-f") 'bf-pretty-print-xml-region)

(provide 'zmg-emacs-text)
