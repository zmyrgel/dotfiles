;;; init-text.el --- Generic text management -*- lexical-binding: t -*-

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

;; Generic text management utilities

;;; Code:

;;; whitespace mode
(setopt whitespace-line-column 80)
(setopt whitespace-style '(face lines-tail trailing))
(setopt whitespace-global-modes '(not agent-shell-mode magit-status-mode magit-diff-mode))
(global-whitespace-mode)

;; | Key chord | Description                  |
;; |-----------+------------------------------|
;; | C-s C-w   | Search char or word at point |
;; | M-s .     | Similar, but broader match   |
;; | M-s o     | Run `occur' on regexp        |
;; | M-s h r   | Highlight regexp             |
;; | M-s h u   | Undo the highlight           |
;; | C-s M-r   | Toggle regexp search         |
;; | M-%       | Run `query-replace'          |
;; | C-M-%     | `query-replace-regexp'       |

(setopt isearch-lazy-highlight t)
(setopt isearch-lazy-count t)
(setopt isearch-lax-whitespace t)
(setopt search-whitespace-regexp ".*?")
(setopt lazy-count-prefix-format nil)
(setopt lazy-count-suffix-format " (%s/%s)")
(setopt isearch-yank-on-move 'shift)
(setopt isearch-allow-scroll 'unlimited)
(setopt query-replace-highlight t)
(setq isearch-regexp-lax-whitespace nil)
(setq isearch-highlight t) ;; not documented?

;;; grep
(setopt grep-find-use-xargs 'exec-plus)

;; find-dired results with human readable sizes
;; (find-ls-option '("-exec ls -ldh {} +" . "-ldh"))
(setopt grep-find-ignored-directories
        '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr"
          "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist"))

;; rg stuff for evaluation
(when (and nil (executable-find "rg"))
  (setopt xref-search-program 'ripgrep)
  (setopt grep-find-template "find <D> <X> -type f <F> -exec rg <C> --no-heading -H  <R> /dev/null {} +")
  (setopt grep-template "rg --no-heading -H -uu -g <F> <R> <D>")
  (setopt grep-find-command '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27))
  (setopt grep-command "rg -nS --no-heading "))

;; TODO: This needed?
;; (add-to-list 'grep-find-ignored-directories "node_modules")
;; (add-to-list 'grep-find-ignored-directories "build")
;; (add-to-list 'grep-find-ignored-directories "dist")

;; | {C-x r t} string-rectangle

;;; electric
(setopt electric-pair-skip-whitespace 'chomp)
(add-hook 'after-init-hook 'electric-pair-mode)
(add-hook 'after-init-hook 'electric-indent-mode)

;;; use print helper
(when (executable-find "gtklp")
  (setopt lpr-command "gtklp"))

;;; flyspell
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
(add-hook 'org-mode-hook 'flyspell-mode)

;;; doc-view - replace with emacs-reader?
(setopt doc-view-mupdf-use-svg t)
(setopt doc-view-resolution 120)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;;; yaml
(add-to-list 'magic-mode-alist '("---" . yaml-ts-mode))

;;; xml
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

;;; conf
(add-to-list 'auto-mode-alist '("\\.env\\'" . conf-mode))

;;; markdown
(autoload 'markdown-ts-mode "markdown-ts-mode" nil t)
(dolist (re '("\\.md\\'" "\\.mdx\\'" "\\.markdown\\'"))
  (add-to-list 'auto-mode-alist (cons re 'markdown-ts-mode)))
(with-eval-after-load 'markdown-ts-mode
  (require 'markdown-ts-mode-x))

;;; tex

;; | Key chord    | Description     |
;; |--------------+-----------------|
;; | { C-c [ }    | add cite        |
;; | { C-c = }    | show toc        |

(ensure-packages-present 'auctex)
(load "auctex.el" nil t t)
(load "preview.el" nil t t)

(add-to-list 'major-mode-remap-alist '(TeX-mode . latex-mode))

(defun my/set-tex-parser ()
  "Set the ispell-parser to use TeX."
  (setq ispell-parser 'tex))

(add-hook 'latex-mode-hook 'auto-fill-mode)
(add-hook 'latex-mode-hook 'reftex-mode)
(add-hook 'tex-mode-hook 'my/set-tex-parser)

;; these two do not match custom type
;; (setopt TeX-view-program-selection
;;         '(((output-dvi has-no-display-manager) "dvi2tty")
;;           ((output-dvi style-pstricks) "dvips and gv")
;;           (output-dvi "xdvi")
;;           (output-pdf "pdf-tools")

(setopt TeX-command-extra-options "-shell-escape -8bit")
(setopt TeX-auto-save t)
(setopt TeX-parse-self t)
(setopt TeX-insert-braces nil)
(setopt TeX-electric-escape t)
(setopt TeX-auto-untabify t)
(setopt TeX-newline-function 'reindent-then-newline-and-indent)
(add-hook 'TeX-after-compilation-finished-functions 'TeX-revert-document-buffer)

;;; pdf-tools
(ensure-packages-present 'pdf-tools)
(require 'pdf-tools nil t)
(add-to-list 'magic-mode-alist '("%PDF" . pdf-view-mode))
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
(add-hook 'pdf-view-mode-hook 'pdf-links-minor-mode)
(add-hook 'pdf-view-mode-hook 'pdf-isearch-minor-mode)
(add-hook 'pdf-view-mode-hook 'pdf-outline-minor-mode)
(add-hook 'pdf-view-mode-hook 'pdf-history-minor-mode)

(setopt pdf-view-display-size 'fit-page)
(pdf-tools-install :no-query :skip-deps :no-error)

;;; bibliothek
(ensure-packages-present 'bibliothek)
(setopt bibliothek-path '("~/Documents"))
(setopt bibliothek-recursive t)

(provide 'init-text)

;;; init-text.el ends here
