;;; init-webdev.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - Web development related programming settings

;;; Code:

;;; PHP programming

(defun php-symbol-lookup ()
    (interactive)
    ;; Poll user for symbol to look up
    (let ((url-format "https://www.php.net/manual/en/function.%s.php"))
      (with-temp-buffer
        (insert (read-from-minibuffer "Function to lookup: "))
        (goto-char (point-min))
        (replace-regexp "_" "-")
        (eww (format url-format (buffer-string))))))

(with-eval-after-load 'php-ts-mode

  (defun my/php-ts-mode-hook ()
    (setopt php-ts-mode-indent-style 'symfony) ;; was 'psr2
    (setq-local indent-tabs-mode nil)
    (setopt php-ts-indent-offset 4))

  (add-hook 'php-ts-mode-hook 'my/php-ts-mode-hook))

(ensure-packages-present 'web-mode)
(dolist (m '(("\\.jsp\\'" . web-mode)
             ("\\.ap[cp]x\\'" . web-mode)
             ("\\.erb\\'" . web-mode)
             ("\\.rhtml\\'" . web-mode)
             ("\\.mustache\\'" . web-mode)
             ("\\.djhtml\\'" . web-mode)
             ("\\.jsx\\'" . web-mode)))
  (add-to-list 'magic-mode-alist m))

(setopt web-mode-markup-indent-offset 2)
(setopt web-mode-css-indent-offset 2)
(setopt web-mode-code-indent-offset 4)

(defun my/web-mode-hook ()
  "Hooks for Web mode."
  (when (and (member (file-name-extension buffer-file-name) '("jsx"))
             (require 'eglot nil 'noerror))
    (eglot-ensure)))

(add-hook 'web-mode-hook 'my/web-mode-hook)
;;(add-hook 'web-mode-hook 'flymake-eslint-enable)

(with-eval-after-load 'typescript-ts-mode
  (add-hook 'typescript-ts-mode-hook 'eglot-ensure)
  ;;(add-hook 'typescript-ts-mode-hook 'flymake-eslint-enable)
  (add-hook 'tsx-ts-mode-hook 'eglot-ensure)
  ;;(add-hook 'tsx-ts-mode-hook 'flymake-eslint-enable)
  (setq-local whitespace-line-column 120))

;; (ensure-packages-present 'flymake-eslint)
;; (setopt flymake-eslint-executable-name "eslint")
;; (setopt flymake-eslint-executable-args nil)
;; (setopt flymake-eslint-show-rule-name t)
;; (setopt flymake-eslint-defer-binary-check t)

;; (ensure-packages-present 'prettier)
;; (add-hook 'typescript-ts-hook #'prettier-mode)

(ensure-packages-present 'ts-comint)

;; TODO: use biome!
(defun my/use-project-eslint ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint
          (and root
               (expand-file-name "node_modules/.bin/eslint"
                                 root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flymake-eslint-executable-name eslint))))

(defun my-typescript-hook ()
  "My shared options for Typescript(-ts)?-mode"
  (setq-local typescript-ts-indent-offset 4)
  (keymap-local-set "C-x C-e" 'ts-send-last-sexp)
  (keymap-local-set "C-M-x" 'ts-send-last-sexp-and-go)
  (keymap-local-set "C-c b" 'ts-send-buffer)
  (keymap-local-set "C-c C-b" 'ts-send-buffer-and-go)
  (keymap-local-set "C-c l" 'ts-load-file-and-go))

(add-hook 'typescript-ts-mode-hook #'my-typescript-hook)

(defun list-npm-package-files ()
  "List of all package.json files within a project."
  (seq-filter (lambda (filepath)
                (string-match "package\.json$" filepath))
              (project-files (project-current))))

(defun list-api-doc-files ()
  "List of all package.json files within a project."
  (seq-filter (lambda (filepath)
                (string-match "openapi\.yaml$" filepath))
              (project-files (project-current))))

(defun %parse-package-names (file)
  "Parse the names of npm packages."
  (unless (json-available-p)
    (error "JSON parsing not available"))
  (let ((packages nil))
    (with-temp-buffer
      (insert-file-contents file)
      (let* ((json-ht (json-parse-buffer))
             (dev-dependencies (gethash "devDependencies" json-ht))
             (dependencies (gethash "dependencies" json-ht)))
        (when dev-dependencies
          (maphash (lambda (k _v)
                     (push k packages))
                   dev-dependencies))
        (when dependencies
          (maphash (lambda (k _v)
                     (push k packages))
                   dependencies))))
    (delete-dups packages)))

(defun valid-npm-sem-ver-p (version-string)
  "Return t if given valid NPM semver string."
  (and (string-match "[\\^~]?[0-9\\.]+" version-string)
       t))

(defun %update-package-version (file package-name new-version)
  "Update the PACKAGE-NAME version in FILE to NEW-VERSION."
  (with-temp-file file
    (insert-file-contents file)
    (when-let* ((package-end (re-search-forward (format "^\s+\"\\%s\": \"\\([~\\.\\^0-9]+\\)\"" package-name) nil t)))
      (delete-region (search-backward ":") package-end)
      (insert (format ": \"%s\"" new-version)))))

(defun project-update-npm-package-version ()
  "Quickly update the npm package versions inside a project."
  (interactive)
  (let* ((package-files (list-npm-package-files))
         (existing-packages (mapcan #'%parse-package-names package-files))
         (package-name (completing-read "Give package name to update: " existing-packages))
         (new-version (read-from-minibuffer "Give new package version to set: ")))
    (unless (valid-npm-sem-ver-p new-version)
      (error "invalid version string: %s" new-version))
    (dolist (file package-files)
      (%update-package-version file package-name new-version))
    (message "updated package %s to version %s in project package.json files" package-name new-version)))

(defun project-update-npm-project-version ()
  "Quickly update the npm project version inside a project."
  (interactive)
  (let* ((package-files (list-npm-package-files))
         (new-version (read-from-minibuffer "Give new project version to set: ")))
    (unless (valid-npm-sem-ver-p new-version)
      (error "invalid version string: %s" new-version))
    (dolist (file package-files)
      (%update-package-version file "version" new-version))
    (message "updated project version to %s in project package.json files" new-version)))

(defun project-update-project-api-version ()
  "Quickly update the project version inside API-docs of an project."
  (interactive)
  (let* ((files (list-api-doc-files))
         (new-version (read-from-minibuffer "Give new project version to set: ")))
    (unless (valid-npm-sem-ver-p new-version)
      (error "invalid version string: %s" new-version))
    (dolist (file files)
      (%update-package-version file "version" new-version))
    (message "updated project version to %s in project openapi.yaml files" new-version)))

(provide 'init-webdev)

;; init-webdev.el ends here
