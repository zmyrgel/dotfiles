;;; init-extras.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - Extra configuration for emacs

;;; Code:

;; enhanche command etc. output with extra info
(ensure-packages-present '(marginalia))

(setopt marginalia-max-relative-age 0)
(marginalia-mode)

;; EMMS
(ensure-packages-present 'emms)
(require 'emms-setup)
(emms-all)
(setopt emms-player-list '(emms-player-vlc)
        emms-info-functions '(emms-info-native))

(ensure-packages-present '(terraform-doc terraform-mode))

(ensure-packages-present 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(with-eval-after-load 'nov
  (defun my-nov-setup-hook ()
    (when-let* ((font (font-spec :name "ETBembo Roman")))
      (face-remap-add-relative 'variable-pitch :family "ETBembo Roman"
                               :height 1.0))
    (set (make-local-variable 'show-trailing-whitespace) nil))
  (add-hook 'nov-mode-hook 'my-nov-setup-hook))

(ensure-packages-present 'x509-mode)

(ensure-packages-present 'plantuml-mode)
(setopt plantuml-default-exec-mode 'jar)
(setopt plantuml-jar-path
        (car (file-expand-wildcards
              (concat (getenv "HOME") "/java/plantuml-*.jar"))))
(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))

(ensure-packages-present 'vundo)

(ensure-packages-present 'easy-kill)
(define-key global-map [remap kill-ring-save] #'easy-kill)
(define-key global-map [remap mark-sexp] #'easy-mark)

(ensure-packages-present 'suomalainen-kalenteri)

(ensure-packages-present 'restclient)
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
;;TODO: change to only apply json formatting when the content-type is
;;application/json
(with-eval-after-load 'restclient
  (keymap-set restclient-mode-map "C-c C-f" 'json-mode-beautify))

(ensure-packages-present 'vcl-mode)

(provide 'init-extras)

;; init-extras.el ends here
