;;; init-extras.el --- Extra package configurations -*- lexical-binding: t -*-

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

;;; - Extra configuration for emacs

;;; Code:

;;; Marginalia :: enhanche command etc. output with extra info
(ensure-packages-present '(marginalia))
(setopt marginalia-max-relative-age 0)
(marginalia-mode)

;;; EMMS :: multimedia playback
(ensure-packages-present 'emms)
(require 'emms-setup)
(emms-all)
(setopt emms-player-list '(emms-player-vlc)
        emms-info-functions '(emms-info-native))

;;; terraform
(ensure-packages-present '(terraform-doc terraform-mode))

;;; nov :: ebook display
(ensure-packages-present 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(with-eval-after-load 'nov
  (defun my-nov-setup-hook ()
    (when-let* ((font (font-spec :name "ETBembo Roman")))
      (face-remap-add-relative 'variable-pitch :family "ETBembo Roman"
                               :height 1.0))
    (set (make-local-variable 'show-trailing-whitespace) nil))
  (add-hook 'nov-mode-hook 'my-nov-setup-hook))

;;; x509 :: certificates
(ensure-packages-present 'x509-mode)

;;; plantuml :: system design
(ensure-packages-present 'plantuml-mode)
(setopt plantuml-default-exec-mode 'jar)
(setopt plantuml-jar-path
        (car (file-expand-wildcards
              (concat (getenv "HOME") "/java/plantuml-*.jar"))))
(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))

;;; vundo :: visual undo
(ensure-packages-present 'vundo)

;;; easy-kill
(ensure-packages-present 'easy-kill)
(define-key global-map [remap kill-ring-save] #'easy-kill)
(define-key global-map [remap mark-sexp] #'easy-mark)

;;; suomalainen-kalenteri :: add finnish holidays
(ensure-packages-present 'suomalainen-kalenteri)

;;; restclient :: add
(ensure-packages-present 'restclient)
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
;;TODO: change to only apply json formatting when the content-type is
;;application/json
(with-eval-after-load 'restclient
  (keymap-set restclient-mode-map "C-c C-f" 'json-mode-beautify))

;;; vcl-mode :: edit varnish/vinyl cache configuration
(ensure-packages-present 'vcl-mode)

;;; hyperbole :: buttons
;; rebinds kill-region :(
(ensure-packages-present 'hyperbole)
(add-hook 'after-init-hook 'hyperbole-mode)

(keymap-global-set "C-c h" 'hkey-either)

(provide 'init-extras)

;;; init-extras.el ends here
