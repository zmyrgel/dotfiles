;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Copyright (c) 2025  Timo Myyrä <timo.myyra@bittivirhe.fi>

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

;; See my dotfiles: https://github.com/zmyrgel/dotfiles

;;; Code:

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1))

;; Better Window Management handling
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("Emacs"))

(setq inhibit-compacting-font-caches t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Temporarily increase the gc threshold and remove time-consuming file
;; handlers for the startup.
(defvar orig--gc-cons-threshold gc-cons-threshold)
(defvar orig--file-name-handler-alist file-name-handler-alist)
(defvar orig--vc-handled-backends vc-handled-backends)

(setq gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold orig--gc-cons-threshold
                  file-name-handler-alist orig--file-name-handler-alist
                  vc-handled-backends orig--vc-handled-backends)))

(provide 'early-init)
;;; early-init.el ends here
