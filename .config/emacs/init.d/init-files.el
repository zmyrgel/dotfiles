;;; init-files.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; - init file-related settings

;;; Code:

;; Useful key bindings { C-0 w } to copy link
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(setq dired-dwim-target t)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-isearch-filenames t)
(setq dired-omit-verbose nil)
(setq dired-omit-lines dired-re-no-dot)
(setq dired-ls-F-marks-symlinks t)
(setq dired-omit-files "^#\\|\\.$\\|~$\\|^RCS$\\|,v$")
(setq dired-guess-shell-alist-user
      '(("\\.avi$\\|\\.mkv$\\|\\.mov$\\|\\.mpeg$\\|\\.mp4$" "cvlc")))
(setq dired-guess-shell-gnutar (unless (eq system-type 'berkeley-unix)
                                 "tar"))

(setq dired-isearch-filenames 'dwim)
(setq dired-create-destination-dirs 'ask)
(setq dired-vc-rename-file t)
(setq dired-movement-style 'bounded)
;; {E} 'dired-do-open'

(define-key ctl-x-map (kbd "C-j") 'dired-jump)
(define-key ctl-x-4-map (kbd "C-j") 'dired-jump-other-window)

(when (executable-find "xwallpaper")
  (setq wallpaper-command "xwallpaper")
  (setq wallpaper-command-args '("--maximize" "%f")))

;; TRAMP stuff
(setq tramp-use-scp-direct-remote-copying t)
(setq tramp-file-name-with-method
      (when-let* ((become-cmd (or (executable-find "doas")
                                  (executable-find "sudo"))))
        (substring become-cmd -4)))

(defun parse-openbsd-ports-variables ()
  "Returns alist of OpenBSD system-specific ports variables."
  (let ((ports-variables nil))
    (with-temp-buffer
      (insert-file-contents "/etc/mk.conf" nil)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (when (string-match "^[A-Z]+" line)
            (let ((matches (split-string line "=" t " ")))
              (push (cons (car matches)
                          (cadr matches))
                    ports-variables)))
          (forward-line 1))))
    ports-variables))

(defun dired-do-copy-port-orig ()
  "Copy file as backup file with `.orig.port' suffix for OpenBSD ports
framework. Use dedicated _pbuild user for the copy. Reads the necessary
system variables from /etc/mk.conf.

If called from a dired buffer copies the file pointed by current line or
all marked files."
  (interactive) ;; add buffer or filename as parameter?
  (let* ((openbsd-ports-variables (parse-openbsd-ports-variables))
         (privsep (alist-get "PORTS_PRIVSEP" openbsd-ports-variables nil nil #'string=))
         (build-user (alist-get "BUILD_USER" openbsd-ports-variables "_pbuild" nil #'string=))
         (ports-dir (alist-get "PORTS_DIR" openbsd-ports-variables "/usr/ports" nil #'string=))
         (patch-orig (alist-get "PATCHORIG" openbsd-ports-variables ".orig.port" nil #'string=))
         (sudo (alist-get "SUDO" openbsd-ports-variables nil nil #'string=))
         (tramp-histfile-override nil)
         (make-backup-files t)
         (backup-by-copying t)
         (tramp-auto-save-directory nil)
         (make-backup-file-name-function (lambda (filename)
                                           (concat filename patch-orig))))
    (dolist (file (if (eq major-mode 'dired-mode)
                      (dired-get-marked-files nil nil #'file-regular-p)
                    (list buffer-file-name)))
      (unless (string-prefix-p (concat ports-dir "/pobj") file)
        (error "port patch files must be generated within the %s directory" ports-dir))
      (let ((orig-buffer (find-file-noselect
                            (if (and privsep sudo)
                                (concat "/" (substring sudo -4) ":" build-user "@localhost:" file)
                              file))))
        (with-current-buffer orig-buffer
          ;; TODO: accesses _pbuild HOME for some reason
          ;; TODO: check if buffer remains open in buffer list or should it be killed
          (backup-buffer))
        (kill-buffer orig-buffer)))) ;; what if file was already open, do not want to kill it
  ;; if from dired,
  (when (eq major-mode 'dired-mode)
    (revert-buffer t t)))

;; bongo
(with-eval-after-load 'bongo
  (add-hook 'bongo-player-started-hook 'bongo-no-autoplay-video)
  (let ((map bongo-playlist-mode-map))
    (define-key map "n" 'bongo-next-object)
    (define-key map "p" 'bongo-previous-object)
    (define-key map "R" 'bongo-rename-line)
    (define-key map "j" 'bongo-dired-line)
    (define-key map "J" 'dired-jump)
    (define-key map "I" 'bongo-insert-special))

  (setq bongo-default-directory (expand-file-name "Music" "~"))
  (setq bongo-prefer-library-buffers nil)
  (setq bongo-insert-whole-directory-trees t)
  (setq bongo-logo nil)
  (setq bongo-display-track-icons nil)
  (setq bongo-display-track-lengths nil)
  (setq bongo-display-header-icons nil)
  (setq bongo-display-playback-mode-indicator t)
  (setq bongo-display-inline-playback-progress t)
  (setq bongo-join-inserted-tracks nil)
  (setq bongo-field-separator (propertize " · " 'face 'shadow))
  (setq bongo-mark-played-tracks t)
  (setq bongo-header-line-mode nil)
  (setq bongo-mode-line-indicator-mode nil)
  (setq bongo-enabled-backends '(vlc mpv))
  (setq bongo-vlc-program-name "cvlc")

  (defun bongo-no-autoplay-video ()
    "don't autoplay next track if playing video"
    (with-bongo-playlist-buffer
     (when (bongo-video-file-name-p
            (bongo-player-get bongo-player 'file-name))
       (setq bongo-next-action 'bongo-stop))))

  (setq bongo-custom-backend-matchers
	`((vlc
           (local-file "file:" "http:" "ftp:")
           "ogg" "flac" "mp3" "m4a" "mka" "wav" "wma"
           "mpg" "mpeg" "vob" "avi" "ogm" "mp4" "m4v"
           "mkv" "flv" "mov" "asf" "wmv" "rm" "rmvb" "ts"))))

(global-set-key (kbd "<C-XF86AudioPlay>") 'bongo-pause/resume)
(global-set-key (kbd "<C-XF86AudioNext>") 'bongo-next)
(global-set-key (kbd "<C-XF86AudioPrev>") 'bongo-previous)
(global-set-key (kbd "<M-XF86AudioPlay>") 'bongo-show)
(global-set-key (kbd "C-z B") 'bongo)

(provide 'init-files)

;; init-files.el ends here
