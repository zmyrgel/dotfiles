;;; ------------------------------
;;; File and directory management
;;; ------------------------------

;; dired, not needed anymore?
;;(define-key ctl-x-map "C-j" 'dired-jump)
;;(define-key ctl-x-4-map "C-j" 'dired-jump-other-window)

(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(setq dired-dwim-target t)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-isearch-filenames t)
(setq dired-omit-verbose nil)
(setq dired-ls-F-marks-symlinks t)
;; Don't pass --dired flag to ls on BSD
(when (eq system-type 'berkeley-unix)
  (setq dired-use-ls-dired nil))
(setq dired-omit-files "^#\\|\\.$\\|~$\\|^RCS$\\|,v$")
(setq dired-guess-shell-alist-user
      '(("\\.avi$\\|\\.mkv$\\|\\.mov$\\|\\.mpeg$\\|\\.mp4$" "cvlc"
         "\\.rar$" "unrar e")))
(setq dired-guess-shell-gnutar (unless (eq system-type 'berkeley-unix)
                                 "tar"))

;; dired-aux
(setq dired-isearch-filenames 'dwim)
;; The following variables were introduced in Emacs 27.1
(unless (version<= emacs-version "27")
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t))

;; bongo
(zmg/package-install 'bongo)
;;  :defer t
(add-hook 'bongo-player-started-hook 'bongo-no-autoplay-video)
(global-set-key (kbd "<C-XF86AudioPlay>") 'bongo-pause/resume)
(global-set-key (kbd "<C-XF86AudioNext>") 'bongo-next)
(global-set-key (kbd "<C-XF86AudioPrev>") 'bongo-previous)
(global-set-key (kbd "<M-XF86AudioPlay>") 'bongo-show)
(global-set-key (kbd "C-z B") 'bongo)

;; FIXME: bongo-playlist-mode-map is void
(with-eval-after-load "bongo"
  (let ((play-map bongo-playlist-mode-map))
    (define-key play-map "n" 'bongo-next-object)
    (define-key play-map "p" 'bongo-previous-object)
    (define-key play-map "R" 'bongo-rename-line)
    (define-key play-map "j" 'bongo-dired-line)
    (define-key play-map "J" 'dired-jump)
    (define-key play-map "I" 'bongo-insert-special)))

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
         "mkv" "flv" "mov" "asf" "wmv" "rm" "rmvb" "ts")))

(provide 'zmg-emacs-files)
