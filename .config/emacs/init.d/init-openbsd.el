;;; init-openbsd.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; OpenBSD specific extensions for Emacs

;;; Code:

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

(provide 'init-openbsd)

;; init-openbsd.el ends here
