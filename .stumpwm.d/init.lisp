;; -*- mode: lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-
;; Main configuration file for StumpWM
;;
;; Author: Timo Myyrä <timo.myyra@bittivirhe.fi>
;; Time-stamp: <2021-09-18 20:00:24 (tmy)>
;; URL: https://github.com/zmyrgel/dotfiles/
;; Copyright (C) 2012 Timo Myyrä
;;
;; ----------------------------------------------------------------------------
;;  - Fix Webjump functionality
;;  - Rewrite contrib packages to more portable way
;;  - Scroll firefox without focusing its frame
;;  - Update hook + notify on IRC messages + other urgent messages
;;  - Prevent pop-ups from stealing focus from current frame
;;  - Remove useless keymaps somehow
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :stumpwm)

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload '(slynk quri))

(defcommand start-slynk () ()
  (setf stumpwm:*top-level-error-action* :break)
  (slynk:create-server))

;; ---------------------------------------
;; Define global configurations

(defvar *font* "-*-terminus-medium-r-normal--16-*-*-*-*-*-iso10646-1"
  "Font to use for displaying stumwm ui elements.")

;;"-*-terminus-*-r-normal-*-16-*-*-*-*-*-*-*"

;;(defvar *fg-color* "gainsboro")
;;(defvar *bg-color* "gray7")

(defvar *contrib-path* (merge-pathnames ".stumpwm.d/modules"
                                        (user-homedir-pathname)))

(defvar *wallpaper-path* (merge-pathnames "wallpapers"
                                          (user-homedir-pathname)))
(defvar *wallpaper-extensions* (list "png" "jpg" "jpeg"))

(defvar *local-display* "eDP"
  "Display used for main display as given by xrandr.")

(defvar *x-lock-command*
  (if (member :openbsd *features*)
      "xlock -mode blank"
      "xscreensaver-command -lock")
  "Command used to lock X session.")

(defvar *browser* "firefox"
  "Default web browser to use.")

;; ---------------------------------------
;; Misc options

(defun update-window-title (window)
  "If the window comes up with no title, set the user title to the
window's instance name so it doesn't appear with no context on the
mode-line"
  (when (equal "" (window-title window))
    (setf (window-user-title window) (window-res window))))

(add-hook *new-window-hook* #'update-window-title)

;; ;; Set the font. Note that since I'm using a TTF font, I need to first
;; ;; load the ttf-fonts module, then (and this was hard to figure out!)
;; ;; I need to call xft:cache-fonts or xft is unable to locate the
;; ;; Monoid font.
;; (load-stump-contrib-module "util/ttf-fonts")
;; (xft:cache-fonts)

;; (defparameter *font*
;;   (make-instance 'xft:font :family "Monoid" :subfamily "Retina" :size 12)
;;   "Font specification for stumpwm windows.")

;; (set-font *font*)


(setf *mouse-focus-policy* :click)

;; Append colors to color map
;; XXX: add function to return name for hex value
;;      and to convert other color formats too
;; (setf *colors*
;;       (append *colors*
;;               (list *fg-color* "MidnightBlue")))
;; (update-color-map (current-screen))

;; ---------------------------------------
;; Message and Input Bar options

;; (set-fg-color *fg-color*)
;; (set-bg-color *bg-color*)

;; (set-border-color *fg-color*)

(set-msg-border-width 1)

(when *font*
  (set-font *font*))

(setf *message-window-padding* 10
      *message-window-gravity* :top-right
      *timeout-wait* 1
      *input-window-gravity* :top-right)

;; ---------------------------------------
;; Window options

;; (setf *window-format* "%m%n%s%20t"
;;       *maxsize-border-width* 1
;;       *transient-border-width* 1
;;       *normal-border-width* 1
;;       *window-border-style* :thin
;;       *window-name-source* :title)
;; (set-normal-gravity :top)
;; (set-maxsize-gravity :center)
;; (set-transient-gravity :center)

;; (set-win-bg-color *bg-color*)
;; (set-focus-color "MidnightBlue")
;; (set-unfocus-color *fg-color*)


;; ---------------------------------------
;; Modeline options

(unless (head-mode-line (current-head))
  (toggle-mode-line (current-screen) (current-head)))

;;(setf *screen-mode-line-format* "(%h){%g} [%W]")
(setf *screen-mode-line-format* "{%g} [%W]")

;; (setf *screen-mode-line-format*
;;       "[^9*^B%n^b^n] %W
;; %d | %m %N ")
;; (setf *mode-line-position* :bottom
;;       *mode-line-border-width* 1
;;       *mode-line-pad-x* 1
;;       *mode-line-pad-y* 1
;;       *mode-line-background-color* "gray7"
;;       *mode-line-foreground-color* "gainsboro"
;;       *mode-line-border-color* "gray7"
;;       *mode-line-timeout* 1)

;; ---------------------------------------
;; Group options

(defun set-stumpwm-groups ()
  "Rename the first group to Emacs and create the other groups."
  (setf (group-name (first (screen-groups (current-screen)))) "Emacs")
  (run-commands "gnewbg Web"
                "gnewbg IM"
                "gnewbg Misc"
                "gnewbg Remote"))

;; ---------------------------------------
;; Frame options

;; start numbering from 1 instead of 0
(setf *frame-number-map* "1234567890"
      *window-number-map* "1234567890")

(setf *suppress-frame-indicator* t)

;; Deny the browser windows from taking focus when started
;;(push '(:role "browser") stumpwm:*deny-map-request*)

;; Deny the slack windows from taking focus when started
;;(push '(:class "Slack") stumpwm:*deny-map-request*)

;; Window placement rules
(clear-window-placement-rules)

(define-frame-preference "Emacs"
  (1 t t :class "Emacs"))

(define-frame-preference "Web"
  (2 t t :role "browser"))

(define-frame-preference "IM"
  (3 t t :class "skype" :role "MainWindow")
  (3 t t :class "skype" :role "Chats")
  (3 t t :class "Slack"))

(define-frame-preference "Remote"
  (5 t t :class "Remmina"))

;; ---------------------------------------
;; Functions

(defun select-random-background-image ()
  "Select a random image from wallpapers directory."
  (let ((file-list (remove-if-not (lambda (x)
                                    (member (pathname-type x) *wallpaper-extensions* :test #'string=))
                                  (list-directory *wallpaper-path*)))
        (*random-state* (make-random-state t)))
    (namestring (nth (random (length file-list)) file-list))))

(defun set-wallpaper ()
  (run-shell-command "xwallpaper --maximize ~/wallpapers/pluto_blueskies.png"))

(defun set-qwerty-input ()
  (run-shell-command "setxkbmap -layout us -variant altgr-intl -option ctrl:nocaps"))

(defun set-dvorak-input ()
  (run-shell-command "setxkbmap -layout us -variant dvorak -option ctrl:nocaps"))

(defun start-initial-apps ()
  (loop for app in (list "xsetroot -solid rgb:11/11/11"
                         "xsetroot -cursor_name left_ptr -fg black -bg white"
                         "wmname LG3D"
                         ;;"pgrep mpd || mpd ~/.mpdconf"
                         )
        do (run-shell-command app)))

#+linux
(progn
  (defun nm-list-connections ()
  "List connections known by NM."
  (run-shell-command "nmcli con show" t)))

;; ---------------------------------------
;; MODULES

;; (handler-case (progn
;;                 (load-module "stumptray")
;;                 (stumptray::stumptray))
;;   (error (c)
;;     (format t "Unable to find module.~&")
;;     (values nil c)))

#+openbsd
(handler-case (progn
                (load-module "stumpwm-sndioctl")
                (define-key *top-map* (kbd "XF86AudioMute") "toggle-mute")
                (define-key *top-map* (kbd "XF86AudioLowerVolume") "volume-down")
                (define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume-up"))
  (error (c)
    (format t "Unable to find module.~&")
    (values nil c)))

(handler-case (progn
                (load-module "globalwindows")
                ;;(define-key *root-map* (kbd "M-1") "global-windowlist")
                ;;(define-key *root-map* (kbd "M-2") "global-pull-windowlist")
                )
  (error (c)
    (format t "Unable to find module.~&")
    (values nil c)))

(handler-case (progn
                (load-module "urgentwindows")
                (define-key *root-map* (kbd "u") "raise-urgent"))
  (error (c)
    (format t "Unable to find module.~&")
    (values nil c)))

(handler-case (progn
                (load-module "notifications")
                (setf (uiop:strcat *screen-mode-line-format*) " (%N)")
                (define-key *root-map* (kbd "N") '*notifications-map*))
  (error (c)
    (format t "Unable to find module.~&")
    (values nil c)))

;; ---------------------------------------
;; Commands

;; prompt the user for an interactive command. The first arg is an
;; optional initial contents.
(defcommand colon1 (&optional (initial "")) (:rest)
  (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
    (when cmd
      (eval-command cmd t))))

;; (defcommand suspend )

(defcommand paste-x-selection () (:rest)
  "Universal rat-less X paste."
  (let ((cmd (concatenate 'string "insert " (get-x-selection))))
    (eval-command cmd)))

(defcommand firefox () ()
  "run firefox"
  (run-or-raise "firefox" '(:class "Firefox")))

#+linux
(progn
  (defcommand slack () ()
    "Start Slack or switch to it, if it is already running."
    (run-or-raise "slack" '(:class "slack")))

  (defcommand steam () ()
    "Start Steam or switch to it, if it is already running."
    (run-or-raise "steam" '(:class "steam")))
  )

;; Web jump (works for Google and Imdb)
(defmacro make-web-jump (name prefix)
  `(defcommand ,(intern name) (search) ((:rest ,(concatenate 'string name " search: ")))
               (when (string/= search "")
                 (run-shell-command (concatenate 'string ,*browser* " " ,prefix (quri:url-encode search))))))

(make-web-jump "ddg" "https://www.duckduckgo.com?q=")

;; ---------------------------------------
;; HOOKS

;(add-hook *start-hook* #'set-displays)
(add-hook *start-hook* #'set-stumpwm-groups)
(add-hook *start-hook* #'set-wallpaper)

;; ---------------------------------------
;; KEYBINDINGS

;; Browse somewhere
(define-key *root-map* (kbd "C-b") (uiop:strcat "colon1 exec " *browser* " https://www."))
;; Ssh somewhere
(define-key *root-map* (kbd "C-s") "colon1 exec xterm -e ssh ")

;; paste
(define-key *root-map* (kbd "y") "paste-x-selection")

;(define-key *root-map* (kbd "s") "slack"))

;; C-t M-s is a terrble binding, but you get the idea.
(define-key *root-map* (kbd "i") "ddg")

;; allow locking x
(define-key *root-map* (kbd "L") (uiop:strcat "exec " *x-lock-command*))

;; Emacs Style Frame Splitting
;;(loop for n below 10
;;   do (undefine-key *root-map* (kbd (write-to-string n))))

;;(define-key *root-map* (kbd "0") "remove")
;;(define-key *root-map* (kbd "1") "only")
;;(define-key *root-map* (kbd "2") "vsplit")
;;(define-key *root-map* (kbd "3") "hsplit")

(define-remapped-keys
    '(("(Firefox|Chrome)"
       ("C-n"   . "Down")
       ("C-p"   . "Up")
       ("C-f"   . "Right")
       ("C-b"   . "Left")
       ("C-v"   . "Next")
       ("M-v"   . "Prior")
       ("M-w"   . "C-c")
       ("C-w"   . "C-x")
       ("C-y"   . "C-v")
       ("M-<"   . "Home")
       ("M->"   . "End")
       ("C-M-b" . "M-Left")
       ("C-M-f" . "M-Right")
       ("C-k"   . ("C-S-End" "C-x")))))

(define-key *root-map* (kbd "C-q") "send-raw-key")

(define-key *root-map* (kbd "B") "mode-line")

;; s-[0-9] moves to a numbered group.
;;(loop for i from 1 to 9
;;   do (define-key *top-map* (kbd (format nil "s-~A" i))
;;	(format nil "gselect ~A" i)))

;; ;; Query addresses
;; (defvar *query-map* nil
;;   "The keymap with net queries (e.g. IMDB)")

;; (setf *query-map*
;;       (let ((m (make-sparse-keymap)))
;;         (define-key m (kbd "i") "imdb")
;;         (define-key m (kbd "g") "google")
;;         (define-key m (kbd "w") "wikipedia")
;;         m))

;; Allow to quickly cycle groups
(define-key *top-map* (kbd "s-,") "gprev")
(define-key *top-map* (kbd "s-.") "gnext")

;; Swap quoteright and " in commands
(define-key *groups-map* (kbd "quoteright") "grouplist")
(define-key *groups-map* (kbd "\"") "gselect")
(define-key *root-map* (kbd "quoteright") "windowlist")
(define-key *root-map* (kbd "\"") "select")

;; undefine unused bindings
(undefine-key *root-map* (kbd "C-c"))
(undefine-key *root-map* (kbd "C-a"))
(undefine-key *root-map* (kbd "C-p"))
(undefine-key *root-map* (kbd "C-n"))
(undefine-key *root-map* (kbd "C-m"))
(undefine-key *root-map* (kbd "C-l"))
(undefine-key *root-map* (kbd "C-k"))

;; (setf *root-map*
;;       (let ((m (make-sparse-keymap)))
;;         (define-key m (kbd "c") "exec xterm")
;;         (define-key m (kbd "e") "emacs")
;;         (define-key m (kbd "b") "banish")
;;         (define-key m (kbd "a") "time")
;;         (define-key m (kbd "C-g") "abort")
;;         (define-key m (kbd "t") "send-escape")
;;         (define-key m (kbd ";") "colon")
;;         (define-key m (kbd ":") "eval")
;;         (define-key m (kbd "C-m") "lastmsg")
;;         (define-key m (kbd "G") "vgroups")
;;         (define-key m (kbd "g") '*groups-map*)
;;         (define-key m (kbd "h") '*help-map*)
;;         (define-key m (kbd "m") '*mpd-map*)
;;         (define-key m (kbd "q") '*query-map*)
;;         (define-key m (kbd "N") '*notifications-map*)
;;         (define-key m (kbd "!") "exec dmenu_run")
;;         (define-key m (kbd "quoteright") "windowlist")
;;         (define-key m (kbd "\"") "select")
;;         (define-key m (kbd "n") "pull-hidden-next")
;;         (define-key m (kbd "p") "pull-hidden-previous")
;;         (define-key m (kbd "M-n") "next")
;;         (define-key m (kbd "M-p") "prev")
;;         (define-key m (kbd "C-M-n") "next-in-frame")
;;         (define-key m (kbd "C-M-p") "prev-in-frame")
;;         (define-key m (kbd "W") "place-existing-windowns")
;;         (define-key m (kbd "C-t") "pull-hidden-other")
;;         (define-key m (kbd "M-t") "other-in-frame")
;;         (define-key m (kbd "R") "remove")
;;         (define-key m (kbd "s") "vsplit")
;;         (define-key m (kbd "S") "hsplit")
;;         (define-key m (kbd "r") "iresize")
;;         (define-key m (kbd "o") "fnext")
;;         (define-key m (kbd "M-TAB") "fprev")
;;         (define-key m (kbd "f") "fselect")
;;         (define-key m (kbd "F") "curframe")
;;         (define-key m (kbd "-") "fclear")
;;         (define-key m (kbd "Q") "only")
;;         ;; (define-key m (kbd "Up") "move-focus up")
;;         ;; (define-key m (kbd "Down") "move-focus down")
;;         ;; (define-key m (kbd "Left") "move-focus left")
;;         ;; (define-key m (kbd "Right") "move-focus right")
;;         ;; (define-key m (kbd "M-Up") "move-window up")
;;         ;; (define-key m (kbd "M-Up") "move-window down")
;;         ;; (define-key m (kbd "M-up") "move-window left")
;;         ;; (define-key m (kbd "M-Up") "move-window right")
;;         (define-key m (kbd "+") "balance-frames")
;;         (define-key m (kbd "l") "redisplay")
;;         (define-key m (kbd "L") "exec xlock -mode blank")
;;         (define-key m (kbd "C-u") "next-urgert")
;;         (define-key m (kbd "w") "windows")
;;         (define-key m (kbd "k") "delete")
;;         (define-key m (kbd "K") "kill")
;;         (define-key m (kbd "C-N") "number")
;;         (define-key m (kbd "#") "mark")
;;         (define-key m (kbd "C-F") "fullscreen")
;;         (define-key m (kbd "A") "title")
;;         (define-key m (kbd "i") "info")
;;         m))
