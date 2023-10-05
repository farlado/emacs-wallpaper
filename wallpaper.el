;;; wallpaper.el --- Setting the wallpaper -*- lexical-binding: t -*-

;; Copyright (C) 2020

;; Author: Farlado <farlado@sdf.org>
;; URL: https://github.com/farlado/emacs-wallpaper
;; Keywords: unix, wallpaper, extensions
;; Version: 1.1.3
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.



;;; Commentary:

;; Ricing `exwm' is pretty cool, but even cooler is being able to manage
;; everything about your rice using Emacs.  This package is meant to
;; make that a little bit easier, by managing your wallpapers for you.
;;
;; The provided modes are `wallpaper-cycle-mode' for cycling randomly
;; through wallpapers in a directory, and `wallpaper-per-workspace-mode'
;; for setting a specific wallpaper or wallpapers in each workspace or
;; a window manager.
;;
;; The following window managers are known to work with this package:
;; - EXWM
;; - Any that use vdesk for virtual desktops
;;
;; The following might work but aren't fully tested:
;; - i3
;;
;; Compatibility is only guaranteed with use of an X desktop session.


;;; Code:

(unless (executable-find "feh")
  (display-warning 'wallpaper "External command `feh' not found!"))

(require 'cl-lib)



(defgroup wallpaper nil
  "Setting the wallpaper."
  :tag "Wallpaper"
  :group 'environment
  :prefix "wallpaper-")



(defcustom wallpaper-per-workspace-alist nil
  "List of wallpapers per workspace.

Each item is (WORKSPACE WALLPAPERS).  When WORKSPACE is the current
workspace, WALLPAPERS are any number of absolute paths for the
wallpapers to be set as from their absolute path."
  :tag "Per-workspace alist"
  :group 'wallpaper
  :type 'list)

(defcustom wallpaper-per-workspace-get #'wallpaper-per-workspace-exwm-get
  "What function to use for determining the current workspace."
  :tag "Per-workspace function"
  :group 'wallpaper
  :type 'function)



(defcustom wallpaper-static-wallpaper-list nil
  "List of wallpapers to use instead of randomly finding wallpapers.

Wallpapers must be entered in this list as absolute paths, in the order
of your monitors.  This list should be left blank if you intend to use
function `wallpaper-cycle-mode'."
  :tag "Static wallpaper(s)"
  :group 'wallpaper
  :type 'list)



(defcustom wallpaper-cycle-interval 15
  "Interval in seconds for cycling in function `wallpaper-cycle-mode'."
  :tag "Wallpaper cycle interval"
  :group 'wallpaper
  :type 'integer)

(defcustom wallpaper-cycle-single nil
  "Whether to use one wallpaper across all monitors.

This setting is not respected when `wallpaper-static-wallpapers' is
non-nil.  To have only one wallpaper for all monitors, ensure only
one path is listed in `wallpaper-static-wallpapers'."
  :tag "Single wallpaper"
  :group 'wallpaper
  :type 'boolean)

(defcustom wallpaper-cycle-directory (expand-file-name "wallpapers"
						       (getenv "XDG_DATA_HOME"))
  "The directory in which to look for wallpapers."
  :tag "Wallpaper directory"
  :group 'wallpaper
  :type 'string)

(defcustom wallpaper-cycle-extension-regexp ".[gjpGJP][inpINP][efgEFG]+$"
  "The regexp used to locate wallpapers in `wallpaper-cycle-directory'."
  :tag "Wallpaper extension regexp"
  :group 'wallpaper
  :type 'string)



(defcustom wallpaper-scaling 'fill
  "What style of wallpaper scaling to use.

The options are
scale: Scale the image to fit the screen, distorting the image
max: Show the whole image, leaving portions of the screen uncovered
fill: Fill the entire screen, cutting off regions of the image
tile: Tile the image across the screen for small images
center: Center the image on the screen

The default option is fill."
  :tag "Wallpaper style"
  :group 'wallpaper
  :type '(radio (const :tag "Scale" scale)
                (const :tag "Maximize" max)
                (const :tag "Fill" fill)
                (const :tag "Tile" tile)
                (const :tag "Center" center)))

(defcustom wallpaper-background "#000000"
  "The background color to display behind the wallpaper."
  :tag "Background color"
  :group 'wallpaper
  :type 'string)



(defvar wallpaper-current-wallpapers nil
  "List of the wallpaper(s) currently in use.

This variable is set automatically.  Hand modification of its value
may interfere with its proper behavior.")

;; In my opinion this should be named `wallpaper-set-random-wallpaper'.  Seeing
;; this I would think that this is an interactive function to choose the
;; wallpaper.  In fact, even reading the doc-string would make me thing that
;; because it just says "set the wallpaper".  It should at least say "set random
;; wallpaper".

;; It's a matter of opinion whether by "set a random wallpaper" the user means
;; set a random wallpaper that's not the current one.  To be honest that's what
;; I mean when I say that.  I mean why would I even call this command if I'm
;; O.K. with the same wallpaper?
;;;###autoload
(defun wallpaper-set-wallpaper ()
  "Set the wallpaper.

This function will either choose a random wallpaper from
`wallpaper-cycle-directory' or use the wallpapers listed in
`wallpaper-static-wallpapers'."
  (interactive)
  (let ((wallpapers (or (wallpaper--per-workspace-wallpapers)
                        wallpaper-static-wallpaper-list
                        (wallpaper--random-wallpapers)))
        (command (concat "feh --no-fehbg " (wallpaper--background))))
    (if (not wallpapers)
        (message "No wallpapers selected.")
      (setq wallpaper-current-wallpapers nil)
      (dolist (wallpaper wallpapers)
        (alet (expand-file-name (shell-quote-argument (f-filename wallpaper)) (f-dirname wallpaper))
          (setq command (concat command (wallpaper--scaling) it " ")))
        (add-to-list 'wallpaper-current-wallpapers wallpaper))
      (start-process-shell-command "Wallpaper" nil command))))

;; I get the feeling the naming system should be more consistent.  We have
;; `wallpaper--random-wallpapers' and `wallpaper--get-available'.  Either we
;; match it with `wallpaper--available-wallpapers` or do
;; `wallpaper--get-random`, no?

;; Also I'm not such a fan of the functionality of
;; `wallpaper--random-wallpapers' relative to its name.  By reading the name I
;; would think it gives me a list of random wallpapers from the available
;; wallpapers; but instead it gives me a random wallpaper for each monitor.  I
;; feel that "monitor" should be in its name if it's a monitor specific
;; function.

;; Surprisingly, there's no interactive function to actually choose a wallpaper.
;; I feel that this is a basic thing that should be here.

;; Choose a wallpaper.  I don't completely understand how there are multiple
;; wallpapers in `wallpaper-set-wallpapers'.  Aren't we just setting one?  The
;; current monitor's?  In any case, I'll follow the idiom.
(defun wallpaper-choose-wallpaper ()
  "Choose a wallpaper for current monitor."
  (interactive)
  (let ((wallpapers (or (wallpaper--per-workspace-wallpapers)
                        wallpaper-static-wallpaper-list
                        (wallpaper--get-available)))
        (wallpaper nil)
        (command (concat "feh --no-fehbg " (wallpaper--background))))
    (if (not wallpapers)
        (message "No wallpapers to choose from.")
      (setq wallpaper-current-wallpapers nil)
      (setq wallpaper (completing-read "wallpaper: " wallpapers))
      (alet (expand-file-name (shell-quote-argument (f-filename wallpaper)) (f-dirname wallpaper))
        (setq command (concat command (wallpaper--scaling) it)))
      (add-to-list 'wallpaper-current-wallpapers wallpaper)
      (start-process-shell-command "Wallpaper" nil command))))



(defun wallpaper--scaling ()
  "Return the wallpaper scaling style to use."
  (cl-case wallpaper-scaling
    (scale "--bg-scale ")
    (max "--bg-max ")
    (fill "--bg-fill ")
    (tile "--bg-tile ")
    (center "--bg-center ")))

(defun wallpaper--background ()
  "Return the background color to use as an argument for feh."
  (concat "--image-bg '" wallpaper-background "' "))



;;;###autoload
(define-minor-mode wallpaper-cycle-mode
  "Toggle Wallpaper Cycle mode.

This mode will activate a timer which will call `wallpaper-set-wallpaper'
at the interval defined by `wallpaper-cycle-interval'.  See function
`wallpaper--toggle-cycle' for more information."
  :lighter " WP"
  :global t
  :group 'wallpaper
  (wallpaper--toggle-cycle))

(defun wallpaper--toggle-cycle ()
  "Stop or start a `wallpaper-set-wallpaper' timer."
  (cancel-function-timers #'wallpaper-set-wallpaper)
  (when wallpaper-cycle-mode
    (run-with-timer 0 wallpaper-cycle-interval #'wallpaper-set-wallpaper)))

(defun wallpaper--random-wallpapers ()
  "Return a string of random wallpapers for each monitor.

If `wallpaper-cycle-single' is non-nil, only one wallpaper is returned."
  (let* ((available (wallpaper--get-available))
         (num-available (length available))
         (num-monitors (if wallpaper-cycle-single 1 (wallpaper--num-monitors)))
         (wallpapers nil))
    (dotimes (_ num-monitors)
      (let ((wallpaper (nth (1- (random num-available)) available)))
        (cl-pushnew wallpaper wallpapers)
        (setq available (delq wallpaper available))))
    wallpapers))

(defun wallpaper--wallpapers ()
  "Return a list of images found in `wallpaper-cycle-directory'."
  (directory-files-recursively wallpaper-cycle-directory
                               wallpaper-cycle-extension-regexp
                               nil))

(defun wallpaper--get-available ()
  "Return `wallpaper--wallpapers' with modification.

This function removes items from `wallpaper-current-wallpapers' from
the resultant list."
  (let ((wallpapers (wallpaper--wallpapers)))
    (dolist (wallpaper wallpaper-current-wallpapers)
      (setq wallpapers (delq wallpaper wallpapers)))
    wallpapers))

(defun wallpaper--num-monitors ()
  "Return the number of connected monitors found by xrandr."
  (length (split-string (shell-command-to-string
                         "xrandr | grep \\* | awk '{print $1}'"))))



;;;###autoload
(define-minor-mode wallpaper-per-workspace-mode
  "Toggle Wallpaper Per Workspace mode.

This mode will set specific wallpapers based on the current workspace.
See `wallpaper-per-workspace-alist' and `wallpaper-per-workspace-get'."
  :lighter " PW"
  :global t
  :group 'wallpaper
  (wallpaper--toggle-per-workspace))

(defun wallpaper--toggle-per-workspace ()
  "Add or remove setting the wallpaper to `exwm-workspace-switch-hook'."
  (if wallpaper-per-workspace-mode
      (progn
        (add-hook 'exwm-workspace-switch-hook #'wallpaper-set-wallpaper)
        (wallpaper-set-wallpaper))
    (remove-hook 'exwm-workspace-switch-hook #'wallpaper-set-wallpaper)))

(defun wallpaper--per-workspace-wallpapers ()
  "Return the wallpapers for the given workspace.

Returns nil if variable `wallpaper-per-workspace-mode' is nil."
  (when wallpaper-per-workspace-mode
    (cdr (assq (funcall wallpaper-per-workspace-get)
               wallpaper-per-workspace-alist))))

(defun wallpaper-per-workspace-exwm-get ()
  "Return the current EXWM workspace."
  (if (boundp 'exwm-workspace-current-index)
      exwm-workspace-current-index
    (error "Cannot get current EXWM workspace!")))

(defun wallpaper-per-workspace-i3-get ()
  "Get the current i3 workspace."
  (if (= (shell-command "pgrep i3") 0)
      (if (executable-find "jq")
          (string-to-number
           (shell-command-to-string
            (concat "i3-msg -t get_workspaces | "
                    "jq -r '.[] | select(.focused==true).name'")))
        (error "External command `jq' is missing!"))
    (error "Window manager `i3' is not in use!")))

(defun wallpaper-per-workspace-vdesk-get ()
  "Get the current vdesk."
  (if (executable-find "vdesk")
      (string-to-number (shell-command-to-string "vdesk"))
    (error "External command `vdesk' is missing!")))



(provide 'wallpaper)

;;; wallpaper.el ends here
