;;; wallpaper.el --- Setting the desktop wallpaper -*- lexical-binding: t -*-

;; Copyright (C) 2020

;; Author: Farlado <farlado@sdf.org>
;; URL: https://github.com/farlado/emacs-wallpaper
;; Keywords: unix, wallpaper, extensions
;; Package-Version: 0.0.1
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
;; By default, wallpapers are assumed to be stored in the path
;; $XDG_CONFIG_HOME/wallpapers, but you are free to change this,
;; or set a list of static wallpapers to use instead of randomly
;; choosing what wallpapers to use.  The program used to set the
;; wallpaper is feh, so make sure you have it installed.
;;
;; Compatibility is only guaranteed with use of an X desktop session.



;;; Code:

(unless (executable-find "feh")
  (display-warning 'wallpaper "Could not find feh, is it installed?"))

(unless (eq window-system 'x)
  (display-warning 'wallpaper "This package is meant for X desktop sessions."))



(defgroup wallpaper nil
  "Setting the wallpaper using Emacs."
  :tag "Wallpaper"
  :group 'environment
  :prefix "wallpaper-")



(defcustom wallpaper-static-wallpaper-list nil
  "List of wallpapers to use instead of randomly finding wallpapers.

Wallpapers must be entered in this list as absolute paths, in the order
of your monitors.  This variable should be nil if you intend to use
`wallpaper-cycle-mode'."
  :tag "Static wallpaper(s)"
  :group 'wallpaper
  :type 'list)

(defcustom wallpaper-cycle-interval 15
  "Interval in seconds for cycling between wallpapers in wallpaper slideshows."
  :tag "Wallpaper cycle interval"
  :group 'wallpaper
  :type 'integer)

(defcustom wallpaper-single nil
  "Whether to use one wallpaper across all monitors.

This setting is not respected when `wallpaper-static-wallpapers' is
non-nil.  To have only one wallpaper for all monitors, ensure only
one path is listed in `wallpaper-static-wallpapers'."
  :tag "Single wallpaper"
  :group 'wallpaper
  :type 'boolean)

(defcustom wallpaper-style 'fill
  "What style of wallpaper scaling to use."
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

(defcustom wallpaper-directory (expand-file-name "~/.config/wallpapers")
  "The directory in which to look for wallpapers."
  :tag "Wallpaper directory"
  :group 'wallpaper
  :type 'string)



(defvar wallpaper--current nil
  "List of the wallpaper(s) currently in use.

This variable is set automatically by `wallpaper-set-wallpaper'.  Hand
modification of its value may interfere with its proper behavior.")



(defun wallpaper--style ()
  "Return the style of background to use for images as an argument for feh."
  (case wallpaper-style
    (scale "--bg-scale ")
    (max "--bg-max ")
    (fill "--bg-fill ")
    (tile "--bg-tile ")
    (center "--bg-center ")))

(defun wallpaper--background ()
  "Return the background color to use as an argument for feh."
  (concat "--image-bg '" wallpaper-background "' "))



(defun wallpaper--wallpapers ()
  "Return a list of absolute paths for images found in `wallpaper-directory'."
  (directory-files-recursively wallpaper-directory ".[jpJP][engENG]+$" nil t t))

(defun wallpaper--update-available ()
  "Return the value returned by `wallpaper--wallpapers' with modification.

This function removes the values in the list `wallpaper--current' from its
return value and clears the list as well."
  (let ((wallpapers (wallpaper--wallpapers))
        (current-wallpapers wallpaper--current))
    (setq wallpaper--current nil)
    (dolist (wallpaper current-wallpapers)
      (setq wallpapers (delq wallpaper wallpapers)))
    wallpapers))

(defun wallpaper--num-monitors ()
  "Return the number of connected monitors found by xrandr."
  (length (split-string (shell-command-to-string
                         "xrandr | grep \\* | awk '{print $1}'"))))



(defun wallpaper--random-command ()
  "Return a feh command for random wallpaper assignment."
  (let* ((command (concat "feh " (wallpaper--background)))
         (wallpapers (wallpaper--update-available))
         (num-wallpapers (length wallpapers))
         (num-monitors (if wallpaper-single 1 (wallpaper--num-monitors))))
    ;; Add as many wallpapers to the command as there are monitors
    ;; Add the wallpapers used to `wallpaper--current'
    (dolist (monitor (number-sequence 1 num-monitors))
      (let ((wallpaper (nth (random num-wallpapers) wallpapers)))
        (setq command (concat command (wallpaper--style) wallpaper " ")
              wallpapers (delq wallpaper wallpapers))
        (add-to-list 'wallpaper--current wallpaper)))
    ;; Return the command
    command))

(defun wallpaper--static-command ()
  "Return a feh command from wallpapers in `wallpaper-static-wallpaper-list'."
  (let ((command (concat "feh " (wallpaper--background))))
    ;; Add a wallpaper for each wallpaper in `wallpaper-static-wallpaper-list'
    (dolist (wallpaper wallpaper-static-wallpaper-list)
      (setq command (concat command (wallpaper--style) wallpaper " ")))
    ;; Return the command
    command))



;;;###autoload
(defun wallpaper-set-wallpaper ()
  "Set the wallpaper.

This function will either choose a random wallpaper from
`wallpaper-directory' or use the wallpapers listed in
`wallpaper-static-wallpaper-list'."
  (interactive)
  (start-process-shell-command
   "Wallpaper" nil (if wallpaper-static-wallpaper-list
                       (wallpaper--static-command)
                     (wallpaper--random-command))))



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
  "Stop all existent `wallpaper-set-wallpaper' timers and start a new one if `wallpaper-cycle-mode' is non-nil."
  (cancel-function-timers 'wallpaper-set-wallpaper)
  (when wallpaper-cycle-mode
    (run-with-timer 0 wallpaper-cycle-interval 'wallpaper-set-wallpaper)))



(provide 'wallpaper)

;;; wallpaper.el ends here
