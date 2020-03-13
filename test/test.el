;;; -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'wallpaper)
(require 'ert)

(defun wallpaper-test--static ()
  "Test whether using a static wallpaper list is working."
  (wallpaper-per-workspace-mode -1)
  (wallpaper-cycle-mode -1)
  (setq wallpaper-static-wallpaper-list '("foo"
                                          "bar"))
  (wallpaper-set-wallpaper)
  (not (equal wallpaper-static-wallpaper-list
              wallpaper-current-wallpapers)))

(ert-deftest wallpaper-test-static ()
  (should (wallpaper-test--static)))

(defun wallpaper-test--cycle ()
  "Test whether `wallpaper-cycle-mode' is setting wallpapers properly."
  (wallpaper-per-workspace-mode -1)
  (setq wallpaper-static-wallpaper-list nil
        wallpaper-cycle-directory (expand-file-name
                                   "test/img" (locate-dominating-file
                                               default-directory ".git"))
        wallpaper-cycle-interval 4
        wallpaper-cycle-single t)
  (wallpaper-cycle-mode 1)
  (let ((previous-wallpapers wallpaper-current-wallpapers))
    (sleep-for 6)
    (not (equal wallpaper-current-wallpapers previous-wallpapers))))

(ert-deftest wallpaper-test-cycle ()
  (should (wallpaper-test--cycle)))

(defun wallpaper-test--regexp ()
  "Test whether using a static wallpaper list is working."
  (let* ((wallpaper-cycle-directory (expand-file-name
                                     "test/img" (locate-dominating-file
                                                 default-directory ".git")))
         (wallpapers (wallpaper--wallpapers))
         (expected-1 (expand-file-name "1.png" wallpaper-cycle-directory))
         (expected-2 (expand-file-name "2.jpg" wallpaper-cycle-directory))
         (expected-3 (expand-file-name "3.gif" wallpaper-cycle-directory))
         (expected-4 (expand-file-name "4.jpeg" wallpaper-cycle-directory)))
    (and (member expected-1 wallpapers)
         (member expected-2 wallpapers)
         (member expected-3 wallpapers)
         (member expected-4 wallpapers))))

(ert-deftest wallpaper-test-regexp ()
  (should (wallpaper-test--regexp)))

(defvar wallpaper-test--current-workspace 0
  "Dummy variable for simulating workspace changes.")

(defun wallpaper-test--workspace-set (n)
  "Set `wallpaper-test--current-workspace' to N."
  (setq wallpaper-test--current-workspace n))

(defun wallpaper-test--workspace-get ()
  "Return `wallpaper-test--current-workspace'."
  wallpaper-test--current-workspace)

(defun wallpaper-test--per-workspace ()
  "Ensure per-workspace wallpaper setting is working."
  (wallpaper-cycle-mode -1)
  (setq wallpaper-per-workspace-get #'wallpaper-test--workspace-get
        wallpaper-per-workspace-alist '((0 "foo")
                                        (1 "bar"
                                           "baz")))
  (wallpaper-per-workspace-mode 1)
  (when (equal (wallpaper--per-workspace-wallpapers)
               wallpaper-current-wallpapers)
    (setq wallpaper-test--current-workspace 1)
    (wallpaper-set-wallpaper)
    (dolist (wallpaper wallpaper-current-wallpapers)
      (unless (or (equal wallpaper "bar")
                  (equal wallpaper "bar"))
        wallpaper-test--current-workspace 2)))
  (= wallpaper-test--current-workspace 1))

(ert-deftest wallpaper-test-per-workspace ()
  (should (wallpaper-test--per-workspace)))
