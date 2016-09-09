;;; packages.el --- helm-changes layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Mittal <mittals@mittals-mac>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `helm-changes-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `helm-changes/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `helm-changes/pre-init-PACKAGE' and/or
;;   `helm-changes/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst helm-changes-packages
  '(helm-types)
  "The list of Lisp packages required by the helm-changes layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

;; (require 'helm-types)
(defun helm-buffer-switch-to-new-window (_candidate)
  "Display buffers in new windows."
  ;; Select the bottom right window
  (require 'winner)
  ;; (select-window (car (last (winner-sorted-window-list))))
  ;; Display buffers in new windows
  (dolist (buf (helm-marked-candidates))
    (select-window (split-window-right))
    (switch-to-buffer buf))
  ;; Adjust size of windows
  (balance-windows))

(defun helm-buffer-switch-new-window ()
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-buffer-switch-to-new-window)))

(defun helm-buffer-switch-to-new-window-h (_candidate)
  "Display buffers in new windows."
  ;; Select the bottom right window
  (require 'winner)
  ;; (select-window (car (last (winner-sorted-window-list))))
  ;; Display buffers in new windows
  (dolist (buf (helm-marked-candidates))
    (select-window (split-window-below))
    (switch-to-buffer buf))
  ;; Adjust size of windows
  (balance-windows))

(defun helm-buffer-switch-new-window-h ()
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-buffer-switch-to-new-window-h)))

(eval-after-load "helm-buffers"
  '(progn
     (define-key helm-buffer-map (kbd "C-v") #'helm-buffer-switch-new-window)))

(eval-after-load "helm-buffers"
  '(progn
     (define-key helm-buffer-map (kbd "C-s") #'helm-buffer-switch-new-window-h)))

(defun helm-file-switch-to-new-window (_candidate)
  "Display buffers in new windows."
  ;; Select the bottom right window
  (require 'winner)
  ;; (select-window (car (last (winner-sorted-window-list))))
  ;; Display buffers in new windows
  (dolist (buf (helm-marked-candidates))
    (select-window (split-window-right))
    (find-file buf))
  ;; Adjust size of windows
  (balance-windows))

(defun helm-file-switch-new-window ()
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-file-switch-to-new-window)))

(defun helm-file-switch-to-new-window-h (_candidate)
  "Display buffers in new windows."
  ;; Select the bottom right window
  (require 'winner)
  ;; (select-window (car (last (winner-sorted-window-list))))
  ;; Display buffers in new windows
  (dolist (buf (helm-marked-candidates))
    (select-window (split-window-below))
    (find-file buf))
  ;; Adjust size of windows
  (balance-windows))

(defun helm-file-switch-new-window-h ()
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-file-switch-to-new-window-h)))


(eval-after-load "helm-files"
  '(progn
    (define-key helm-find-files-map (kbd "C-v") #'helm-file-switch-new-window)
    (define-key helm-find-files-map (kbd "C-s") #'helm-file-switch-new-window-h)))

(eval-after-load "helm-locate"
  '(progn
     (define-key helm-generic-files-map (kbd "C-v") #'helm-file-switch-new-window)
     (define-key helm-generic-files-map (kbd "C-s") #'helm-file-switch-new-window-h)))
;;; packages.el ends here
