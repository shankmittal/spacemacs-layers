;;; packages.el --- linux-kernel layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Shashank Mittal <mittals@mittals-linux>
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
;; added to `linux-kernel-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `linux-kernel/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `linux-kernel/pre-init-PACKAGE' and/or
;;   `linux-kernel/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst linux-kernel-packages
  '()
  "The list of Lisp packages required by the linux-kernel layer.

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

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(c-add-style "linux-tabs-only"
	     '("linux" (c-offsets-alist
			(arglist-cont-nonempty
			 c-lineup-gcc-asm-reg
			 c-lineup-arglist-tabs-only))))

(defun m/kernel-source-hook ()
  (let ((filename (buffer-file-name)))
    ;; Enable kernel mode for the appropriate files
    (if (and filename
	     (or (string-match (expand-file-name "/local/mnt/workspace/mittals/.*/kernel")
			       filename)
		 (string-match "/local/mnt/workspace/mittals/msm-kvm"
			       filename)
		 (locate-dominating-file filename "Kbuild")
		 (locate-dominating-file filename "Kconfig")
		 (save-excursion (goto-char 0)
				 (search-forward-regexp "^#include <linux/\\(module\\|kernel\\)\\.h>$" nil t))))
	(progn
	  (setq indent-tabs-mode t)
	  (setq tab-width 8)
	  (setq c-basic-offset 8)
	  ;;(message "Setting up indentation for the linux kernel")
	  (c-set-style "linux"))
      (c-set-style "k&r"))))
(add-hook 'c-mode-hook 'm/kernel-source-hook)
(add-hook 'dts-mode-hook
          (lambda ()
            (progn
              (setq indent-tabs-mode t)
              (setq tab-width 8))))
;;; packages.el ends here
