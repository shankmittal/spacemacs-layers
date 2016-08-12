;;; packages.el --- coreboot layer packages file for Spacemacs.
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
;; added to `coreboot-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `coreboot/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `coreboot/pre-init-PACKAGE' and/or
;;   `coreboot/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst coreboot-packages
  '()
  "The list of Lisp packages required by the coreboot layer.

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


(defun m/coreboot-tags-hook ()
  (let ((filename (buffer-file-name)))
    (if (and filename
	     (string-match ".*/src/third_party/coreboot/*"
			   (expand-file-name filename)))
	(progn
	  (message "Setting GTAGSLIBPATH for CROS Coreboot Project.")
	  (let ((coreboot-path (replace-regexp-in-string "third_party/coreboot/.*" "third_party/coreboot" (expand-file-name filename))))
	    (message coreboot-path)
	    (let ((atf-path (replace-regexp-in-string "third_party/coreboot" "third_party/arm-trusted-firmware" coreboot-path))
		  (vboot-path (replace-regexp-in-string "third_party/coreboot" "platform/vboot_reference" coreboot-path)))
	      (message atf-path)
	      (message vboot-path)
	      (make-local-variable 'process-enviroment)
	      (message (format "GTAGSLIBPATH=%s:%s:%s" coreboot-path atf-path vboot-path))
	      (setenv "GTAGSLIBPATH" (format "%s:%s:%s" coreboot-path atf-path vboot-path))
	      (setq process-enviroment (format "GTAGSLIBPATH=%s:%s:%s" coreboot-path atf-path vboot-path))))))
    ))
(add-hook 'c-mode-hook 'm/coreboot-tags-hook)
;;; packages.el ends here
