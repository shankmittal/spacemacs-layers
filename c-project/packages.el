;;; packages.el --- c-project layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: mittals <mittals@mittals-work>
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
;; added to `c-project-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `c-project/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `c-project/pre-init-PACKAGE' and/or
;;   `c-project/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst c-project-packages
  '(helm-gtags)
  "The list of Lisp packages required by the c-project layer.

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


;;; packages.el ends here

(defun c-project/get-path () (read-file-name "Enter C-Project Name: "))

(defun c-project/create-projectile-file (project-dir)
  (let ((file (concat project-dir "/.projectile")))
    (write-region "" "" file)))

(defun c-project/copy-template (name template project-dir project-name)
  (cl-flet ((substitute (old new) (let ((case-fold-search nil))
                                    (save-excursion
                                      (goto-char (point-min))
                                      (while (search-forward old nil t)
                                        (replace-match new t))))))
    (let ((src (concat "~/spacemacs_layers/template" "/"
                       (format "%s.template" template)))
          (dest (concat project-dir "/" name)))
      (copy-file src dest)
      (find-file dest)
      (substitute "%PROJECT_NAME%" project-name)
      (save-buffer))))

  (defun c-project/create () (let ((name (c-project/get-path)))
                               (message (format "Porject name: %s" name))
                               (if (file-exists-p name)
                                   (message "File exists. Can't create project!!")
                                 (progn (make-directory name t)
                                        (c-project/create-projectile-file name)
                                        (c-project/copy-template "Makefile" "Makefile" name (file-name-nondirectory name))
                                        (c-project/copy-template (concat (file-name-nondirectory name) ".c") "c" name (file-name-nondirectory name))
                                        (helm-gtags-create-tags name "defualt")))))

  (spacemacs/declare-prefix "S" "shashank-prefix")
  (spacemacs/set-leader-keys "Sc" (lambda() (interactive) (c-project/create)))
