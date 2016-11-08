;;; packages.el --- helm-gtags-hook layer packages file for Spacemacs.
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
;; added to `helm-gtags-hook-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `helm-gtags-hook/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `helm-gtags-hook/pre-init-PACKAGE' and/or
;;   `helm-gtags-hook/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst helm-gtags-hook-packages
  '()
  "The list of Lisp packages required by the helm-gtags-hook layer.

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

;; enable helm-gtags-mode
(add-hook 'c-mode-hook   'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; key bindings
(eval-after-load "helm-gtags"
  '(progn
     ;;(define-key evil-motion-state-map (kbd "\C-]") 'helm-gtags-find-tag)
     (define-key evil-normal-state-map (kbd "\C-]") 'helm-gtags-find-tag)
     (define-key evil-normal-state-map (kbd "\C-t") 'helm-gtags-previous-history)
     (define-key evil-normal-state-map (kbd "\C-y") 'helm-gtags-next-history)
     (define-key evil-normal-state-map (kbd "\C-\\") 'helm-gtags-find-rtag)
     ;; (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
     ;; (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
   ))
;;; packages.el ends here
