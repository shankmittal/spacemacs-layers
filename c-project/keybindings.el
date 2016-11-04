;; We define prefix commands only for the sake of which-key
(setq spacemacs/key-binding-prefixes '(("S"   "shashank-prefix")
                                       ("Sc"  "create c project")))
(mapc (lambda (x) (apply #'spacemacs/declare-prefix x))
      spacemacs/key-binding-prefixes)

;;(spacemacs/declare-prefix ("S" "shashank-prefix"))
;;(spacemacs/set-leader-keys "Sc" (lambda() (interactive) (c-project/create)))

(defun c-project-create () (interactive) (c-project/create))
(spacemacs/set-leader-keys "Sc" 'c-project-create)
