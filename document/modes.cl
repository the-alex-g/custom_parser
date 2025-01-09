(defvar formatting nil "first element for 'font-lock-defaults'")

(setq formatting
      `(("#\\(.+?\\)#" . (1 'font-lock-comment-face))
	("<.+?>" . 'font-lock-string-face)
	("\\[[^\s]*\\|\\]" . 'font-lock-function-name-face)
	("#" . 'font-lock-comment-delimiter-face)
	("&" . 'font-lock-keyword-face)))

(define-derived-mode brand-mode fundamental-mode "brand"
  "major mode for editing brand language code."
  (setq font-lock-defaults '(formatting)))


(defvar yaml-formatting '((": \\(>\\)" . (1 'font-lock-keyword-face))
			  ("^.+?:" . 'font-lock-variable-name-face))
  "")

(defun builder (list1 list2)
   (if (car list2)
       (builder (cons (car list2) list1) (cdr list2))
       list1))

(setq yaml-formatting (builder formatting yaml-formatting))

(define-derived-mode yaml-mode brand-mode "yaml"
  "major mode for editing yaml code with brand components"
  (setq font-lock-defaults '(yaml-formatting)))
