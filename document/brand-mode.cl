(defvar formatting nil "first element for 'font-lock-defaults'")

(setq formatting
      `(("\\#\\([^<]+?\\)\\#" . (1 'font-lock-comment-face))
	("\\[[^\s]*\\|\\]" . 'font-lock-function-name-face)
	("\\#" . 'font-lock-comment-delimiter-face)
	("\\&" . 'font-lock-keyword-face)))

(define-derived-mode brand-mode fundamental-mode "brand"
  "major mode for editing brand language code."
  (setq font-lock-defaults '(formatting)))
