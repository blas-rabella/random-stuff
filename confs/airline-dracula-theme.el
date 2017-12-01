;;; Code:

(deftheme airline-dracula
  "source: this")

(let ((normal-outer-foreground  "#282a36") (normal-outer-background  "#bd93f9")
      (normal-inner-foreground  "#282a36") (normal-inner-background  "#8d63c9")
      (normal-center-foreground "#ccccc7") (normal-center-background "#181a26")

      (insert-outer-foreground  "#282a36") (insert-outer-background  "#50fa7b")
      (insert-inner-foreground  "#282a36") (insert-inner-background  "#20ca4b")
      (insert-center-foreground "#ccccc7") (insert-center-background "#181a26")
      
      (visual-outer-foreground  "#282a36") (visual-outer-background  "#ffb86c")
      (visual-inner-foreground  "#282a36") (visual-inner-background  "#cf983c")
      (visual-center-foreground "#ccccc7") (visual-center-background "#181a26")
	    
      (replace-outer-foreground  "#282a36") (replace-outer-background  "#ff79c6")
      (replace-inner-foreground  "#282a36") (replace-inner-background  "#cf49a6")
      (replace-center-foreground "#ccccc7") (replace-center-background "#181a26")
	    
      (emacs-outer-foreground  "#282a36") (emacs-outer-background  "#8be9fd")
      (emacs-inner-foreground  "#282a36") (emacs-inner-background  "#4ba9fd")
      (emacs-center-foreground "#eeeeee") (emacs-center-background "#005f87")

      (inactive1-foreground "#565761") (inactive1-background "#f8f8f2")
      (inactive2-foreground "#464752") (inactive2-background "#ccccc7")
      (inactive3-foreground "#373844") (inactive3-background "#a5a5a1"))

  (airline-themes-set-deftheme 'airline-dracula)

  (when airline-cursor-colors
    (progn
     (setq evil-emacs-state-cursor   emacs-outer-background)
     (setq evil-normal-state-cursor  normal-outer-background)
     (setq evil-insert-state-cursor  `(bar ,insert-inner-background))
     (setq evil-replace-state-cursor replace-outer-foreground)
     (setq evil-visual-state-cursor  visual-outer-background)))
)

(airline-themes-set-modeline)

(provide-theme 'airline-dracula)

;;; airline-dracula-theme.el ends here
