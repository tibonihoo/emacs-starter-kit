;;; starter-kit-latex.el --- Configuration for LaTeX document editing.
;;
;; Maybe part of the Emacs Starter Kit ?


;; AucTeX mode
(when (locate-library "tex-site")
  ;; Tackle the problem that AUC TeX will produce error-messages if the
  ;; function `set-text-properties' is defined.
  (fset 'set-text-properties (symbol-function 'ignore))
  (require 'tex-site)
  (require 'preview)
  ;; Specific syntax highlighting
  (require 'font-latex)
  )

;; RefTeX configuration
;; Eases handling of references (\ref, \label, \cite and the like)
(autoload 'reftex-mode "reftex" "RefTeX minor mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" t)
;;;###autoload
(defun reftex-local-setup ()
  (setq reftex-extra-bindings t)	; use additional C-c bindings
  (setq reftex-enable-partial-scans t)	; only parse current file
  (setq reftex-save-parse-info t)	; save parsing results
  (setq reftex-use-multiple-selection-buffers t) ; diff buffs for label select.
  (setq reftex-plug-into-AUCTeX t)  ; interact with AucTeX, of course/
  (setq reftex-cite-format (quote default))

)

;; Format accents in LaTeX's way
;;;###autoload
(defun latex-transform-accents()
  "Transform special caracters into a generic way undersdtandable by LaTeX"
  (interactive)
  ;; upercase first (because of the way 'replace-string works)
  (beginning-of-buffer)
  (replace-string "É" "\\'E")
  (beginning-of-buffer)
  (replace-string "È" "\\`E")
  (beginning-of-buffer)
  (replace-string "Ê" "\\^E")
  (beginning-of-buffer)
  (replace-string "À" "\\`A")
  (beginning-of-buffer)
  (replace-string "Â" "\\^A")
  (beginning-of-buffer)
  (replace-string "Ü" "\\\"U")
  (beginning-of-buffer)
  (replace-string "Û" "\\^U")
  (beginning-of-buffer)
  (replace-string "Ï" "\\\"I")
  (beginning-of-buffer)
  (replace-string "Î" "\\^I")
  (beginning-of-buffer)
  (replace-string "Ù" "\\`U")
  (beginning-of-buffer)
  (replace-string "Ç" "\\c{C}")
  (beginning-of-buffer)
  (replace-string "Ñ" "\\~N")
  (beginning-of-buffer)
  (replace-string "Œ" "{\\OE}")
  (beginning-of-buffer)
  (replace-string "Ô" "\\^O")
  ;;lower case
  (beginning-of-buffer)
  (replace-string "œ" "{\\oe}")
  (beginning-of-buffer)
  (replace-string "é" "\\'e")
  (beginning-of-buffer)
  (replace-string "è" "\\`e")
  (beginning-of-buffer)
  (replace-string "ê" "\\^e")
  (beginning-of-buffer)
  (replace-string "à" "\\`a")
  (beginning-of-buffer)
  (replace-string "â" "\\^a")
  (beginning-of-buffer)
  (replace-string "ü" "\\\"u")
  (beginning-of-buffer)
  (replace-string "û" "\\^u")
  (beginning-of-buffer)
  (replace-string "ï" "\\\"i")
  (beginning-of-buffer)
  (replace-string "î" "\\^i")
  (beginning-of-buffer)
  (replace-string "ù" "\\`u")
  (beginning-of-buffer)
  (replace-string "ç" "\\c{c}")
  (beginning-of-buffer)
  (replace-string "ñ" "\\~n")
  (beginning-of-buffer)
  (replace-string "ô" "\\^o")
  ;; very special chars
  (beginning-of-buffer)
  (replace-string "°" "$^{\\circ}$")

  )
;;;###autoload
(defun latex-transform-accents-back()
  "Reverse operation compared to transform-accents"
  (interactive)
  ;; upercase first (because of the way 'replace-string works)
  (beginning-of-buffer)
  (replace-string "\\'E" "É")
  (beginning-of-buffer)
  (replace-string "\\`E" "È")
  (beginning-of-buffer)
  (replace-string "\\^E" "Ê")
  (beginning-of-buffer)
  (replace-string "\\`A" "À")
  (beginning-of-buffer)
  (replace-string "\\^A" "Â")
  (beginning-of-buffer)
  (replace-string "\\\"U" "Ü")
  (beginning-of-buffer)
  (replace-string "\\^U" "Û")
  (beginning-of-buffer)
  (replace-string "\\\"I" "Ï")
  (beginning-of-buffer)
  (replace-string "\\^I" "Î")
  (beginning-of-buffer)
  (replace-string "\\`U" "Ù")
  (beginning-of-buffer)
  (replace-string "\\c{C}" "Ç")
  (beginning-of-buffer)
  (replace-string "\\~N" "Ñ")
  (beginning-of-buffer)
  (replace-string "{\\OE}" "Œ")
  (beginning-of-buffer)
  (replace-string "\\^O" "Ô")
  ;; lower case:
  (beginning-of-buffer)
  (replace-string "{\\oe}" "œ")
  (beginning-of-buffer)
  (replace-string "\\'e" "é")
  (beginning-of-buffer)
  (replace-string "\\`e" "è")
  (beginning-of-buffer)
  (replace-string "\\^e" "ê")
  (beginning-of-buffer)
  (replace-string "\\`a" "à")
  (beginning-of-buffer)
  (replace-string "\\^a" "â")
  (beginning-of-buffer)
  (replace-string "\\\"u" "ü")
  (beginning-of-buffer)
  (replace-string "\\^u" "û")
  (beginning-of-buffer)
  (replace-string "\\\"i" "ï")
  (beginning-of-buffer)
  (replace-string "\\^i" "î")
  (beginning-of-buffer)
  (replace-string "\\`u" "ù")
  (beginning-of-buffer)
  (replace-string "\\c{c}" "ç")
  (beginning-of-buffer)
  (replace-string "\\~n" "ñ")
  (beginning-of-buffer)
  (replace-string "\\^o" "ô")
  ;; very special chars
  (beginning-of-buffer)
  (replace-string "$^{\\circ}$" "°")
  )


;; Skeletons (TODO: use yasnippets ?)
;;;###autoload
(defun set-skeletons-for-latex ()
  (interactive)
  ;; Skeleton definitions
  (define-skeleton latex-insert-includegraphics
    "Inserts an includegraphics" 
    nil 
    "\\includegraphics[width=\\textwidth,angle=0]{"_"}"
    )
  ;; abbrevs
  (abbrev-mode t)
  ;; key binding
  (define-key	LaTeX-mode-map	"\M-s" 'latex-insert-includegraphics)
  (define-key LaTeX-mode-map "{" 'skeleton-pair-insert-maybe)
  )


;; Smarter LaTeX compilation (when using a Makefile)
;;;###autoload
(defun LaTeX-compile ()
  "An interface to `compile'.
It calls `compile' and define the compile command depending on
the presence of a makefile. Takes into account the master-file
system as defined by AucTeX for multiple file documents."
  (interactive)
  (let ((master-name (expand-file-name (TeX-master-file)))
        (makefile-candidate_u (concat (file-name-directory (expand-file-name (TeX-master-file))) "Makefile"))
	(makefile-candidate_l (concat (file-name-directory (expand-file-name (TeX-master-file))) "Makefile"))
	(use-tex-command nil)
	)

    (if (not master-name)(error "cannot get master's filename."))

    (cond
     ;; Makefile ?
     ((file-readable-p makefile-candidate_u)     
      (set (make-local-variable 'compile-command) 
	   (concat "cd `dirname " makefile-candidate_u "`; make ref"))
      )

     ;; makefile ?
     ((file-readable-p makefile-candidate_u)
      (set (make-local-variable 'compile-command) 
	   (concat "cd `dirname " makefile-candidate_u "`; make ref"))
     )

     ;; else
     (t
      (if (and (local-variable-p 'compile-command)
	       compile-command)
	    (setq use-tex-command t))
      )
     )
     ;; compile
    (if (not use-tex-command)
	(call-interactively 'compile)
      (TeX-command-master)
      )
    )
  )


;; LaTeX hook
;;;###autoload
(defun starter-kit-latex-hook ()
  "Config LaTeX"
  ;; Use RefTeX
  (reftex-local-setup)
  (turn-on-reftex)
  ;; Generate and visualise PDF by default
  (TeX-PDF-mode t)
  ;; Live spell check
  (flyspell-mode)
  ;; Auto fill
  (turn-on-auto-fill) 
  ;; Add a menu with section names
  (imenu-add-to-menubar "Index")
  ;; Enable skeletons
  (set-skeletons-for-latex)
  ;; More keybindings
  (define-key LaTeX-mode-map "\C-ca" 'latex-transform-accents)
  (define-key LaTeX-mode-map "\C-cz" 'latex-transform-accents-back)
  (define-key LaTeX-mode-map "\C-c." 'LaTeX-close-environment)
  (define-key LaTeX-mode-map "\C-c\C-c" 'LaTeX-compile)
  ;; Add items to Command menu
  (add-to-list 'TeX-command-list
	       (list "PDFLaTeX" "pdflatex %s.tex"
		     'TeX-run-command nil t))
  (add-to-list 'TeX-command-list
	       (list "DVIPS" "dvips -o %s.ps %s.dvi"
		     'TeX-run-command nil t))
  (add-to-list 'TeX-command-list
	       (list "Clean" "rm %s.log %s.aux %s.out %s.idx"
		     'TeX-run-command nil t))

  (if (string-match "Carbon" (emacs-version))
      (progn
	(add-to-list 'TeX-output-view-style
		     (list "^pdf$" "." "open %o"))
	(add-to-list 'TeX-command-list
		     (list "View PDF document" "open %s.pdf"
			   'TeX-run-command nil t))
	)
    (progn
      (add-to-list 'TeX-output-view-style
		   (list "^pdf$" "." "gnome-open %o"))
      (add-to-list 'TeX-command-list
		   (list "View PDF document" "gnome-open %s.pdf"
			 'TeX-run-command nil t))
      )	
    )
  )

(add-hook 'LaTeX-mode-hook 'starter-it-latex-hook)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)


(provide 'starter-kit-latex)

