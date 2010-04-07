;;; starter-kit-cc.el --- Some helpful C-like langages editing
;;  (C,C++,Java... cf "cc-mode")
;;
;; Maybe part of the Emacs Starter Kit ?
;;
;; Tips: You can copy and adapt the following se the following code
;; snippest to further customize you C/C++/ObjC/Java modes, though
;; that may feel awkward for a starter-kit.
;;
;; Enable "hungry delete":
;; use: 
;;     * backspace deletes all contiguous blank spaces in one stroke; 
;;     * C-d performs an "usual" backspace.
;; (c-toggle-hungry-state 1)
;;
;;
;; ;; Custom coding style
;; (require 'cc-my-style)
;; (my-style)
;;
;; The previous snippets can be set in a (add-hook 'c-mode-common-hook
;;  '(lambda () ...)) construct for instance.
;;
;;


(eval-after-load 'cc-mode
  '(progn
     (define-key c-mode-base-map (kbd "C-m") 'newline-and-indent)
     (define-key c-mode-base-map (kbd "C-M-h") 'backward-kill-word)
     (require 'sourcepair)
     (setq sourcepair-recurse-ignore  (append sourcepair-recurse-ignore '(".svn" ".hg" ".git" )))
     (define-key c-mode-base-map (kbd "C-co") 'sourcepair-load)
     ))


(add-hook 'c-mode-common-hook 'run-coding-hook)
(add-hook 'c-mode-common-hook 'idle-highlight)


(add-hook 'c-mode-common-hook
	  '(lambda () 
	     ;;
	     ;;Enable "electric" mode for new lines
	     ;;use:
	     ;;    *automatic indentation and 'end of line' 
	     ;;    after some specific caracters (#,{,...).
	     (c-toggle-auto-newline 1)
	     ;;
	     ;; Launch Hideshow minor mode:
	     ;; use:
	     ;;    *S-Mouse-2 hide/show a block
	     (hs-minor-mode t)
	     ;;
	     ;; Tells in which function you are
	     (which-function-mode)
	     ;;
	     ;; Doxygen help (but don't fail if doxymacs cannot be found)
	     (condition-case err
                 (progn
                   (if (eq system-type 'windows-nt)
                       ;; not all functionalities work nicely on windows 
                       (require 'doxymacs-light)
                     (require 'doxymacs)
                     )
                   ;; set this to true if you have compiled to
                   ;; required external parser...
                   (setq doxymacs-use-external-xml-parser nil)
                   (doxymacs-mode)
                   (doxymacs-font-lock)
                   )
	       (error
		(message "ERROR: Cannot activate doxymacs mode." (cdr err))
		)
	       )
             );; end of c-mode-common-hook
          )


(add-hook 'font-lock-mode-hook
          '(lambda ()
             ;; Fontify doxygen comments in C and C++ modes only.
             (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
                 (doxymacs-font-lock)
               )
             ))



          
;; Cosmetic...
(add-hook 'c++-mode-hook
	  '(lambda ()
	     ;; change name 
	     (setq mode-name "C++")
	     )
	  );; end of c++-mode-hook


(provide 'starter-kit-cc)
;; starter-kit-cc.el ends here

