;;; starter-kit-completion.el --- A few common completion tricks
;;
;; Maybe part of the Emacs Starter Kit ?


;; -----------------------------------------------------------------------------
;; AUTO INSERTION OF MATCHING SYMBOLS
;; -----------------------------------------------------------------------------
(setq skeleton-pair t)
(define-key global-map "(" 'skeleton-pair-insert-maybe)
(define-key global-map "[" 'skeleton-pair-insert-maybe)
(define-key global-map "{" 'skeleton-pair-insert-maybe)


;; -----------------------------------------------------------------------------
;; On demand completion
;; -----------------------------------------------------------------------------
;; Code snippets with YaSnippets
(require 'yasnippet-bundle)

(setq hippie-expand-try-functions-list
      '(
        try-expand-dabbrev-visible
	try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-expand-all-abbrevs
	try-complete-file-name-partially
	try-complete-file-name
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol
	try-expand-whole-kill
        ))

(global-set-key (kbd "C-/") 'hippie-expand)


(defun ac-eshell-mode-setup ()
  (add-to-list 'ac-sources 'ac-source-files-in-current-dir))

;; Live completion with auto-complete
;; (see http://cx4a.org/software/auto-complete/)
(when (require 'auto-complete-config nil t)
  (require 'ac-dabbrev)  
  (global-auto-complete-mode t)
  (setq ac-auto-start .5)
  (setq ac-quick-help-delay 0.5)
  ;; Do What I Mean mode
  (setq ac-dwim t)
  (ac-config-default)
  ;; set also the completion for eshell  
  (add-hook 'eshell-mode-hook 'ac-eshell-mode-setup)
  ;; custom keybindings to use tab, enter and up and down arrows
  (define-key ac-complete-mode-map "\t" 'ac-expand)
  (define-key ac-complete-mode-map "\r" 'ac-complete)
  (define-key ac-complete-mode-map "\M-n" 'ac-next)
  (define-key ac-complete-mode-map "\M-p" 'ac-previous)
  )


(provide 'starter-kit-completion)
;;; starter-kit-completion.el ends here
