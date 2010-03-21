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
        yas/hippie-try-expand
	try-expand-dabbrev-all-buffers
        ))

(global-set-key (kbd "C-/") 'hippie-expand)



;; Live completion with auto-complete
;; (
(when (require 'auto-complete nil t)
  (require 'ac-dabbrev)  
  (global-auto-complete-mode t)
  (setq ac-auto-start 3)
  ;; Do What I Mean mode
  (setq ac-dwim t)
  (set-default 'ac-sources
               '(
                 ac-source-words-in-buffer
                 ac-source-symbols
                 ac-source-abbrev
                 ac-source-dabbrev
                 ))
  
  ;; custom keybindings to use tab, enter and up and down arrows
  (define-key ac-complete-mode-map "\t" 'ac-expand)
  (define-key ac-complete-mode-map "\r" 'ac-complete)
  (define-key ac-complete-mode-map "\M-n" 'ac-next)
  (define-key ac-complete-mode-map "\M-p" 'ac-previous)
  )


(provide 'starter-kit-completion)
;;; starter-kit-completion.el ends here
