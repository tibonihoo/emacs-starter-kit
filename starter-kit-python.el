;;; starter-kit-python.el --- Some helpful Python code
;;
;; Maybe part of the Emacs Starter Kit ?

(eval-after-load 'python
  '(progn
     (define-key python-mode-map (kbd "C-m") 'newline-and-indent)
     (define-key python-mode-map (kbd "C-M-h") 'backward-kill-word)
     (define-key python-mode-map (kbd "C-c l") "lambda")))


;; We never want to edit compiled filed in python mode
(add-to-list 'completion-ignored-extensions ".pyc")

(defun ac-python-mode-setup ()
  (setq ac-sources (append '(ac-source-yasnippet) ac-sources)))


(add-hook 'python-mode-hook 'run-coding-hook)
(add-hook 'python-mode-hook 'idle-highlight)
(add-hook 'python-mode-hook 'ac-python-mode-setup)


;; Pymacs (http://pymacs.progiciels-bpi.ca/index.html)
;; Makes it possible to interface emacs lisp with some python code.
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(eval-after-load 'pymacs
  '(progn
     (add-to-list 'pymacs-load-path
                  (concat dotfiles-dir "/elpa-to-submit/pymacs/lib"))
     (add-to-list 'load-path
                  (concat dotfiles-dir "/elpa-to-submit/pymacs/lib"))
     ))


;; ;; Ropemacs (http://rope.sourceforge.net/ropemacs.html)
;; ;; To get some live (intelligent) completion, documentation and
;; ;; refactoring function.
;; (eval-after-load 'python
;;   '(progn
;;      ;; Setup auto-complete to use Rope
;;      (ac-ropemacs-enable) 
;;      (setq ropemacs-enable-autoimport t)
;;      (setq ropemacs-enable-shortcuts nil)
;;      (setq ropemacs-guess-project t)
;;      (add-hook 'python-mode-hook 'ropemacs-mode)
;;      (define-key python-mode-map "\C-cds" 'rope-show-doc)
;;      ))


;;; Flymake
;; ~Static code checking
(eval-after-load 'python
  '(progn
     (require 'flymake)
     ;; Taken from http://plope.com/Members/chrism/flymake-mode
     ;;
     ;; works with http://pypi.python.org/pypi/pyflakes (v>3.0)
     ;;
     ;; The first stanza tells flymake to use pyflakes on .py files
     ;; when in flymake-mode. The second tells emacs to always use
     ;; flymake as a minor mode when loading buffers so I don't have
     ;; to type "M-x flymake-mode" to get syntax checking when I load
     ;; a buffer.
     (defun flymake-pyflakes-init () 
       (let* ((temp-file (flymake-init-create-temp-buffer-copy 
                          'flymake-create-temp-inplace)) 
              (local-file (file-relative-name 
                           temp-file 
                           (file-name-directory buffer-file-name)))) 
         (list "pyflakes" (list local-file)))) 
     
     (add-to-list 'flymake-allowed-file-name-masks 
                  '("\\.py\\'" flymake-pyflakes-init))
     ))
(add-hook 'python-mode-hook 'flymake-mode)



(provide 'starter-kit-python)
;; starter-kit-python.el ends here
