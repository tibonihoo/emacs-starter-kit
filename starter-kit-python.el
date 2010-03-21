;;; starter-kit-python.el --- Some helpful Python code
;;
;; Maybe part of the Emacs Starter Kit ?

(eval-after-load 'python
  '(progn
     (add-hook 'python-mode-hook 'inf-python-keys)
     (define-key python-mode-map (kbd "C-m") 'newline-and-indent)
     (define-key python-mode-map (kbd "C-M-h") 'backward-kill-word)
     (define-key python-mode-map (kbd "C-c l") "lambda")))


;; We never want to edit compiled filed in python mode
(add-to-list 'completion-ignored-extensions ".pyc")


(add-hook 'python-mode-hook 'run-coding-hook)
(add-hook 'python-mode-hook 'idle-highlight)


;;; Flymake

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
     ;; navigate errors returned by flymake
     (define-key python-mode-map "\C-c\C-v" 'my-flymake-show-next-error)
     ))

(add-hook 'python-mode-hook 'flymake-mode)


(provide 'starter-kit-python)
;; starter-kit-python.el ends here
