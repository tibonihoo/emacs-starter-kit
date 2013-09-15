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

;; (setq c-paragraph-start "\\("
;;       c-paragraph-start
;;       "\\|"
;;       "//! "
;;       "\\)"))

(setq c-paragraph-start 
      "\\(//!\\|/*\\|^ * \\)"
      )

(eval-after-load 'cc-mode
  '(progn
     (define-key c-mode-base-map (kbd "C-m") 'newline-and-indent)
     (define-key c-mode-base-map (kbd "C-M-h") 'backward-kill-word)
     ;; navigation between corresponding header and source files
     (require 'sourcepair)
     (setq sourcepair-recurse-ignore  (append sourcepair-recurse-ignore '(".svn" ".hg" ".git" )))
     (setq sourcepair-source-path  (append sourcepair-source-path '("../src" "../source" "./src" "./source" "../*" "../../src/*")))
     (setq sourcepair-header-path  (append sourcepair-header-path '("../include" "../inc" "../*" "../../include/*")))
     (define-key c-mode-base-map (kbd "C-c o") 'sourcepair-load)
     ;; Doxygen help (but don't fail if doxymacs cannot be found)
     (condition-case err
         (require 'doxymacs)
       (error
        (progn
          (message "WARNING: Plain doxymacs cannot be loaded, using the light version.")
          (require 'doxymacs-light)
          (setq doxymacs-use-external-xml-parser nil)
          )
        )
       )
     )
  )

(defun hideshow-setup ()
  "Launch hide-show minor mode and setup custom bindings"
  (hs-minor-mode t)
  (define-key c-mode-base-map (kbd "C-c s") 'hs-show-block)
  (define-key c-mode-base-map (kbd "C-c h") 'hs-hide-block)
  (define-key c-mode-base-map (kbd "C-c H") 'hs-hide-all)
  )

(add-hook 'c-mode-common-hook 'run-coding-hook)


(add-hook 'c-mode-common-hook
	  '(lambda () 
	     ;;
	     ;;Enable "electric" mode for new lines
	     ;;use:
	     ;;    *automatic indentation and 'end of line' 
	     ;;    after some specific caracters (#,{,...).
	     (c-toggle-auto-newline 1)
	     ;; Launch Hideshow minor mode
	     (hideshow-setup)
	     ;; Display in which function you are
	     (which-function-mode)
             ;; Use doxymacs
             (doxymacs-mode)
             );; end of c-mode-common-hook
          )


(add-hook 'font-lock-mode-hook
          '(lambda ()
             ;; Fontify doxygen comments in C and C++ modes only.
             (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
                  (condition-case err
                      (doxymacs-font-lock)
                    (error
                     (message "ERROR: Cannot set font decoration for doxygen comments "))
                    )
               )
             ))

(defun add-clang-include-options-for-ede-project (option-list)
  "Generate the string of -I's denoting all include dir, deduced from ede config for project if any. May return nil."
  (let* (
         (current-dir (file-name-directory
                       (or (buffer-file-name (current-buffer)) default-directory)))
         (prj (ede-current-project current-dir))
         (current-include-dirs (oref prj include-path))
         (current-project-root-dir (ede-project-root-directory prj))
         (separator-with-root (concat "-I" current-project-root-dir))
         )
    (when current-include-dirs
      (dolist (path current-include-dirs) (push (concat separator-with-root path) option-list))
      (identity option-list) 
      )
    )
  )


(when (eq system-type 'darwin)
  (require 'flymake)
  ;; adapted from https://github.com/dmacvicar/duncan-emacs-setup
  (defun flymake-clang-c++-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name)))
           (option-list (add-clang-include-options-for-ede-project (list "-fsyntax-only" "-fno-color-diagnostics" "-x" "c++" local-file)))
           )
      ;;(message option-list)
      (list "clang++" option-list)
      )
    )
  (defun flymake-clang-c++-load ()
    (interactive)
    (unless (eq buffer-file-name nil)
      (add-to-list 'flymake-allowed-file-name-masks
                   '("\\.cpp\\'" flymake-clang-c++-init))
      (add-to-list 'flymake-allowed-file-name-masks
                   '("\\.cc\\'" flymake-clang-c++-init))
      (add-to-list 'flymake-allowed-file-name-masks
                   '("\\.h\\'" flymake-clang-c++-init))
      (add-to-list 'flymake-allowed-file-name-masks
                   '("\\.hpp\\'" flymake-clang-c++-init))
      (flymake-mode t))
    )
  (add-hook 'c++-mode-hook 'flymake-clang-c++-load)
  )

          
;; Cosmetic...
(add-hook 'c++-mode-hook
	  '(lambda ()
	     ;; change name 
	     (setq mode-name "C++")
	     )
	  );; end of c++-mode-hook


(provide 'starter-kit-cc)
;; starter-kit-cc.el ends here

