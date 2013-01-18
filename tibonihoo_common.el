;;
;; Common part of my customizations on top of starter-kit
;;
;; (c) 2010-2013 Thibauld Nion
;;

;; TODO: make the doxygen and C++ style 100% depends on the style class (for now I still have to redefine a
;; couple of functions in the <usename>.el

;; Template for <username>.el :
;; ;; Personal info for signatures in code and stuff...
;; (set 'user-full-name "Thibauld Nion")
;; (set 'user-mail-address "tnion@dxo.com")
;; ;; Work context
;; (defvar my-work-context "DO")

;; (if (eq system-type 'windows-nt)
;;     (setq backup-directory-alist `(("." . ,(expand-file-name "d:/Perso/emacs/backups"))))
;;   )

;; (if (eq system-type 'windows-nt)
;;     (progn
;;       ;; Set prefered frame size
;;       (add-to-list 'default-frame-alist '(height . 59))
;;       (add-to-list 'default-frame-alist '(width . 100))
;;       (add-to-list 'default-frame-alist '(top + -3))
;;       (add-to-list 'default-frame-alist '(left + -1))
;;       )
;;   (progn
;;     ;; for my linux workstation
;;     (frame-set-fullscreen-off)
;;     (add-to-list 'default-frame-alist '(height . 66))
;;     (add-to-list 'default-frame-alist '(width . 281))
;;     ))

;; (if (eq system-type 'windows-nt)
;;     (set-face-attribute 'default nil :family "Consolas" :height 105)
;;   ;; else use lucida
;;   (set-face-attribute 'default nil :family "b&h-lucidatypewriter")
;;   )

;; (add-hook 'c-mode-common-hook
;;           '(lambda () 
;;              ;; Enable "hungry delete":
;;              ;; use: 
;;              ;;    *backspace deletes all contiguous blank spaces in one stroke; 
;;              ;;    * C-d performs an "usual" backspace.
;;              ;;
;;              (c-toggle-hungry-state 1)
;;              ;; Use custom c/c++ style
;;              (require 'cc-do-style)
;;              (do-style)
;;              (require 'doxymacs-do-style)
;;              ;;
;;              ;; Add a new function for doxygen comments
;;              (defun doxymacs-insert-subgrouping-comments (start end)
;;                "Inserts doxygen subgrouping comments around the current region."
;;                (interactive "*r")
;;                (let* (
;;                       (groupname 
;;                        (read-from-minibuffer "Enter name of the subgroup: "))
;;                       (starter-template '(
;;                                           "  //! " (doxymacs-doxygen-command-char) "name " groupname > n
;;                                           "  //! " > n
;;                                           "  //! @{" > n > n

;;                                           ))
;;                       (ender-template '( n "  // doxysubgroup: " groupname > n
;;                                            "  //! @} " > n >
;;                                            ))
;;                       )
;;                  (save-excursion
;;                    (goto-char end)
;;                    (end-of-line)
;;                    ;;       (insert ender)
;;                    (tempo-insert-template 'ender-template tempo-insert-region)
;;                    (goto-char start)
;;                    (beginning-of-line)
;;                    (tempo-insert-template 'starter-template tempo-insert-region)
;;                    ;;       (insert starter)
;;                    )))
;;              ;; (Re)define some keybindings
;;              ;; Groups ("g" is easier than @ on fr kbds)
;;              (define-key doxymacs-mode-map "\C-cdg"
;;                'doxymacs-insert-grouping-comments)
;;              ;; Subgroupiong with the  @name command
;;              (define-key doxymacs-mode-map "\C-cdn" 
;;                'doxymacs-insert-subgrouping-comments)
;;              ;; ;; semantic
;;              ;; (semantic-mode)
;;              ;; (semantic-idle-scheduler-mode)
;;              ;; (add-to-list 'semanticdb-project-roots "d:/DATA/WORK/Framework-trunk")
;;              ;; (add-to-list 'semanticdb-project-roots "d:/DATA/WORK/Framework-2-x")
;;              ;; (add-to-list 'ac-sources 'ac-source-semantic)
;;              (set-compilation-regexp-alist-for-msbuild)
;;              ;; trigger other common coding hooks
;;              (run-coding-hook)
;;              ))
;;
;; Always at the end !
;; (require 'tibonihoo_common)

;; --- End of template



;; -----------------------------------------------------------------------------
;; Version control modes triggers (autoloads)
;; -----------------------------------------------------------------------------
(autoload 'svn-status "psvn" nil t)
(autoload 'ahg-status "ahg" nil t)



;; -----------------------------------------------------------------------------
;; Get a behaviour closer to all other apps
;; -----------------------------------------------------------------------------
;; Make commands like C-c C-x C-v work like in every other 'modern'
;; editor (ie copy, cut, paste) WHEN a region is active and behave
;; emacs like else.
(cua-mode t)
;; Binding for full screen
(global-set-key (kbd "<f11>") 'toggle-fullscreen)
;; Replace highlighted/marked areas
(delete-selection-mode t)
;; Tab navigation among buffers
(require 'ibs)


;; -----------------------------------------------------------------------------
;; Colors ! colors everywhere...
;; -----------------------------------------------------------------------------
;; Custom color theme
(color-theme-tango)
(global-hl-line-mode)

(add-hook 'coding-hook 'idle-highlight)


;; -----------------------------------------------------------------------------
;; Dictionary definitions for ispell
;; -----------------------------------------------------------------------------
(setq-default ispell-program-name "aspell")
;; Set to the french dictionary by default
(setq flyspell-default-dictionary "francais")
;; Utily functions of quick switches
(defun dico-fr ()
  "switch ispell language to francais"
  (interactive)
  (ispell-change-dictionary "francais"))

(defun dico-en ()
  "switch ispell language to british"
  (interactive)
  (ispell-change-dictionary "british"))
;; a handy shortcut for fly-spell This way you have: C-, goto next
;; mispelled word (default) M-o correct current word (instead of
;; M-TAB)
(global-set-key "\M-o" 'flyspell-auto-correct-word)


(require 'windmove)

;; swap function taken from emacsd-tile.el at http://gist.github.com/287633
(defun swap-window-with (dir)
  (interactive)
  (let ((other-window (windmove-find-other-window dir)))
    (when other-window
      (let* ((this-window (selected-window))
             (this-buffer (window-buffer this-window))
             (other-buffer (window-buffer other-window))
             (this-start (window-start this-window))
             (other-start (window-start other-window)))
        (set-window-buffer this-window other-buffer)
        (set-window-buffer other-window this-buffer)
        (set-window-start this-window other-start)
        (set-window-start other-window this-start)))))

;; swap down and up
(global-set-key (kbd "C-M-j") (lambda () (interactive) (swap-window-with 'down)))
(global-set-key (kbd "C-M-k") (lambda () (interactive) (swap-window-with 'up)))
;; swap left and right
(global-set-key (kbd "C-M-h") (lambda () (interactive) (swap-window-with 'left)))
(global-set-key (kbd "C-M-l") (lambda () (interactive) (swap-window-with 'right)))



;; -----------------------------------------------------------------------------
;; Misc customisations
;; -----------------------------------------------------------------------------

;; Flymake additional functionalities
(eval-after-load 'flymake
  '(progn
     (require 'flymake-add)
     ))

(add-hook 'flymake-mode-hook
          '(lambda ()
             (local-set-key (kbd "M-n") 'flymake-goto-next-error)
             (local-set-key (kbd "M-p") 'flymake-goto-prev-error)
             ))
  


;; -----------------------------------------------------------------------------
;; Python personal config
;;-----------------------------------------------------------------------------
(eval-after-load 'python
  '(progn
     (setq python-indent 2)
     (setq indent-tabs-mode nil)
     (setq python-guess-indent nil)
     (idle-highlight)
     ;; Define some handy snippets (requires yasnippet or yasnippet-bundle)
     (yas/define-snippets 'python-mode
                          '(
                            ("tnc" "# `(user-login-name)`: NOCOMMIT" "Mark as uncommitable" nil)
                            ("tntd" "# `(user-login-name)`: TODO" "todo stamp" nil)
                            ("ncprint" "print $0 # `(user-login-name)`: NOCOMMIT" "no commit print" nil)
                            ("ptd" "pass # `(user-login-name)`: TODO" "Insert the default shebang for python "nil)
                            ("encoding" "# -*- coding: utf-8 -*-
" "Set the file encoding" nil)
                            ("shebang" "#!/usr/bin/env python
" "Insert the default shebang for python "nil)
                            )
                          )
     ;; imenu settings: also show function's args
     (setq py-imenu-show-method-args-p t)
     ;; A simple way to lookup a subject (so long as you're connected
     ;; to internet, else you can still use C-c C-f
     (defun python-search-www ()
       "Launch web search"
       (interactive)
       (let* (
              (userQuery
               (read-from-minibuffer "Enter search term: "))
              (escapedQuery 
               (replace-regexp-in-string " " "+" userQuery))
              (weburl 
               (concat "http://docs.python.org/search.html?q=" escapedQuery))
              )
         (browse-url weburl)
         )
       )
     (define-key python-mode-map "\C-xt" 'python-test-this)
     (condition-case err
         (progn
           ;; Doxygen mode
           (require 'doxymacs-python)
           ;; Usage:
           ;;  - C-c d ? will look up documentation for the symbol under the point.
           ;;  - C-c d r will rescan your Doxygen tags file.
           ;;  - C-c d f will insert a Doxygen comment for the next function.
           ;;  - C-c d i will insert a Doxygen comment for the current file.
           ;;  - C-c d ; will insert a Doxygen comment for the current member.
           ;;  - C-c d m will insert a blank multi-line Doxygen comment.
           ;;  - C-c d s will insert a blank single-line Doxygen comment.
           ;;  - C-c d @ will insert grouping comments around the current region.
           ;;
           ;; Fontify doxygen comments
           (defun my-doxymacs-python-font-lock-hook ()
             (if (eq major-mode 'python-mode)
                 (doxymacs-python-font-lock)
               )
             )
           
           (add-hook 'font-lock-mode-hook 'my-doxymacs-python-font-lock-hook)
           (add-hook 'python-mode-hook 'doxymacs-python-mode)
           
           ;; Select the default format
           (setq doxymacs-python-doxygen-style "default")
           
           ;; (Re)define some keybindings
           ;; Groups ("g" is easier than @ on fr kbds)
           (define-key doxymacs-python-mode-map "\C-cdg"
             'doxymacs-python-insert-grouping-comments)
           ;; Subgroupiong with the  @name command
           (define-key doxymacs-python-mode-map "\C-cdn" 
             'doxymacs-python-insert-subgrouping-comments)
           
           )
       (error
        (message "ERROR: Cannot load doxymacs-python package (%s)" (cdr err))
        ))
     ))

(defun setup-runtests-for-python ()
  "Specific setup to use a specific test framework (on top of the standard unittests module)"
  
  (eval-after-load 'python
    '(progn
       (defun python-runtests (&optional additional-runtest-args)
         "Run the closest RunTests.py in the file hierarchy."
         (let  ( 
                ;; the directory where the RunTests.py file should be located (if any)
                test-script-parent-directory 
                ;; the full path to the RunTests.py file (if it exists)
                test-script-file
                ;; remember in which directory the function is run
                (saved-default-directory default-directory)
                )
           ;; lookup for the directory containing the closest RunTest.py (in
           ;; the file hierarchy).
           (setq test-script-parent-directory 
                 (locate-dominating-file 
                  (directory-file-name (expand-file-name default-directory))
                  "RunTests.py"
                  )
                 )
           (if test-script-parent-directory
               (progn
                 (setq test-script-parent-directory (expand-file-name test-script-parent-directory))
                 (setq test-script-file (concat test-script-parent-directory "/RunTests.py"))
                 ;; bullet proofize against windows "\" vs "/" plague
                 (if (eq system-type 'windows-nt)
                     (progn
                       (setq test-script-file
                             (replace-regexp-in-string "/" "\\\\" test-script-file ))
                       )
                   )
                 ;; jump to RunTests.py's directory to launch it properly
                 (cd test-script-parent-directory)
                 (if additional-runtest-args
                     (compile (concat "python " test-script-file " " additional-runtest-args))
                   (compile (concat "python " test-script-file))
                   )
                 ;; restore the initial directory
                 (cd saved-default-directory)
                 )
             ;; a message in case of error
             (message "RunTests.py not found, unit test cannot be launched !")
             )
           )
         )

       (defun python-test-all ()
         "Call RunTests.py to launch all avaiable tests. Usefull when
you are inside a directory hierachy on top of which there is a
RunTests.py script and all the more if you work in a testXXX
file. "
         (interactive)
         (python-runtests)
         )

       (defun python-test-this ()
         "Call RunTests.py for this file only, usefull only when working
in a 'testXXX' file"
         (interactive)
         (let (filenametrunk)
           (setq filenametrunk
                 (file-name-nondirectory
                  (file-name-sans-extension buffer-file-name)
                  )
                 )
           (python-runtests (concat "--run_test=" filenametrunk))
           )
         )

       (defun python-test-select (test-name)
         "Call RunTests.py for a given test only. Usefull when you are
inside a directory hierachy on top of which there is a
RunTests.py script and all the more if you work in a testXXX
file. "
         (interactive)
         (python-runtests (concat "--run_test=" test-name))
         )
       )
    )
  )

(defun setup-doxygen-for-python ()
  "Specific setup to use doxygen-style doc in Python sources"

  (eval-after-load 'python
    '(progn
       (condition-case err
           (progn
             ;; Doxygen mode
             (require 'doxymacs-python)
             ;; Usage:
             ;;  - C-c d ? will look up documentation for the symbol under the point.
             ;;  - C-c d r will rescan your Doxygen tags file.
             ;;  - C-c d f will insert a Doxygen comment for the next function.
             ;;  - C-c d i will insert a Doxygen comment for the current file.
             ;;  - C-c d ; will insert a Doxygen comment for the current member.
             ;;  - C-c d m will insert a blank multi-line Doxygen comment.
             ;;  - C-c d s will insert a blank single-line Doxygen comment.
             ;;  - C-c d @ will insert grouping comments around the current region.
             ;;
             ;; Fontify doxygen comments
             (defun my-doxymacs-python-font-lock-hook ()
               (if (eq major-mode 'python-mode)
                   (doxymacs-python-font-lock)
                 )
               )
             
             (add-hook 'font-lock-mode-hook 'my-doxymacs-python-font-lock-hook)
             (add-hook 'python-mode-hook 'doxymacs-python-mode)
             
             ;; Select the default format
             (setq doxymacs-python-doxygen-style "default")
             
             ;; (Re)define some keybindings
             ;; Groups ("g" is easier than @ on fr kbds)
             (define-key doxymacs-python-mode-map "\C-cdg"
               'doxymacs-python-insert-grouping-comments)
             ;; Subgroupiong with the  @name command
             (define-key doxymacs-python-mode-map "\C-cdn" 
               'doxymacs-python-insert-subgrouping-comments)
             )
         (error
          (message "ERROR: Cannot load doxymacs-python package (%s)" (cdr err))
          ))
       )
    )
  )

;; -----------------------------------------------------------------------------
;; Code semantics (c/e/tags, cedet ...)
;;-----------------------------------------------------------------------------

;;(require 'semantic)
(require 'gtags)

(defun ww-next-gtag ()
  "Find next matching tag, for GTAGS."
  (interactive)
  (let ((latest-gtags-buffer
         (car (delq nil  (mapcar (lambda (x) (and (string-match "GTAGS SELECT" (buffer-name x)) (buffer-name x)) )
                                 (buffer-list)) ))))
    (cond (latest-gtags-buffer
           (switch-to-buffer latest-gtags-buffer)
           (forward-line)
           (gtags-select-it nil))
          ) ))

;; M-, cycles to next result, after doing M-. C-M-. or C-M-,
;; M-. finds tag
;; C-M-. find all references of tag
;; C-M-, find all usages of symbol.
(global-set-key (kbd "M-,") 'ww-next-gtag)   
(global-set-key (kbd "M-.") 'gtags-find-tag)
(global-set-key (kbd "C-M-.") 'gtags-find-rtag)   
(global-set-key (kbd "C-M-,") 'gtags-find-symbol)



(define-key gtags-select-mode-map "\^?" 'scroll-down)
(define-key gtags-select-mode-map " " 'scroll-up)
(define-key gtags-select-mode-map "\C-b" 'scroll-down)
(define-key gtags-select-mode-map "\C-f" 'scroll-up)
(define-key gtags-select-mode-map "k" 'previous-line)
(define-key gtags-select-mode-map "j" 'next-line)
(define-key gtags-select-mode-map "p" 'previous-line)
(define-key gtags-select-mode-map "n" 'next-line)
(define-key gtags-select-mode-map "q" 'gtags-pop-stack)
(define-key gtags-select-mode-map "u" 'gtags-pop-stack)
(define-key gtags-select-mode-map "." 'gtags-select-tag)
(define-key gtags-select-mode-map "o" 'gtags-select-tag-other-window)



;; -----------------------------------------------------------------------------
;; C/C++ personal config
;;-----------------------------------------------------------------------------

;; .h are also C++
(setq auto-mode-alist (cons '("\\.h\\'" . c++-mode) auto-mode-alist))

(eval-after-load 'cc-mode
  '(progn
     (idle-highlight)
     ;; Custom snippets (require 'yasnippet)
     (yas/define-snippets 'c++-mode
                          '(
                            ("tnc" "// `(user-login-name)`: NOCOMMIT" "no commit stamp" nil)
                            ("tntd" "// `(user-login-name)`: TODO" "todo stamp" nil)
                            ("///" "//! " nil)
                            ("ncout" "std::cout << \"`(user-login-name)`: NOCOMMIT:\" << $0 << std::endl;" "no commit std::cout" nil)
                            ))
     (yas/define-snippets 'c-mode
                          '(
                            ("tnc" "/* `(user-login-name)`: NOCOMMIT */" "no commit stamp" nil)
                            ("tntd" "/* `(user-login-name)`: TODO */" "todo stamp" nil)
                            ))
     ))


;; MSBuild regexp
(defun set-compilation-regexp-alist-for-msbuild ()
  (let (
        (err_msbuild
            ; Error sample:
            ; 272>D:\MOUF\MIF\branch\module1\src\file.cpp(363): error C2227: left of '->Bidule' must point to class/struct/union/generic type [D:\MOUF\MIF\branch\build\Visual_Studio_10\module1.vcxproj]
         '(msbuildvc
           "^[ \t]*\\(?:[0-9]+>\\)\\(\\(?:[a-zA-Z0-9]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\))\s*: \\(error\\|fatal error\\)"
           1 2 nil 2))
        (warn_msbuild
            ; Warning sample:
            ; 272>D:\MOUF\MIF\branch\module1\src\file.cpp(363): warning ...
         '(msbuildvc_warn
           "^[ \t]*\\(?:[0-9]+>\\)\\(\\(?:[a-zA-Z0-9]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\))\s*: warning"
           1 2 nil 1))
        (info_msbuild
            ; Info sample:
            ; 272>D:\MOUF\MIF\branch\module1\src\file.cpp(363): warning ...
         '(msbuildvc_info
           "^[ \t]*\\(?:[0-9]+>\\)\\(\\(?:[a-zA-Z0-9]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\))\s*: \\(see declaration\\|see reference\\)"
           1 2 nil 0))
        (err_msbuild_lnk
            ; Error sample:
            ;LINK : fatal error LNK1181
         '(msbuildvclnk
           "^[ \t]*\\(?:[0-9]+>\\)?\\([^:\t\n]+\\)\s*: \\(?:fatal error\\|error\\) LNK[0-9]+"
           1 nil nil 2))
        )
    (add-to-list 'compilation-error-regexp-alist-alist err_msbuild)
    (add-to-list 'compilation-error-regexp-alist-alist err_msbuild_lnk)
    (add-to-list 'compilation-error-regexp-alist-alist warn_msbuild)
    (add-to-list 'compilation-error-regexp-alist-alist info_msbuild)
    (setq compilation-error-regexp-alist '(msbuildvc msbuildvc_warn msbuildvc_info msbuildvclnk msft))
    )
  )
  

  




;; -----------------------------------------------------------------------------
;; compilation and grep buffers interaction improvements
;; -----------------------------------------------------------------------------

;; Define this (mostly copy-pasted from simple.el)
(defun first-error-no-select (&optional n)
  "Move point to the first error in the `next-error' buffer and highlight match.
Prefix arg N says how many error messages to move forwards (or
backwards, if negative).
Finds and highlights the source line like \\[next-error], but does not
select the source buffer."
  (interactive "p")
  (let ((next-error-highlight next-error-highlight-no-select))
    (next-error n t))
  (pop-to-buffer next-error-last-buffer))




;; Compilation mode deserve the same handy kbd as grep
(defun my-compilation-hook ()
  "Just a few keybindings customisation for compilation mode"
  (define-key	compilation-mode-map "n" 'next-error-no-select)
  (define-key	compilation-mode-map "p" 'previous-error-no-select)
  (define-key	compilation-mode-map "a" 'first-error-no-select)
  (define-key	compilation-mode-map "N" 'next-error)
  (define-key	compilation-mode-map "P" 'previous-error)
  (define-key	compilation-mode-map "A" 'first-error)
  (define-key compilation-mode-map (kbd "M-n") 'compilation-next-error)
  (define-key compilation-mode-map (kbd "M-p") 'compilation-previous-error)
)
(add-hook 'compilation-mode-hook 'my-compilation-hook)

(defun my-grep-hook ()
  "Just a few keybindings customisation for grep mode"
  (define-key	grep-mode-map "a" 'first-error-no-select)
  (define-key	grep-mode-map "N" 'next-error)
  (define-key	grep-mode-map "P" 'previous-error)
  (define-key	grep-mode-map "A" 'first-error)
  )
(add-hook 'grep-setup-hook 'my-grep-hook)


;; -----------------------------------------------------------------------------
;; Region commenting and copying at the same time please
;; -----------------------------------------------------------------------------

;;;###autoload
(defun duplicate-comment-out (arg)
  "Comment out and duplicate current line or region. If the
  region is active and `transient-mark-mode' is on, the full
  region will be duplicated, and the original region will be
  commented out.  Else the same happen but only on current line."
  (interactive "*P")
  (if (and mark-active transient-mark-mode)
      (progn
	(let (
	      (original-region-begining (region-beginning))
	      (original-region-end (region-end))
	      )
	  (kill-region original-region-begining original-region-end)
	  (yank)
	  (yank)
	  (comment-region original-region-begining original-region-end)
	  )
	)
    (progn
      (move-beginning-of-line arg)
      (let ((beg (point)))
	(move-end-of-line arg)
	(comment-region beg (point))
	)
      (kill-whole-line)
      (yank)
      (yank)
      (forward-line -1)
      (let ((beg (point)))
	(move-end-of-line arg)
	(uncomment-region beg (point)))
      )))
(global-set-key [(control ?\;)] 'duplicate-comment-out)


;; the following is taken from http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs/998472#998472
;;;###autoload
(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))
(global-set-key [(control ?:)] 'duplicate-line)


;; -----------------------------------------------------------------------------
;; Buffer stats
;; -----------------------------------------------------------------------------

;;;###autoload
(defun count-symbol-at-point ()
  "Count the number of occurences of the symbol at point, in the whole buffer"
  (interactive)
  (let ((target-symbol (symbol-at-point)))
    (unless target-symbol (error "No symbol at point"))
    (save-excursion
      (goto-char (point-min))
      (count-matches target-symbol))))

(global-set-key [(meta m)] 'count-symbol-at-point)


;; -----------------------------------------------------------------------------
;; External OS/Desktop integration
;; -----------------------------------------------------------------------------

;;;###autoload
(defun explore-current-directory ()
  "Show current directory in OS's explorer."
  (interactive)
  (let* (
	 (command   
	  (if (eq system-type 'windows-nt)
	      "explorer"
	    "nautilus")
	  )
	 (arguments 
	  (if (eq system-type 'windows-nt)
	      ;; converting back and forth the '\\' and "/" in order
	      ;; for expand-file-name to work and then for explorer to
	      ;; understand the path.
	      (replace-regexp-in-string "/" "\\\\" (expand-file-name (replace-regexp-in-string "\\\\" "/" default-directory )))
	    (concat default-directory "/")
	    )
	  )
	 )
    (start-process-shell-command "Current directory exploration" nil 
				 command arguments)
    (message (concat "Directory exploration with: " command " " arguments))
    )
  )


;;;###autoload
(defun command-on-current-directory ()
  "Show an external (native) command line with the working directory set to current one."
  (interactive)
  (if (eq system-type 'windows-nt)
      ;; (let* ( 
      ;; 	     ( 
      ;; 	      ;; converting back and forth the '\\' and "/" in order
      ;; 	      ;; for expand-file-name to work and then for explorer to
      ;; 	      ;; understand the path.
      ;; 	      (replace-regexp-in-string "/" "\\\\" (expand-file-name (replace-regexp-in-string "\\\\" "/" default-directory )))
      (progn
	(w32-shell-execute "open" "cmd")
	(message (concat "Command on directory with: " launcher))
	)
    (let* (
	   (command  "gnome-terminal")
	   (change-directory-cmd
	    (concat
	     "cd "
	     (concat default-directory "/")
	     )
	    )
	   (launcher (concat
		      change-directory-cmd
		      " && "
		      command
		      )
		     )
	   )
      (start-process-shell-command "Current directory for command" nil launcher nil)
      (message (concat "Command on directory with: " launcher))
      )
    )
  )



(defun generate-doc-doxygen ()
  "Generate Doxygen documentation related to current file or folder"
  (interactive)
  (let  ( 
	 ;; the directory where the doxygen.cfg file should be located (if any)
	 doxygen-config-parent-directory 
	 ;; the full path to the doxygen.cfg file (if it exists)
	 doxygen-config-file
	 ;; "expected" full path to the index of the generated doc
	 doxygen-html-index-file
	 ;; remember in which directory the function is run
	 (saved-default-directory default-directory)
	 )
    ;; lookup for the directory containing the closest doxygen.conf
    ;; the file hierarchy).
    (setq doxygen-config-parent-directory
	  (locate-dominating-file 
	   (directory-file-name (expand-file-name default-directory))
	   "doxygen.cfg"
	   )
	  )
    (if doxygen-config-parent-directory
	  (progn
	    (setq doxygen-config-parent-directory (expand-file-name doxygen-config-parent-directory))
	    (setq doxygen-config-file (concat doxygen-config-parent-directory "/doxygen.cfg"))
	    (setq doxygen-html-index-file (concat doxygen-config-parent-directory "/build_doc/html/index.html"))
	    ;; bullet proofize against windows "\" vs "/" plague
	    (if (eq system-type 'windows-nt)
		(progn
		  (setq doxygen-config-file
			(replace-regexp-in-string "/" "\\\\" doxygen-config-file ))
		  )
	      )
	    ;; jump to doxygen.cfg's directory to launch it properly
	    (cd doxygen-config-parent-directory)
	    (compile (concat "doxygen " doxygen-config-file
			     " && firefox " "file://" doxygen-html-index-file))
	    ;; restore the initial directory
	    (cd saved-default-directory)
	    ;; ;; also automatically launch a view on the generated documentation
	    ;; (start-process-shell-command "Browse Doxygen" nil 
	    ;; 				 (concat "firefox " 
	    ;; 					 "file://" doxygen-html-index-file))
	    )
	  ;; a message in case of error
	  (message "doxygen.conf not found, no documentation generated !")
	  )
    )
  )


;; *****************************************************************************
;; ********* WORKAROUNDS to starter kit ****************************************
;; *****************************************************************************


;; Key bindings corrections
(add-hook 'paredit-mode-hook
          (lambda ()
            ;; paredit's bindings C-<right/left> are just
            ;; killing me
            (define-key paredit-mode-map (kbd "C-<right>") 'forward-word)
            (define-key paredit-mode-map (kbd "C-<left>") 'backward-word)
            ))


(setq ac-auto-start 3)
(setq ac-candidate-limit 20)
(setq ac-delay 0.5)
(setq semantic-idle-scheduler-idle-time 3)


;; *****************************************************************************
;; *********  Stuff to be called at last  **************************************
;; *****************************************************************************

;; Use smex as M-x improvement
;; (see http://github.com/nonsequitur/smex/tree/master)
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'execute-extended-command)
;; Warning:
;; must always be called at last (registers all defined functions so far)
(smex-initialize)
;; auto-update after every x-second-idle period
(smex-auto-update 3)

(provide 'tibonihoo_common)
