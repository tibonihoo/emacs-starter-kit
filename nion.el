;;
;; Additional customisation on top of the starter-kit's defaults.
;;
;; (c) 2010 Thibauld Nion
;;

;; Personal info for signatures in code and stuff...
(set 'user-full-name "Thibauld Nion")
(set 'user-mail-address "tn@a2ia.com")
;; Work context
(defvar my-work-context "DR")
;; Set initialise frame size
(add-hook 'window-setup-hook 'frame-set-fullscreen t)



(if (eq system-type 'windows-nt)
    (set-face-attribute 'default nil :family "Consolas" :height 105)
  ;; else use lucida
  (set-face-attribute 'default nil :family "b&h-lucidatypewriter")
  )

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
;; Highlight Some keywords
(require 'highlight-fixmes-mode)
;; Customizing the font and keywords (@see
;; http://members.iinet.net.au/~bethandmark/elisp/highlight-fixmes-mode.el)
(setq fixme-words '("FIXME" "TODO" "NOCOMMIT" "THINKME" "BUG" "DEBUG" "NDEBUG"))
(custom-set-faces
 '(fixme-face ((t (:background "orange" :foreground "black" :bold t))) t))
;; create a global mode out of the minor mode (most stuff copied from font-core.el)
(defun turn-on-highlight-fixmes-mode ()
  "Turn on Highlight Fixmes mode."
  (unless highlight-fixmes-mode
    (highlight-fixmes-mode))
  )
(define-globalized-minor-mode global-highlight-fixmes-mode 
  highlight-fixmes-mode 
  turn-on-highlight-fixmes-mode
  )
;; activate the highlight for everyone
(global-highlight-fixmes-mode t)



;;To avoid the compilation buffer to be displayed if there's no error
(cons  
     (lambda (buf str)
	(if (string-match "exited abnormally" str)
	    ;;there were errors
	    (message "compilation errors.")
	  ;;no errors, make the compilation window go away in 0.5 seconds
	  (run-at-time 0.5 nil 'delete-windows-on buf)
	  (message "NO COMPILATION ERRORS!")
	  )
	)
     compilation-finish-functions
     )

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


;; -----------------------------------------------------------------------------
;; Scroll behavior
;; -----------------------------------------------------------------------------
;; No new line
(setq next-line-add-newlines nil)
;; Smooth scroll
(setq scroll-step 1)



;; Tell dired to suggest the path to another open dired buffer (if
;; any) as default path for stuff like copy (sweeeeeet)
(setq dired-dwim-target 1)



;; -----------------------------------------------------------------------------
;; Navigation between buffers
;; -----------------------------------------------------------------------------

(defun window-height-increase ()
  "Increase the height of the window by 5%"
  (interactive)
  (enlarge-window (max 1 (round (* .05 (window-height)))))
  )

(defun window-height-decrease ()
  "Decrease the height of the window by 5%"
  (interactive)
  (enlarge-window (min -1 (round (* -.05 (window-width)))))
  )

(defun window-width-increase ()
  "Increase the width of the window by 5%"
  (interactive)
  (enlarge-window-horizontally (max 1 (round (* .05 (window-width)))))
  )

(defun window-width-decrease ()
  "Decrease the width of the window by 5%"
  (interactive)
  (enlarge-window-horizontally (min -1 (round (* -.05 (window-width)))))
  )



(defun window-height-small-increase ()
  "Increase the height of the window by 1pix"
  (interactive)
  (enlarge-window 1)
  )

(defun window-height-small-decrease ()
  "Decrease the height of the window by 1pix"
  (interactive)
  (enlarge-window -1)
  )

(defun window-width-small-increase ()
  "Increase the width of the window by 1pix"
  (interactive)
  (enlarge-window-horizontally 1)
  )

(defun window-width-small-decrease ()
  "Decrease the width of the window by 1pix"
  (interactive)
  (enlarge-window-horizontally -1)
  )

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

;; height
(global-set-key (kbd "C-S-j") 'window-height-increase)
(global-set-key (kbd "C-S-k") 'window-height-decrease)
(global-set-key (kbd "C-M-S-j") 'window-height-small-increase)
(global-set-key (kbd "C-M-S-k") 'window-height-small-decrease)
;; width
(global-set-key (kbd "C-S-h") 'window-width-increase)
(global-set-key (kbd "C-S-l") 'window-width-decrease)
(global-set-key (kbd "C-M-S-h") 'window-width-small-increase)
(global-set-key (kbd "C-M-S-l") 'window-width-small-decrease)



;; -----------------------------------------------------------------------------
;; Misc customisations
;; -----------------------------------------------------------------------------

;; Flymake additional functionalities
(require 'flymake-add)


;; -----------------------------------------------------------------------------
;; Python personal config
;;-----------------------------------------------------------------------------
(eval-after-load 'python
  '(progn
     ;; A2iA indentation is 3 spaces
     (setq python-indent 3)
     (setq indent-tabs-mode nil)
     (setq python-guess-indent nil)     
     ;; Define some handy snippets (requires yasnippet or yasnippet-bundle)
     (yas/define-snippets 'python-mode
                          '(
                            ("tnc" "# TN: NOCOMMIT"
                             "Mark as uncommitable" nil)
                            ("ptd" "pass # TODO"
                             "Insert the default shebang for python "nil)
                            ("encoding" "# -*- coding: cp1252 -*-
" "Set the file encoding" nil)
                            ("shebang" "#!/usr/bin/env python
" "Insert the default shebang for python "nil)
                            )
                          )
     ;; imenu settings: also show function's args
     (setq py-imenu-show-method-args-p t)
     ;; navigate errors returned by flymake
     (define-key python-mode-map "\C-c\C-v" 'my-flymake-show-next-error)
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

;; -----------------------------------------------------------------------------
;; LaTeX personal confirm-kill-emacs
;; -----------------------------------------------------------------------------
(eval-after-load 'reftex
  ;; Set the path to my biblio files according to my machine
  (if (string-match "Carbon" (emacs-version))
      (progn
	;; on my mac
	(setq reftex-bibpath-environment-variables 
	      '(".//:..:/Users/thibauldnion/Documents/LaTeXForge/Biblio//"))
	)
    (progn
      ;; on unix
      (setq reftex-bibpath-environment-variables 
	    '(".//:..:/home/thibauld/Documents/Scriptorium/LaTeXForge/Biblio//"))
      )
    ))



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


(defun sysparam-show ()
  "Show the sysparam page"
  (interactive)
  (let* (
	 (weburl "file:///d:/HOME/Nion/Sources/Documentation/DevDoc/spb/SysParamRec.html")
	 (command (format "firefox %s"
			  weburl))
	 )
    (start-process-shell-command "Sysparam view" nil command)
    )
  )

(defun api-doc-show ()
  "Show documentation for API"
  (interactive)
  (let* (
	 (command "hh C:/Program Files/A2iA/A2iA DocumentReader V3.0 R3/Doc/A2iA DocumentReader V3.0R3 API Reference.chm")
	 )
    (start-process-shell-command "API doc view" nil command)
    )
  )


(defun pyrates-doc-show ()
  "Show the sysparam page"
  (interactive)
  (let* (
	 (weburl "http://src2/doc/clean_doc/html/d8/dea/group__py__rates.html")
	 (command (format "firefox %s"
			  weburl))
	 )
    (start-process-shell-command "PyRates doc view" nil command)
    )
  )


(defun structureddocument-doc-show ()
  "Show the sysparam page"
  (interactive)
  (let* (
	 (weburl "file://///seaborgium/D_A2iA/Sources_b3/Buildbot/PyModules/pythonDoc/group__structureddoc.html")
	 (command (format "firefox %s"
			  weburl))
	 )
    (start-process-shell-command "PyRates doc view" nil command)
    )
  )


(defun pkview ()
  "Load a db or an image via Python_Kob"
  (interactive)
  (let* (
	 (pkviewCMD "D:/Home/nion/Bin/pkview ")
	 (command (concat pkviewCMD buffer-file-name))
	 )
    (start-process-shell-command "PythonKob view" nil command)
    )
  )


(defun directory-parent-directory (directory-name)
  "Return the parent directory of a given directory"
  (file-name-directory 
   (directory-file-name 
    (expand-file-name directory-name)
    )
   )
  )

(defun directory-is-root (directory-name)
  "Return t if the directory name is a root of the filesystem (that is '/' or 'c:/', 'd:/' (...) on Windows."
  (string= 
   (directory-parent-directory directory-name)
   directory-name
   )
  )

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

;; -----------------------------------------------------------------------------
;; Display corrections
;; -----------------------------------------------------------------------------
;; Display 'Emacs' and then then current file's name in the window title
(setq frame-title-format '("" "Emacs - %b"))

;; And I want the cursor to blink (helps in seeing when the program is
;; unstuck)
(blink-cursor-mode t)

;; Key bindings corrections
(add-hook 'paredit-mode-hook
          (lambda ()
            ;; paredit's bindings C-<right/left> are just
            ;; killing me
            (define-key paredit-mode-map (kbd "C-<right>") 'forward-word)
            (define-key paredit-mode-map (kbd "C-<left>") 'backward-word)
            ))


;; -----------------------------------------------------------------------------
;; Basic setup for ido
;; -----------------------------------------------------------------------------
;; Let me hit enter before jumping to the (unique) matched item
(setq ido-confirm-unique-completion t)
;; Fuzzy matching 
(setq ido-enable-flex-matching t)
;; Disable matching on the merged list of directories (way to dangerous for me)
(setq ido-auto-merge-work-directories-length nil)
;; Show a dot to open a directory 
;; (keeps the usual emacs behaviour when opening a file)
(setq ido-show-dot-for-dired t)
;; I find this disturbing
(setq ido-use-filename-at-point 'disabled)
;; but the feature is nice, so we might bind it to a diferent key:
(global-set-key "\C-cf" 'find-file-at-point)


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
;; auto-update after every 30-second-idle period
(smex-auto-update 10)
