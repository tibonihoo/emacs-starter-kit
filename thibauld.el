;;
;; Additional customisation on top of the starter-kit's defaults.
;;
;; (c) 2010 Thibauld Nion
;;

;; Personal info for signatures in code and stuff...
(set 'user-full-name "Thibauld Nion")
(set 'user-mail-address "thibauld@tibonihoo.net")
;; Work context
(defvar my-work-context "home")
;; Set initialise frame size
(add-to-list 'default-frame-alist '(height . 42))
(add-to-list 'default-frame-alist '(width . 81))


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
;; Window size control
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
     ;; my prefered indentation is 2 spaces
     (setq python-indent 2)
     ;; Define some handy snippets
     (yas/define-snippets 'python-mode
                          '(
                            ("tnc" "# TN: NOCOMMIT" "Mark as uncommitable" nil)
                            ("encoding" "# -*- coding: utf-8 -*-
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
     ))


;; -----------------------------------------------------------------------------
;; C/C++ personal config
;;-----------------------------------------------------------------------------
(add-hook 'c-mode-common-hook
	  '(lambda () 
	     ;; Enable "hungry delete":
	     ;; use: 
	     ;;    *backspace deletes all contiguous blank spaces in one stroke; 
	     ;;    * C-d performs an "usual" backspace.
	     ;;
	     (c-toggle-hungry-state 1)
             ;; Use custom c/c++ style
             (require 'cc-morphee-style)
             (morphee-style)
             (require 'doxymacs-morphee-style)
             ))


;; -----------------------------------------------------------------------------
;; LaTeX personal config
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
(global-set-key "\C-c;" 'duplicate-comment-out)


;;;###autoload
(defun duplicate-line-or-region (arg)
  "Duplicate current line or region. If the region is active and
  `transient-mark-mode' is on, the full region will be
  duplicated.  Else the same happen but only on current line."
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
	  )
	)
    (progn
      (move-beginning-of-line arg)
      (kill-whole-line)
      (yank)
      (yank)
      )
    ))
(global-set-key "\C-c:" 'duplicate-line-or-region)



;;;###autoload
(defun count-word-occurences ()
  "Print number of occurence of the symbol at point in the
currently selected region. (requires thingatpt.el)"
  (interactive)
  (let ((symbol (word-at-point)))
    (unless symbol (error "No symbol at point"))
    ;; 1. Set up appropriate conditions.
    (save-excursion
      (goto-char (point-min))
      (let ((count 0))
	
	;; 2. Run the while loop.
	(while (< (point) (point-max))
	  (re-search-forward symbol (point-max) 0)
	  (setq count (1+ count)))
	;; we're automatically counting one more than necessary
	(setq count (- count 1))
	
	;; 3. Send a message to the user.
	(cond ((zerop count)
	       (message
		"The region does NOT have any words."))
	      ((= 1 count)
	       (message
		(concat "Found 1 occurence of " symbol)))
	      (t
	       (message (concat "Found " (number-to-string count)
                                " occurences of " symbol ))
	       )
	      )))))





;; *****************************************************************************
;; ********* WORKAROUNDS to starter kit ****************************************
;; *****************************************************************************

;; -----------------------------------------------------------------------------
;; Display corrections
;; -----------------------------------------------------------------------------
;; Display 'Emacs' and then then current file's name in the window title
;; (setq frame-title-format '("" "Emacs - %b"))


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
(setq ido-use-filename-at-point nil)
;; but the feature is nice, so we might bind it to a diferent key:
(global-set-key "\C-x\C-g" 'find-file-at-point)


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
