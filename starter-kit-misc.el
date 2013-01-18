;;; starter-kit-misc.el --- Things that don't fit anywhere else
;;
;; Part of the Emacs Starter Kit

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(add-hook 'before-make-frame-hook 'turn-off-tool-bar)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq visible-bell t
      echo-keystrokes 0.1
      font-lock-maximum-decoration t
      inhibit-startup-message t
      transient-mark-mode t
      color-theme-is-global t
      shift-select-mode nil
      mouse-yank-at-point t
      require-final-newline t
      truncate-partial-width-windows nil
      uniquify-buffer-name-style 'forward
      whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100
      ediff-window-setup-function 'ediff-setup-windows-plain
      oddmuse-directory (concat dotfiles-dir "oddmuse")
      xterm-mouse-mode t
      save-place-file (concat dotfiles-dir "places"))

;; Make sure the cursor blinks (helps in seeing when the program is stuck or not)
(blink-cursor-mode t)

;; No new line
(setq next-line-add-newlines nil)

;; Smooth scroll
(setq scroll-step 1)

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


;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Save a list of recent files visited.
(recentf-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; -----------------------------------------------------------------------------
;; Basic setup for ido
;; -----------------------------------------------------------------------------
;; ido-mode is like magic pixie dust!
(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point nil
        ido-max-prospects 10)
  ;; Also use ido for bookmarks 
  ;; ( taken from Qichen Huang's dot-emacs:
  ;; http://code.google.com/p/my-emacs-jasonal/source/browse/emacs-init/trunk/dot-emacs.el?r=1 )
  (defun ido-bookmark-jump (bookmark)
    (interactive
     (progn
       (require 'bookmark)
       (bookmark-maybe-load-default-file)
       (list (ido-completing-read "Jump to bookmark: "
                                  (mapcar 'car bookmark-alist)))))
    (bookmark-jump bookmark))
  (global-set-key (kbd "C-x r b") 'ido-bookmark-jump)
  )
;; Let me hit enter before jumping to the (unique) matched item
(setq ido-confirm-unique-completion t)
;; Fuzzy matching 
(setq ido-enable-flex-matching t)
;; Disable matching on the merged list of directories (way too dangerous)
(setq ido-auto-merge-work-directories-length nil)
;; Tell dired to suggest the path to another open dired buffer (if
;; any) as default path for stuff like copy (sweeeeeet)
(setq dired-dwim-target 1)
;; Show a dot to open a directory 
;; (keeps the usual emacs behaviour when opening a file)
(setq ido-show-dot-for-dired t)
;; I find this disturbing
(setq ido-use-filename-at-point nil)
;; but the feature is nice, so we might bind it to a diferent key:
(global-set-key "\C-x\C-g" 'find-file-at-point)


;; Don't use tabs for indentation
(set-default 'indent-tabs-mode nil)

;; show on the left fringe if a line is empty
(set-default 'indicate-empty-lines t)

;; Force imenu to always rescan the buffers
(set-default 'imenu-auto-rescan t)

;; So that C-n adds a new line if already at the end of the buffer.
(setq next-line-add-newlines nil)

;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'text-mode-hook 'turn-on-flyspell)

(defvar coding-hook nil
  "Hook that gets run on activation of any programming mode.")

(defalias 'yes-or-no-p 'y-or-n-p)
(random t) ;; Seed the random-number generator

;; Hippie expand: at times perhaps too hip
(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)

;; Don't clutter up directories with files~
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

;; nxhtml stuff
(setq mumamo-chunk-coloring 'submode-colored
      nxhtml-skip-welcome t
      indent-region-mode t
      rng-nxml-auto-validate-flag nil)

;; Associate modes with file extensions

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . xml-mode))

;; Default to unified diffs
(setq diff-switches "-u")

;; Cosmetics

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))

(eval-after-load 'mumamo
  '(eval-after-load 'zenburn
     '(ignore-errors (set-face-background
                      'mumamo-background-chunk-submode "gray22"))))

;; Platform-specific stuff
(when (eq system-type 'darwin)
  ;; Work around a bug on OS X where system-name is FQDN
  (setq system-name (car (split-string system-name "\\."))))

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

;; Get around the emacswiki spam protection
(add-hook 'oddmuse-mode-hook
          (lambda ()
            (unless (string-match "question" oddmuse-post)
              (setq oddmuse-post (concat "uihnscuskc=1;" oddmuse-post)))))

(provide 'starter-kit-misc)
;;; starter-kit-misc.el ends here
