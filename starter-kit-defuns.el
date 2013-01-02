;;; starter-kit-defuns.el --- Define some custom functions
;;
;; Part of the Emacs Starter Kit

(require 'thingatpt)
(require 'imenu)

;; Network

(defun view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    ;; TODO: switch to nxml/nxhtml mode
    (cond ((search-forward "<?xml" nil t) (xml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

;; Buffer-related

(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))
                              
                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))
                              
                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))
                             
                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))



;;; Dealing with encodings


(defun encode-string-to-hexa-c-chars (str &optional keepASCII)
  "Encode a string into another string where each byte is
replaced by the exlicit notation of its hexadecimal code usable
in C programming language for characters (the notation begining
by '\\x').

If keepASCII is non-nil then ASCII characters won't be converted,
even when these ASCII characters are actually part of the
decomposition of a single (multi-byte) character in the input string."
  (mapconcat
   (lambda (char) (if (and keepASCII (eq (char-charset char) 'ascii))
                 (char-to-string char) (format "\\x%02X"  char)))
   (string-to-list   (string-as-unibyte str))
   ""))


(defun insert-string-as-hexa-c-chars (str &optional keepASCII)
  "Given a string, will convert its chars to their c hexadecimal
representation and insert it in current buffer.

If keepASCII is non-nil, ASCII characters will remain in the inserted string."
  (interactive "sEncode and insert: ")
  (insert (encode-string-to-hexa-c-chars str keepASCII))
  )

(defun insert-string-as-hexa-c-chars-keep-ascii (str)
  "Just an alias to call insert-string-as-hexa-c-chars interactively
and keep ASCII characters in the output."
  (interactive "sEncode and insert: ")
  (insert-string-as-hexa-c-chars str t)
  )

(defun encode-region-to-hexa-c-chars (start end &optional keepASCII)
  "Given a region, replace its content byt the string obtained
after convertion via encode-string-to-hexa-c-chars.

If keepASCII is non-nil, ASCII characters will remain in the replacement string."
  (interactive "*r")
  (setq regionStr (buffer-substring start end))
  (delete-region start end)
  (goto-char start)
  (insert-string-as-hexa-c-chars regionStr keepASCII)
  )

(defun encode-region-to-hexa-c-chars-keep-ascii (start end)
  "Just an alias to call encode-region-to-hexa-c-chars interactively
and keep ASCII characters in the output."
  (interactive "*r")
  (encode-region-to-hexa-c-chars start end t)
  )

(defun comment-copy-encode-region-to-hexa-c-chars-keep-ascii (start end &optional keepASCII)
  "Same as encode-region-to-hexa-c-chars-keep-ascii but keep
the original region in place and comment it out."
  (interactive "*r")
  (setq regionStr (buffer-substring start end))
  (goto-char end)
  (insert (encode-string-to-hexa-c-chars regionStr keepASCII))
  (comment-region start end)
  )

(defun comment-copy-encode-region-to-hexa-c-chars-keep-ascii (start end)
  "Just an alias to call comment-copy-encode-region-to-hexa-c-chars-keep-ascii interactively
and keep ASCII characters in the output."
  (interactive "*r")
  (comment-copy-encode-region-to-hexa-c-chars-keep-ascii start end t)
  )


;;; These belong in coding-hook:

;; We have a number of turn-on-* functions since it's advised that lambda
;; functions not go in hooks. Repeatedly evaling an add-to-list with a
;; hook value will repeatedly add it since there's no way to ensure
;; that a lambda doesn't already exist in the list.

(defun local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))

(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun turn-on-hl-line-mode ()
  (if window-system (hl-line-mode t)))

(defun turn-on-save-place-mode ()
  (setq save-place t))

(defun turn-on-whitespace ()
  (whitespace-mode t))

(defun turn-on-paredit ()
  (paredit-mode t))

(defun turn-off-tool-bar ()
  (tool-bar-mode -1))

(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|FIX\\|TODO\\|HACK\\|REFACTOR\\|BUG\\|NOCOMMIT\\|WARNING\\|ERROR\\|UGLY\\)[^[:word:]]*"
          1 font-lock-warning-face t))))

(add-hook 'coding-hook 'local-column-number-mode)
(add-hook 'coding-hook 'local-comment-auto-fill)
(add-hook 'coding-hook 'turn-on-hl-line-mode)
(add-hook 'coding-hook 'turn-on-save-place-mode)
(add-hook 'coding-hook 'pretty-lambdas)
(add-hook 'coding-hook 'add-watchwords)
  
(defun run-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'coding-hook))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;; Cosmetic

(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

;; XML tags everywhere
;; from
;; http://www.johndcook.com/blog/2010/08/05/emacs-command-to-add-html-tags/

(defun tag-word-or-region (tag)
    "Surround current word or region with a given tag."
    (interactive "sEnter tag (without <>): ")
    (let (pos1 pos2 bds start-tag end-tag)
        (setq start-tag (concat "<" tag ">"))
        (setq end-tag (concat "</" tag ">"))
        (if (and transient-mark-mode mark-active)
            (progn
                (goto-char (region-end))
                (insert end-tag)
                (goto-char (region-beginning))
                (insert start-tag))
            (progn
                (setq bds (bounds-of-thing-at-point 'symbol))
                (goto-char (cdr bds))
                (insert end-tag)
                 (goto-char (car bds))
                 (insert start-tag)))))


;; Other

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory dotfiles-dir 0)
  ;; TODO: remove elpa-to-submit once everything's submitted.
  (byte-recompile-directory (concat dotfiles-dir "elpa-to-submit/") 0))

(defun regen-autoloads (&optional force-regen)
  "Regenerate the autoload definitions file if necessary and load it."
  (interactive "P")
  (let ((autoload-dir (concat dotfiles-dir "/elpa-to-submit"))
        (generated-autoload-file autoload-file))
    (when (or force-regen
              (not (file-exists-p autoload-file))
              (some (lambda (f) (file-newer-than-file-p f autoload-file))
                    (directory-files autoload-dir t "\\.el$")))
      (message "Updating autoloads...")
      (let (emacs-lisp-mode-hook)
        (update-directory-autoloads autoload-dir))))
  (load autoload-file))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(defun switch-or-start (function buffer)
  "If the buffer is current, bury it, otherwise invoke the function."
  (if (equal (buffer-name (current-buffer)) buffer)
      (bury-buffer)
    (if (get-buffer buffer)
        (switch-to-buffer buffer)
      (funcall function))))

(defun insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun pairing-bot ()
  "If you can't pair program with a human, use this instead."
  (interactive)
  (message (if (y-or-n-p "Do you have a test for that? ") "Good." "Bad!")))

(defun esk-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (set (make-local-variable 'paredit-space-delimiter-chars)
       (list ?\"))
  (paredit-mode 1))

(defun message-point ()
  (interactive)
  (message "%s" (point)))


;; -----------------------------------------------------------------------------
;; Frame size manipulation
;; -----------------------------------------------------------------------------

(defun frame-set-fullscreen (&optional f)
  "This function is called by toggle-fullscreen if the frame was previously in a 'custom' size.

You can also make it so that this function is called directly when emacs starts by adding the following command to your initialisation file:

 ;; Set initialise frame size
 (add-hook 'window-setup-hook 'frame-set-fullscreen t)
"
  (interactive)
  (if (not f)
      (setq f (selected-frame))
      )
  ;; store the current width and height into the frame's variables
  (set-frame-parameter f 'previous-width (frame-width f))
  (set-frame-parameter f 'previous-height (frame-height f))
  (cond
   ( (eq system-type 'windows-nt)
     ;; under Windows
     (w32-send-sys-command #xf030) ;; from Windows WM_SYSCOMMAND : SC_MAXIMIZE
     )
   ( (memq window-system '(mac ns))
     (progn ;; specific for a macbookpro of 2012
       (set-frame-width f 202)
       (set-frame-height f 53)
       )
     )
   ( t
     ;; default to internal method
     (set-frame-parameter f 'fullscreen 'fullboth)
     )
   )
  ;; This is a hack to get a flag that will also work on windows,
  ;; hopefully it won't be needed for long.
  (set-frame-parameter f 'is-fullscreen t)
  )

(defun frame-set-fullscreen-off (&optional f)
  "This function is called by toggle-fullscreen if the frame was previously in a 'fullscreen' size.

This should restore your frame to its previous size.

Known bug: sometimes only one of the two dimensions gets restored.
"
  (interactive)
  (if (not f)
      (setq f (selected-frame))
    )
  ;; First make sure to be out of the fullscren mode
  (cond
   ( (eq system-type 'windows-nt)
     ;; under Windows
     (w32-send-sys-command #xf120) ;; from Windows WM_SYSCOMMAND: SC_RESTORE
     )
   ( t
     ;; default to internal method
     (set-frame-parameter f 'fullscreen 'nil)
     )
   )
  ;; see if some info is stored about previous sizes and restore them.
  ;; NOTE: this seem to be a little buggy: maybe a synchronisation
  ;; problem between the wm command to take effect and the execution
  ;; of the following instruction ???
  (when (assq 'previous-width (frame-parameters f))
    (set-frame-width f (frame-parameter f 'previous-width)))
  (when (assq 'previous-height (frame-parameters f))
    (set-frame-height f (frame-parameter f 'previous-height)))
  (when (assq 'is-fullscreen (frame-parameters f))
    (set-frame-parameter f 'is-fullscreen nil)
    )
  )

(defun frame-is-set-to-fullscreen (&optional f)
  "Check wether the frame is fullscreen.
This function is here to take into account the hacks from frame-set-fullscreen"
  (if (not f)
      (setq f (selected-frame))
    )
  (or (frame-parameter f 'is-fullscreen) 
      (frame-parameter f 'fullscreen))
  )


(defun toggle-fullscreen (&optional f)
  "Switch between the fullscreen and custom sized frames"
  (interactive)
  (if (not f)
      (setq f (selected-frame))
    )
  (if (frame-is-set-to-fullscreen f) 
      (frame-set-fullscreen-off f)
    (frame-set-fullscreen f)
    )
  )

;; A monkeypatch to cause annotate to ignore whitespace
(defun vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 0 name "blame" "-w" rev)))

(provide 'starter-kit-defuns)
;;; starter-kit-defuns.el ends here
