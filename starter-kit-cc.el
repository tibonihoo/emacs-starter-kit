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
     (setq sourcepair-source-path  (append sourcepair-source-path '("../../src/*" "../src" "../source" "./src" "./source" "../*")))
     (setq sourcepair-header-path  (append sourcepair-header-path '("../../include/*" "../include" "../inc" "../*")))
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
           (option-list (add-clang-include-options-for-ede-project (list "-Wall" "-Werror" "-fsyntax-only" "-fno-color-diagnostics" "-x" "c++" "-std=c++14"  "-DDEFAULT_IP_IMPL=ip_ipp" local-file)))
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


;; Fontify C++11+ novelties
;; http://stackoverflow.com/questions/8549351/c11-mode-or-settings-for-emacs
(add-hook
 'c++-mode-hook
 '(lambda()
    ;; We could place some regexes into `c-mode-common-hook', but note that their evaluation order
    ;; matters.
    (font-lock-add-keywords
     nil '(;; complete some fundamental keywords
           ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
           ;; namespace names and tags - these are rendered as constants by cc-mode
           ("\\<\\(\\w+::\\)" . font-lock-function-name-face)
           ;;  new C++11 keywords
           ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
           ("\\<\\(char16_t\\|char32_t\\)\\>" . font-lock-keyword-face)
           ;; PREPROCESSOR_CONSTANT, PREPROCESSORCONSTANT
           ("\\<[A-Z]*_[A-Z_]+\\>" . font-lock-constant-face)
           ("\\<[A-Z]\\{3,\\}\\>"  . font-lock-constant-face)
           ;; hexadecimal numbers
           ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
           ;; integer/float/scientific numbers
           ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
           ;; c++11 string literals
           ;;       L"wide string"
           ;;       L"wide string with UNICODE codepoint: \u2018"
           ;;       u8"UTF-8 string", u"UTF-16 string", U"UTF-32 string"
           ("\\<\\([LuU8]+\\)\".*?\"" 1 font-lock-keyword-face)
           ;;       R"(user-defined literal)"
           ;;       R"( a "quot'd" string )"
           ;;       R"delimiter(The String Data" )delimiter"
           ;;       R"delimiter((a-z))delimiter" is equivalent to "(a-z)"
           ("\\(\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\)" 1 font-lock-keyword-face t) ; start delimiter
           (   "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\(.*?\\))[^\\s-\\\\()]\\{0,16\\}\"" 1 font-lock-string-face t)  ; actual string
           (   "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(.*?\\()[^\\s-\\\\()]\\{0,16\\}\"\\)" 1 font-lock-keyword-face t) ; end delimiter

           ;; user-defined types (rather project-specific)
           ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(type\\|ptr\\)\\>" . font-lock-type-face)
           ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)
           ))
    ) t)


;; Indent enum class correctly
;; taken from http://stackoverflow.com/questions/6497374/emacs-cc-mode-indentation-problem-with-c0x-enum-class
(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t]+class[ \t]+[^}]+"))))

(defun align-enum-class (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      '-
    '+))

(defun fix-enum-class ()
  "Setup `c++-mode' to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist
               '(statement-cont . align-enum-class-closing-brace)))

(add-hook 'c++-mode-hook 'fix-enum-class)


(provide 'starter-kit-cc)
;; starter-kit-cc.el ends here

