;;; -*-emacs-lisp-*-
;;; doxymacs-python.el --- ELisp package for making doxygen related stuff easier.
;;
;; Copyright (C) 2006-2009 Thibauld Nion
;;
;; Author: Thibauld Nion
;; Created: 06/11/2006
;; Version: 1.8.0
;; Keywords: doxygen documentation
;;
;; This file is NOT part of GNU Emacs or XEmacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;
;; $Id$

;; Commentary:
;;
;; Doxymacs-Python depends on the following packages:
;;
;; - W3      http://www.cs.indiana.edu/usr/local/www/elisp/w3/docs.html
;; - tempo   http://www.lysator.liu.se/~davidk/elisp/
;;
;;
;; Doxymacs-python is a quick adaptation of the original Doxymacs
;; written by Ryan T. Sammartino 
;;
;; Doxymacs homepage: http://doxymacs.sourceforge.net/
;;
;; The main changes compared to the original Doxymacs for C/C++ are:
;; 
;; - use of the Python way of starting comments and the adapted
;; symbols defined by Doxygen to indicate groups and keywords...
;;
;; - documentation parsing stuff which I don't use, and don't want to maintain
;;
;; - added a function to insert subgroups (the ones defined by @name)
;;
;;
;; Usage and configuration:
;;
;; - Put (require 'doxymacs-python) in your .emacs
;;
;; - Invoke doxymacs-python-mode with M-x doxymacs-python-mode.  To
;;   have doxymacs-python-mode invoked automatically when in Python
;;   mode, put
;;
;;   (add-hook 'python-mode-hook 'doxymacs-python-mode)
;;
;;   in your .emacs.
;;
;; - If you want Doxygen keywords fontified use M-x doxymacs-python-font-lock.
;;   To do it automatically, add the following to your .emacs:
;;
;;    (defun my-doxymacs-python-font-lock-hook ()
;; 	(if (eq major-mode 'python-mode)
;; 	    (doxymacs-python-font-lock)
;; 	  )
;; 	)
;;   (add-hook 'font-lock-mode-hook 'my-doxymacs-python-font-lock-hook)
;;
;;   This will add the Doxygen keywords to python-mode only.
;;
;; - Default key bindings are:
;;   - C-c d ? will look up documentation for the symbol under the point.
;;   - C-c d r will rescan your Doxygen tags file.
;;   - C-c d f will insert a Doxygen comment for the next function.
;;   - C-c d i will insert a Doxygen comment for the current file.
;;   - C-c d ; will insert a Doxygen comment for the current member.
;;   - C-c d m will insert a blank multiline Doxygen comment.
;;   - C-c d s will insert a blank singleline Doxygen comment.
;;   - C-c d @ will insert grouping comments around the current region.
;;
;; Doxymacs-Python has been tested on and works with:
;; - GNU Emacs 22, 23
;;
;; If you have success or failure with other version of {X}Emacs, please
;; let the authors know.

;; Front matter and variables

(provide 'doxymacs-python)

(require 'custom)
(require 'tempo)

(defconst doxymacs-python-version "1.8.0"
  "Doxymacs-Python version number")

(defun doxymacs-python-version ()
  "Report the current version of doxymacs-python in the minibuffer."
  (interactive)
  (message "Using doxymacs-python version %s" doxymacs-python-version))


(defgroup doxymacs-python nil
  "Find documentation created by Doxygen, and create Doxygen comments."
  :group 'tools)

(defcustom doxymacs-python-doxygen-style
  "Python"
  "The style of comments to insert into code.
See http://www.stack.nl/~dimitri/doxygen/docblocks.html#docblocks for examples
of the various styles.

Must be \"default\". Setting this variable
to anything else will generate errors."
  :type '(radio 
	  (const :tag "default" "default"))
  :group 'doxymacs-python)

(defcustom doxymacs-python-command-character
  nil
  "The character to use to introduce Doxygen commands when inserting comments.
If nil, then use the default dictated by `doxymacs-python-doxygen-style'.  Otherwise,
must be one of \"@\" or \"\\\"."
  :type '(choice (const :tag "None" nil)
		 string)
  :group 'doxymacs-python)

(defcustom doxymacs-python-use-external-xml-parser
  nil
  "*Use the external (written in C) XML parser or the internal (LISP) parser.
For smallish tag files, you are better off with the internal parser.
For larger tag files, you are better off with the external one.
Set to non-nil to use the external XML parser."
  :type '(choice (const :tag "Yes" t)
		 (const :tag "No" nil))
  :group 'doxymacs-python)

(defcustom doxymacs-python-external-xml-parser-executable
  ""
  "*Where the external XML parser executable is."
  :type 'string
  :group 'doxymacs-python)

(defcustom doxymacs-python-browse-url-function
  'browse-url
  "*Function to call to launch a browser to display Doxygen documentation.
This function should take one argument, a string representing the URL to
display."
  :type 'function
  :group 'doxymacs-python)

(defcustom doxymacs-python-blank-multiline-comment-template
  nil
  "A tempo template to insert for `doxymacs-python-insert-blank-multiline-comment'.
If nil, then a default template based on the current style as indicated
by `doxymacs-python-doxygen-style' will be used.

For help with tempo templates, see http://www.lysator.liu.se/~davidk/elisp/"
  :type 'list
  :group 'doxymacs-python)

(defcustom doxymacs-python-blank-singleline-comment-template
  nil
  "A tempo template to insert for `doxymacs-python-insert-blank-singleline-comment'.
If nil, then a default template based on the current style as indicated
by `doxymacs-python-doxygen-style' will be used.

For help with tempo templates, see http://www.lysator.liu.se/~davidk/elisp/"
  :type 'list
  :group 'doxymacs-python)

(defcustom doxymacs-python-file-comment-template
  nil
  "A tempo template to insert for `doxymacs-python-insert-file-comment'.
If nil, then a default template based on the current style as indicated
by `doxymacs-python-doxygen-style' will be used.

For help with tempo templates, see http://www.lysator.liu.se/~davidk/elisp/"
  :type 'list
  :group 'doxymacs-python)

(defcustom doxymacs-python-function-comment-template
  nil
  "A tempo template to insert for `doxymacs-python-insert-function-comment'.
If nil, then a default template based on the current style as
indicated by `doxymacs-python-doxygen-style' will be used.  Note that the
function `doxymacs-python-find-next-func' is available to you... it returns
an assoc list with the function's name, argument list (BUG: may be
incorrect for parameters that require parentheses), and return
value:

(cdr (assoc 'func (doxymacs-python-find-next-func))) is the function name (string).
(cdr (assoc 'args (doxymacs-python-find-next-func))) is a list of arguments.
(cdr (assoc 'return (doxymacs-python-find-next-func))) is the return type (string).

The argument list is a list of strings.

For help with tempo templates, see http://www.lysator.liu.se/~davidk/elisp/"
  :type 'list
  :group 'doxymacs-python)

(defcustom doxymacs-python-void-types
  "*args self **kwargs"
  "String with variabled names you don't want to see in the automatically generated list of documented arguments.
"
  :type 'string
  :group 'doxymacs-python)

(defcustom doxymacs-python-member-comment-start
  nil
  "String to insert to start a new member comment.
If nil, use a default one based on the current style as indicated by
`doxymacs-python-doxygen-style'."
  :type '(choice (const :tag "None" nil)
		 string)
  :group 'doxymacs-python)

(defcustom doxymacs-python-member-comment-end
  nil
  "String to insert to end a new member comment.
If nil, use a default one based on the current style as indicated by
`doxymacs-python-doxygen-style'.

Should be an empty string if comments are terminated by end-of-line."
  :type '(choice (const :tag "None" nil)
		 string)
  :group 'doxymacs-python)

(defcustom doxymacs-python-group-comment-start
  nil
  "A string to begin a grouping comment (`doxymacs-python-insert-grouping-comments').
If nil, then a default template based on the current style as indicated
by `doxymacs-python-doxygen-style' will be used."
  :type '(choice (const :tag "None" nil)
		 string)
  :group 'doxymacs-python)

(defcustom doxymacs-python-group-comment-end
  nil
  "A string to end a grouping comment (`doxymacs-python-insert-grouping-comments').
If nil, then a default template based on the current style as indicated
by `doxymacs-python-doxygen-style' will be used."
  :type '(choice (const :tag "None" nil)
		 string)
  :group 'doxymacs-python)

;; End of customisable variables

(defvar doxymacs-python-tags-buffers nil
  "The buffers with our Doxygen tags; a list of the form
'((DIR . BUFFER) (...)) where:

 DIR is one of the directories from `doxymacs-python-doxygen-dirs'; and
 BUFFER is the buffer holding the Doxygen tags for that DIR.")

;; The structure of this list has been chosen for ease of use in the
;; completion functions.
(defvar doxymacs-python-completion-lists nil
  "The lists with doxytags completions.
The structure is as follows:

 ( (dir1 . (symbol-1 . ((description-1a . url-1a) (description-1b . url-1b)))
           (symbol-2 . ((description-2a . url-2a))))
   ... )

where

  dir1 is one of the directories from `doxymacs-python-doxygen-dirs';
  symbol-1 is one of the symbols in the associated Doxygen XML file;
  description-1a is one of symbol-1's description from the XML file; and
  url-1a is the associated URL.")

(defvar doxymacs-python-current-completion-list nil
  "The current list we are building")

(defvar doxymacs-python-completion-buffer "*Completions*"
  "The buffer used for displaying multiple completions.")



;; Minor mode implementation

(defvar doxymacs-python-mode nil
  "nil disables doxymacs-python, non-nil enables.")
 
(make-variable-buffer-local 'doxymacs-python-mode)

(defun doxymacs-python-mode (&optional arg)
  ;; All of the following text shows up in the "mode help" (C-h m)
  "Minor mode for using/creating Doxygen documentation.
To submit a problem report, request a feature or get support, please
visit doxymacs-python' homepage at http://doxymacs-python.sourceforge.net/.

To see what version of doxymacs-python you are running, enter
`\\[doxymacs-python-version]'.

In order for `doxymacs-python-lookup' to work you will need to customise the
variable `doxymacs-python-doxygen-dirs'.

Key bindings:
\\{doxymacs-python-mode-map}"
  (interactive "P")
  (setq doxymacs-python-mode
        (if (null arg)
            ;; Toggle mode
            (not doxymacs-python-mode)
          ;; Enable/Disable according to arg
          (> (prefix-numeric-value arg) 0)))
  (when doxymacs-python-mode
    (when (boundp 'filladapt-token-table)
      ;; add tokens to filladapt to match doxygen markup
      (let ((bullet-regexp "[@\\]\\(param\\(?:\\s-*\\[\\(?:in\\|out\\|in,out\\)\\]\\)?\\s-+\\sw+\\|return\\)"))
	(unless (assoc bullet-regexp filladapt-token-table)
	  (setq filladapt-token-table
		(append filladapt-token-table
			(list (list bullet-regexp 'bullet)))))))))

;; Keymap

(defvar doxymacs-python-mode-map (make-sparse-keymap)
  "Keymap for doxymacs-python minor mode.")

(define-key doxymacs-python-mode-map "\C-cd?"
  'doxymacs-python-lookup)
(define-key doxymacs-python-mode-map "\C-cdr"
  'doxymacs-python-rescan-tags)

(define-key doxymacs-python-mode-map "\C-cdf"
  'doxymacs-python-insert-function-comment)
(define-key doxymacs-python-mode-map "\C-cdi"
  'doxymacs-python-insert-file-comment)
(define-key doxymacs-python-mode-map "\C-cdm"
  'doxymacs-python-insert-blank-multiline-comment)
(define-key doxymacs-python-mode-map "\C-cds"
  'doxymacs-python-insert-blank-singleline-comment)
(define-key doxymacs-python-mode-map "\C-cd;"
  'doxymacs-python-insert-member-comment)
(define-key doxymacs-python-mode-map "\C-cd@"
  'doxymacs-python-insert-grouping-comments)
(define-key doxymacs-python-mode-map "\C-cdn" ;; "n" from the @*n*ame command
  'doxymacs-python-insert-subgrouping-comments)


;;;###autoload
(or (assoc 'doxymacs-python-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(doxymacs-python-mode " doxython") minor-mode-alist)))

(or (assoc 'doxymacs-python-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'doxymacs-python-mode doxymacs-python-mode-map)
		minor-mode-map-alist)))

;; This stuff has to do with fontification
;; Thanks to Alec Panovici for the idea.

(defconst doxymacs-python-doxygen-keywords
  (list
   (list
    ;; One shot keywords that take no arguments
    (concat "\\([@\\\\]\\(brief\\|li\\|\\(end\\)?code\\|sa"
	    "\\|note\\|\\(end\\)?verbatim\\|return\\|arg\\|fn"
	    "\\|hideinitializer\\|showinitializer"
	    ;; FIXME
	    ;; How do I get & # < > % to work?
	    ;;"\\|\\\\&\\|\\$\\|\\#\\|<\\|>\\|\\%"
	    "\\|\\$"
	    "\\|internal\\|nosubgrouping\\|author\\|date\\|endif"
	    "\\|invariant\\|post\\|pre\\|remarks\\|since\\|test\\|version"
	    "\\|\\(end\\)?htmlonly\\|\\(end\\)?latexonly\\|f\\$\\|file"
	    "\\|\\(end\\)?xmlonly\\|\\(end\\)?manonly\\|property"
	    "\\|mainpage\\|name\\|overload\\|typedef\\|deprecated\\|par"
	    "\\|addindex\\|line\\|skip\\|skipline\\|until\\|see"
	    "\\|endlink\\|callgraph\\|endcond\\|else\\)\\)\\>")
    '(0 font-lock-keyword-face prepend))
   ;; attention, warning, etc. given a different font
   (list
    "\\([@\\\\]\\(attention\\|warning\\|todo\\|bug\\)\\)\\>"
    '(0 font-lock-warning-face prepend))
   ;; keywords that take a variable name as an argument
   (list
    (concat "\\([@\\\\]\\(param\\(?:\\s-*\\[\\(?:in\\|out\\|in,out\\)\\]\\)?"
	    "\\|a\\|namespace\\|relates\\(also\\)?"
	    "\\|var\\|def\\)\\)\\s-+\\(\\sw+\\)")
    '(1 font-lock-keyword-face prepend)
    '(4 font-lock-variable-name-face prepend))
   ;; keywords that take a type name as an argument
   (list
    (concat "\\([@\\\\]\\(class\\|struct\\|union\\|exception\\|enum"
	    "\\|throw\\|interface\\|protocol\\)\\)\\s-+\\(\\(\\sw\\|:\\)+\\)")
    '(1 font-lock-keyword-face prepend)
    '(3 font-lock-type-face prepend))
   ;; keywords that take a function name as an argument
   (list
    "\\([@\\\\]retval\\)\\s-+\\([^ \t\n]+\\)"
    '(1 font-lock-keyword-face prepend)
    '(2 font-lock-function-name-face prepend))
   ;; bold
   (list
    "\\([@\\\\]b\\)\\s-+\\([^ \t\n]+\\)"
    '(1 font-lock-keyword-face prepend)
    '(2 (quote bold) prepend))
   ;; code
   (list
    "\\([@\\\\][cp]\\)\\s-+\\([^ \t\n]+\\)"
    '(1 font-lock-keyword-face prepend)
    '(2 (quote underline) prepend))
   ;; italics/emphasised
   (list
    "\\([@\\\\]e\\(m\\)?\\)\\s-+\\([^ \t\n]+\\)"
    '(1 font-lock-keyword-face prepend)
    '(3 (quote italic) prepend))
   ;; keywords that take a list
   (list
    "\\([@\\\\]ingroup\\)\\s-+\\(\\(\\sw+\\s-*\\)+\\)\\s-*$"
    '(1 font-lock-keyword-face prepend)
    '(2 font-lock-string-face prepend))
   ;; one argument that can contain arbitrary non-whitespace stuff
   (list
    (concat "\\([@\\\\]\\(link\\|copydoc\\|xrefitem"
	    "\\|if\\(not\\)?\\|elseif\\)\\)"
	    "\\s-+\\([^ \t\n]+\\)")
    '(1 font-lock-keyword-face prepend)
    '(4 font-lock-string-face prepend))
   ;; one optional argument that can contain arbitrary non-whitespace stuff
   (list
    "\\([@\\\\]\\(cond\\|dir\\)\\(\\s-+[^ \t\n]+\\)?\\)"
    '(1 font-lock-keyword-face prepend)
    '(3 font-lock-string-face prepend t))
   ;; one optional argument with no space between
   (list
    "\\([@\\\\]\\(~\\)\\([^ \t\n]+\\)?\\)"
    '(1 font-lock-keyword-face prepend)
    '(3 font-lock-string-face prepend t))
   ;; one argument that has to be a filename
   (list
    (concat "\\([@\\\\]\\(example\\|\\(dont\\)?include\\|includelineno"
	    "\\|htmlinclude\\|verbinclude\\)\\)\\s-+"
	    "\\(\"?[~:\\/a-zA-Z0-9_. ]+\"?\\)")
    '(1 font-lock-keyword-face prepend)
    '(4 font-lock-string-face prepend))
   ;; dotfile <file> ["caption"]
   (list
    (concat "\\([@\\\\]dotfile\\)\\s-+"
	    "\\(\"?[~:\\/a-zA-Z0-9_. ]+\"?\\)\\(\\s-+\"[^\"]+\"\\)?")
    '(1 font-lock-keyword-face prepend)
    '(2 font-lock-string-face prepend)
    '(3 font-lock-string-face prepend t))
   ;; image <format> <file> ["caption"] [<sizeindication>=<size>]
   (list
    "\\([@\\\\]image\\)\\s-+\\(html\\|latex\\)\\s-+\\(\"?[~:\\/a-zA-Z0-9_. ]+\"?\\)\\(\\s-+\"[^\"]+\"\\)?\\(\\s-+\\sw+=[0-9]+\\sw+\\)?"
    '(1 font-lock-keyword-face prepend)
    '(2 font-lock-string-face prepend)
    '(3 font-lock-string-face prepend)
    '(4 font-lock-string-face prepend t)
    '(5 font-lock-string-face prepend t))
   ;; one argument that has to be a word
   (list
    (concat "\\([@\\\\]\\(addtogroup\\|defgroup\\|weakgroup\\|package"
	    "\\|page\\|anchor\\|ref\\|section\\|subsection"
	    "\\)\\)\\s-+\\(\\sw+\\)")
    '(1 font-lock-keyword-face prepend)
    '(3 font-lock-string-face prepend))))

(defun doxymacs-python-font-lock ()
  "Turn on font-lock for Doxygen keywords."
  ;; FIXME How do I turn *off* font-lock for Doxygen keywords?
  (interactive)
  (modify-syntax-entry ?_ "w")
  (if (functionp 'font-lock-add-keywords)
      ;; Use new (proper?) font-lock-add-keywords function
      (font-lock-add-keywords nil doxymacs-python-doxygen-keywords)
    ;; Use old-school way
    (let ((old (if (eq (car-safe font-lock-keywords) t)
		 (cdr font-lock-keywords)
	       font-lock-keywords)))
      (setq font-lock-keywords (append old doxymacs-python-doxygen-keywords)))))




;; These functions have to do with inserting doxygen commands in code

;; FIXME
;; So, in the source code for XEmacs 21.1.14, they commented out the
;; definition of deactivate-mark for some reason... and the tempo package
;; needs it.  So, here is a placeholder just to get it to stop
;; complaining. This is a hack, since I don't know what the proper fix
;; should be.
(if (not (fboundp 'deactivate-mark))
    (defsubst deactivate-mark ()
      (zmacs-deactivate-region)))	; Is this correct?
;; Also need a hack for mark-active
(if (not (boundp 'mark-active))
    (defvar mark-active nil))		; Is this correct? Probably not.


;; Default templates


(defconst doxymacs-python-default-blank-multiline-comment-template
 '("##  " p > n "# " (doxymacs-python-doxygen-command-char) "brief" > n "#" > n> "#")
 "Default default-style template for a blank multiline doxygen comment.")

(defconst doxymacs-python-default-blank-singleline-comment-template
 '("## " > p)
 "Default default-style template for a blank single line doxygen comment.")

(defun doxymacs-python-doxygen-command-char ()
  (cond
   (doxymacs-python-command-character doxymacs-python-command-character)
   ((string= doxymacs-python-doxygen-style "default") "@")
   (t "@")))

(defun doxymacs-python-user-mail-address ()
  "Return the user's email address"
  (or
   (and (and (fboundp 'user-mail-address) (user-mail-address))
	(list 'l " <" (user-mail-address) ">"))
   (and (and (boundp 'user-mail-address) user-mail-address)
	(list 'l " <" user-mail-address ">"))))


(defconst doxymacs-python-default-file-comment-template
 '("#" > "#" n
   "# " (doxymacs-python-doxygen-command-char) "file   "
   (if (buffer-file-name)
       (file-name-nondirectory (buffer-file-name))
     "") > n
     "# " (doxymacs-python-doxygen-command-char) "author " (user-full-name)
     ;; (doxymacs-python-user-mail-address)
     > n
     "# " > n
     ;; "# " (doxymacs-python-doxygen-command-char) "date   " (current-time-string) > n
     ;; "# " > n
     "# " (doxymacs-python-doxygen-command-char) "brief  " (p "Brief description of this file: ") > n
     "# " p > n )
 "Default default-style template for file documentation.")


(defun doxymacs-python-parm-tempo-element (parms)
  "Inserts tempo elements for the given parms in the given style."
  (if parms
      (let ((prompt (concat "Parameter " (car parms) ": ")))
	(cond
	 ((string= doxymacs-python-doxygen-style "default")
	  (list 'l " # " (doxymacs-python-doxygen-command-char)
		"param " (car parms) " " (list 'p prompt) '> 'n
		(doxymacs-python-parm-tempo-element (cdr parms))))
	 (t
	  (doxymacs-python-invalid-style))))
    nil))

(defconst doxymacs-python-default-function-comment-template
  '((let ((next-func (doxymacs-python-find-next-func)))
      (if next-func
	  (list
	   'l
	   "#" '> "#" 'n
	  " # " (doxymacs-python-doxygen-command-char) "brief " 'p '> 'n
	  " # " '> 'n
	  
	  (doxymacs-python-parm-tempo-element (cdr (assoc 'args next-func)))
	  " # " '> )
       (progn
	 (error "Can't find next function declaraton.")
	 nil))))
 "Default default-style template for function documentation."
)
;; (defconst doxymacs-python-default-subgroup-starter-template
;;   )
;;  "Default default-style template for function documentation.")
(defun doxymacs-python-invalid-style ()
  "Warn the user that he has set `doxymacs-python-doxygen-style' to an invalid
style."
  (error (concat
	  "Invalid `doxymacs-python-doxygen-style': "
	  doxymacs-python-doxygen-style
	  ": must be \"default\" .")))

;; This should make it easier to add new templates and cut down
;; on copy-and-paste programming.
(defun doxymacs-python-call-template (template-name)
  "Insert the given template."
  (let* ((user-template-name (concat "doxymacs-python-" template-name "-template"))
	 (user-template (car (read-from-string user-template-name)))
	 (default-template-name (concat "doxymacs-python-"
					doxymacs-python-doxygen-style "-"
					template-name "-template"))
	 (default-template (car (read-from-string default-template-name))))
    (cond
     ((and (boundp user-template)	; Make sure it is a non-nil list
	   (listp (eval user-template))
	   (eval user-template))
      ;; Use the user's template
      (tempo-insert-template user-template tempo-insert-region))
     ((and (boundp default-template)
	   (listp (eval default-template))
	   (eval default-template))
      ;; Use the default template, based on the current style
      (tempo-insert-template default-template tempo-insert-region))
     (t
      ;; Most likely, `doxymacs-python-doxygen-style' has been set wrong.
      (doxymacs-python-invalid-style)))))

(defun doxymacs-python-insert-blank-multiline-comment ()
  "Inserts a multi-line blank Doxygen comment at the current point."
  (interactive "*")
  (doxymacs-python-call-template "blank-multiline-comment"))

(defun doxymacs-python-insert-blank-singleline-comment ()
  "Inserts a single-line blank Doxygen comment at current point."
  (interactive "*")
  (doxymacs-python-call-template "blank-singleline-comment"))

(defun doxymacs-python-insert-file-comment ()
  "Inserts Doxygen documentation for the current file at current point."
  (interactive "*")
  (doxymacs-python-call-template "file-comment"))

(defun doxymacs-python-insert-function-comment ()
  "Inserts Doxygen documentation for the next function declaration at
current point."
  (interactive "*")
  (doxymacs-python-call-template "function-comment"))

;; FIXME
;; The following was borrowed from "simple.el".
;; If anyone knows of a better/simpler way of doing this, please let me know.
(defconst doxymacs-python-comment-indent-function
  (lambda (skip)
    (save-excursion
      (beginning-of-line)
      (let ((eol (save-excursion (end-of-line) (point))))
	(and skip
	     (re-search-forward skip eol t)
	     (setq eol (match-beginning 0)))
	(goto-char eol)
	(skip-chars-backward " \t")
	(max comment-column (1+ (current-column))))))
  "Function to compute desired indentation for a comment.
This function is called with skip and with point at the beginning of
the comment's starting delimiter.")

(defun doxymacs-python-insert-member-comment ()
  "Inserts Doxygen documentation for the member on the current line in
the column given by `comment-column' (much like \\[indent-for-comment])."
  (interactive "*")
  (let* ((empty (save-excursion (beginning-of-line)
				(looking-at "[ \t]*$")))
	 (starter (or doxymacs-python-member-comment-start
		      (cond
		       ((string= doxymacs-python-doxygen-style "default")
			"##< ")
		       (t
			(doxymacs-python-invalid-style)))))
	 (skip (concat (regexp-quote starter) "#"))
	 (ender (or doxymacs-python-member-comment-end
		    (cond
		       ((string= doxymacs-python-doxygen-style "default")
			" #")
		       (t
			(doxymacs-python-invalid-style))))))
    (if empty
	;; Insert a blank single-line comment on empty lines
	(doxymacs-python-insert-blank-singleline-comment)
      (if (null starter)
	  (error "No Doxygen member comment syntax defined")
	(let* ((eolpos (save-excursion (end-of-line) (point)))
	       cpos indent begpos)
	  (beginning-of-line)
	  (if (re-search-forward skip eolpos 'move)
	      (progn (setq cpos (point-marker))
		     ;; Find the start of the comment delimiter.
		     ;; If there were paren-pairs in skip,
		     ;; position at the end of the first pair.
		     (if (match-end 1)
			 (goto-char (match-end 1))
		       ;; If skip matched a string with
		       ;; internal whitespace (not final whitespace) then
		       ;; the delimiter start at the end of that
		       ;; whitespace.  Otherwise, it starts at the
		       ;; beginning of what was matched.
		       (skip-syntax-backward " " (match-beginning 0))
		       (skip-syntax-backward "^ " (match-beginning 0)))))
	  (setq begpos (point))
	  ;; Compute desired indent.
	  (cond
	   ((= (current-column) 0)
	    (goto-char begpos))
	   ((= (current-column)
	       (setq indent (funcall doxymacs-python-comment-indent-function skip)))
	    (goto-char begpos))
	   (t
	    ;; If that's different from current, change it.
	    (skip-chars-backward " \t")
	    (delete-region (point) begpos)
	    (indent-to indent)))
	  ;; An existing comment?
	  (if cpos
	      (progn (goto-char cpos)
		     (set-marker cpos nil))
	    ;; No, insert one.
	    (insert starter)
	    (save-excursion
	      (insert ender))))))))

(defun doxymacs-python-insert-grouping-comments (start end)
  "Inserts doxygen grouping comments around the current region."
  (interactive "*r")
  (let* (
	 (groupname 
	  (read-from-minibuffer "Enter name of the group: "))
         (starter  (or doxymacs-python-group-comment-start
		      (cond
		       ((string= doxymacs-python-doxygen-style "default")
			(concat "##\n# " (doxymacs-python-doxygen-command-char) "addtogroup " groupname"\n##@{\n" ))
		       (t
			(doxymacs-python-invalid-style)))))
	 (ender (or doxymacs-python-group-comment-end
		    (cond
		       ((string= doxymacs-python-doxygen-style "default")
			(concat"\n# doxygroup: "  groupname "\n##@}" "\n"))
		       (t
			(doxymacs-python-invalid-style))))))
    (save-excursion
      (goto-char end)
      (end-of-line)
      (insert ender)
      (goto-char start)
      (beginning-of-line)
      (insert starter))))



(defun doxymacs-python-insert-subgrouping-comments (start end)
  "Inserts doxygen subgrouping comments around the current region."
  (interactive "*r")
  
  (let* (
	 (groupname 
	  (read-from-minibuffer "Enter name of the subgroup: "))
	 (starter-template '(
			     "#" > "#" n
			     "# " (doxymacs-python-doxygen-command-char) "name " groupname > n
			     "# " > n
			     "# @{" > n n
			     ))
	 (ender-template '( n "# doxysubgroup: " groupname > n
			      "#" > "# @} " n
			    ))
	 )
    (save-excursion
      (goto-char end)
      (end-of-line)
;;       (insert ender)
      (tempo-insert-template 'ender-template tempo-insert-region)
      (goto-char start)
      (beginning-of-line)
;;       (insert starter)
      (tempo-insert-template 'starter-template tempo-insert-region)
      )))

;; These are helper functions that search for the next function
;; declerations/definition and extract its name, return type and
;; argument list.  Used for documenting functions.

(defun doxymacs-python-extract-args-list (args-string)
  "Extracts the arguments from the given list (given as a string)."
  (cond
   ;; arg list is empty
   ((string-match "\\`[ \t\n]*\\'" args-string)
    nil)
   ;; arg list is empty
   ((string-match "\\`[ \t\n]self[ \t\n]*\\'" args-string)
    nil)
   ;;argument list consists of one word
   ((string-match "\\`[ \t\n]*\\([a-zA-Z0-9_]+\\)[ \t\n]*\\'" args-string)
    ;; ... extract this word
    (let ((arg (substring args-string (match-beginning 1) (match-end 1))))
      ;; if this arg is a void type return nil
      (if (string-match (regexp-quote arg) doxymacs-python-void-types)
          nil
        ;; else return arg
        (list arg))))
   ;; else split the string and extact var names from args
   (t
    (doxymacs-python-extract-args-list-helper
     (doxymacs-python-save-split args-string)))))


(defun doxymacs-python-save-split (args-string)
  "Splits a declaration list as string and returns list of single
declarations."
  (let ((comma-pos (string-match "," args-string))
        (paren-pos (string-match "(" args-string)))
    (cond
     ;; no comma in string found
     ((null comma-pos)     (list args-string))
     ;; comma but no parenthethes: split-string is save
     ((null paren-pos)     (split-string args-string ","))
     ;; comma first then parenthesis
     ((< comma-pos paren-pos)
      (cons (substring args-string 0 comma-pos)
            (doxymacs-python-save-split (substring args-string (1+ comma-pos)))))
     ;; parenthesis first then comma. there must exist a closing parenthesis
     (t
      ;; cut off the (...) part
      (save-excursion
        ;; create temporary buffer
        (set-buffer (get-buffer-create "*doxymacs-python-scratch*"))
        (erase-buffer)
        (insert args-string)
        (beginning-of-buffer)
        (search-forward "(")
        (prog1
            (let ((depth 1)
                  (exit)
                  (comma-found))
              (while (not exit)
                ;; step through buffer
                (forward-char 1)
                (cond
                 ;; end of buffer: exit
                 ((= (point) (point-max)) (setq exit t))
                 ;; decrease depth counter
                 ((looking-at ")")        (setq depth (1- depth)))
                 ;; increase depth counter
                 ((looking-at "(")        (setq depth (1+ depth)))
                 ;; comma at depth 0, thats it!
                 ((and (looking-at ",") (= 0 depth))
                  (setq exit t)
                  (setq comma-found t))))
              (if (not comma-found)
                  ;; whole string is one arg
                  (list (buffer-substring 1 (point)))
                ;; else split at comma ...
                (cons (buffer-substring 1 (point))
                      ;; and split rest of declaration list
                      (doxymacs-python-save-split
                       (buffer-substring (1+ (point)) (point-max))))))
          (kill-buffer (current-buffer))))))))


;; This regexp fails if the opt. parentheses
;; contain another level of parentheses.  E.g. for:
;; int f(int (*g)(int (*h)()))
(defun doxymacs-python-extract-args-list-helper (args-list)
  "Recursively get names of arguments."
  (if args-list
      (if (string-match 
	   "\\([ \t\n]*self[ \t\n]*\\)"
	   (car args-list)
	   )
	  (progn
	    (substring (car args-list) (match-beginning 1) (match-end 1))
	    (doxymacs-python-extract-args-list-helper (cdr args-list))
	    )
    
	(if args-list
	    (if (string-match
		 (concat
		  "\\("
		  "([ \t\n]*\\*[ \t\n]*\\([a-zA-Z0-9_]+\\)[ \t\n]*)" ; (*varname)
		  "\\|"					 ; or
		  "\\*?[ \t\n]*\\([a-zA-Z0-9_]+\\)" ; opt. *, varname
		  "\\)"
		  "[ \t\n]*"				 ; opt. spaces
		  "\\(\\[[ \t\n]*[a-zA-Z0-9_]*[ \t\n]*\\]\\|" ; opt. array bounds
		  "([^()]*)\\)?"	; or opt. func args
		  "[ \t\n]*"		; opt. spaces
		  "\\(=[ \t\n]*[^ \t\n]+[ \t\n]*\\)?" ; optional assignment
		  "[ \t\n]*\\'"			      ; end
		  ) (car args-list))
		(cons
		 (cond
		  ;; var name in: (*name)
		  ((match-beginning 2)
		   (substring (car args-list) (match-beginning 2) (match-end 2)))
		  ;; var name in: *name
		  ((match-beginning 3)
		   (substring (car args-list) (match-beginning 3) (match-end 3)))
		  ;; no match: return complete declaration
		  (t
		   (car args-list)))
		 (doxymacs-python-extract-args-list-helper (cdr args-list)))
	      ;; else there is no match
	      nil))
	))
  )

(defun doxymacs-python-core-string (s)
  "Returns the argument string with leading and trailing blank
and new-line characters cut off."
  (string-match "\\`[ \t\n]*\\(.*?\\)[ \t\n]*\\'" s)
  (if (match-beginning 1)
      (substring s (match-beginning 1) (match-end 1))
    s))

(defun doxymacs-python-find-next-func ()
  "Returns a list describing next function declaration, or nil if not found.

(cdr (assoc 'func (doxymacs-python-find-next-func))) is the function name (string).
(cdr (assoc 'args (doxymacs-python-find-next-func))) is a list of arguments.

The argument list is a list of strings."
  (interactive)
  (save-excursion
    (if (re-search-forward
	 (concat
	  ;; definition			;
	  "\\(def[ \t\n]+\\)+"
	  ;; name			;
	  "\\([a-zA-Z0-9_]\\)+"
	  "[ \t\n]*("
	  ) nil t)
        (let* ((func (buffer-substring (match-end 1) (match-end 2)))
	       (args (buffer-substring (point) (progn
                                                (backward-char 1)
                                                (forward-list)
                                                (backward-char 1)
                                                (point))))
	       )
	  (list (cons 'func func)
 		(cons 'args (doxymacs-python-extract-args-list args))
		)
        )
)))

;;; doxymacs-python.el ends here
