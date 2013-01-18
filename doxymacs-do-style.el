;;; -*-emacs-lisp-*-
;;; doxymacs-do-style.el --- A doxymacs style enforcing DOCE convention.
;
;; Author: Thibauld Nion
;; Created 30/05/2012
;; Keywords: doxygen documentation


;; -------------------------------------------------------------------------------
;; Setting new values for some customizable variables
;; -------------------------------------------------------------------------------

;; The character to use to introduce Doxygen commands
(setq doxymacs-command-character "@")

;; String to insert to start a new member comment
(setq doxymacs-member-comment-start "//!< ")

;; String to insert to end a new member comment
(setq doxymacs-member-comment-end "//!")


;; -------------------------------------------------------------------------------
;; Creating specific tempo templates
;; -------------------------------------------------------------------------------

(defconst doxymacs-do-blank-multiline-comment-template
 '("//! " > n "//! " p > n "//! " > n >)
 "Default do-style template for a blank multiline doxygen comment.")

(defconst doxymacs-do-blank-singleline-comment-template
 '("//! " > p)
 "Default do-style template for a blank single line doxygen comment.")

(defconst doxymacs-do-file-comment-template
 '("//!" > n
   "//! " (doxymacs-doxygen-command-char) "file   "
   (if (buffer-file-name)
       (file-name-nondirectory (buffer-file-name))
     "") > n
   "//! " (doxymacs-doxygen-command-char) "author " (user-full-name)
   ;; (doxymacs-user-mail-address)
   > n
   ;; "//! " (doxymacs-doxygen-command-char) "date   " (current-time-string) > n
   ;; "//! " > n
   "//! "  (p "Brief description of this file: ") > n
   "//! " > n
   "//! " p > n
   "//!" > n > )
 "Default do-style template for file documentation.")


(defconst doxymacs-do-function-comment-template
 '((let ((next-func (doxymacs-find-next-func)))
     (if next-func
	 (list
	  'l
	  "//!" '> 'n
	  "//! " 'p '> 'n
	  "//! " '> 'n
	  ;; "//! " '> 'n
	  (doxymacs-parm-tempo-element (cdr (assoc 'args next-func)))
	  (unless (string-match
                   (regexp-quote (cdr (assoc 'return next-func)))
                   doxymacs-void-types)
	    '(l "//!  " > n "//! " (doxymacs-doxygen-command-char)
		"return " (p "Returns: ") > n))
	  "//!" '> 'n )
       (progn
	 (error "Can't find next function declaraton.")
	 nil))))
 "Default do-style template for function documentation.")



;; -------------------------------------------------------------------------------
;; Redefining uncustomizable functions
;; -------------------------------------------------------------------------------


(defun doxymacs-parm-tempo-element (parms)
  "Inserts tempo elements for the given parms in the given style."
  (if parms
      (let ((prompt (concat "Parameter " (car parms) ": ")))
	(cond
;; 	 ((string= doxymacs-doxygen-style "JavaDoc")
;; 	  (list 'l " * " (doxymacs-doxygen-command-char)
;; 		"param " (car parms) " " (list 'p prompt) '> 'n
;; 		(doxymacs-parm-tempo-element (cdr parms))))
;; 	 ((string= doxymacs-doxygen-style "Qt")
;; 	  (list 'l " " (doxymacs-doxygen-command-char)
;; 		"param " (car parms) " " (list 'p prompt) '> 'n
;; 		(doxymacs-parm-tempo-element (cdr parms))))
;; 	 ((string= doxymacs-doxygen-style "C++")
;; 	  (list 'l "/// " (doxymacs-doxygen-command-char)
;; 		"param " (car parms) " " (list 'p prompt) '> 'n
;; 		(doxymacs-parm-tempo-element (cdr parms))))
	 ((string= doxymacs-doxygen-style "do")
	  (list 'l "//! " (doxymacs-doxygen-command-char)
		"param " (car parms) " " (list 'p prompt) '> 'n
		(doxymacs-parm-tempo-element (cdr parms))))
	 (t
	  (doxymacs-invalid-style))))
    nil))



(defun doxymacs-insert-grouping-comments (start end)
  "Inserts doxygen grouping comments around the current region."
  (interactive "*r")
  (let* (  
         (groupname 
	    (read-from-minibuffer "Enter name of the group: "))
	 (starter  (or doxymacs-group-comment-start
		      (cond
;; 		       ((string= doxymacs-doxygen-style "JavaDoc")
;; 			"//@{")
;; 		       ((string= doxymacs-doxygen-style "Qt")
;; 			"/*@{//!")
;; 		       ((string= doxymacs-doxygen-style "C++")
;; 			"/// @{")
;; 		       ((string= doxymacs-doxygen-style "Morphee")
;; 			"//@{")
		       ((string= doxymacs-doxygen-style "do")
			(concat 
			 "//!\n//! @addtogroup "
			 groupname
			 "\n//!\n//!@{\n//!"))
		       (t
			(doxymacs-invalid-style)))))
	 (ender (or doxymacs-group-comment-end
		    (cond
;; 		       ((string= doxymacs-doxygen-style "JavaDoc")
;; 			"//@}")
;; 		       ((string= doxymacs-doxygen-style "Qt")
;; 			"/*@}//!")
;; 		       ((string= doxymacs-doxygen-style "C++")
;; 			"/// @}")
;; 		       ((string= doxymacs-doxygen-style "Morphee")
;; 			"//@}")
		       ((string= doxymacs-doxygen-style "do")
			(concat
			 "//! @} doxygroup: "
			 groupname ))
		       (t
			(doxymacs-invalid-style))))))
    (save-excursion
      (goto-char end)
      (end-of-line)
      (insert ender)
      (goto-char start)
      (beginning-of-line)
      (insert starter))))


;; Set style to the new DO one
;; 
;; Beware this is incompatible with the radio list you may get when
;; browsing doxymacs' options
(setq doxymacs-doxygen-style "do")


(provide 'doxymacs-do-style)
