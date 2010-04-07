;;; -*-emacs-lisp-*-
;;; doxymacs-dr-style.el --- A doxymacs style enforcing DR convention.
;
;; Author: Thibauld Nion
;; Created 14/06/2009
;; Keywords: doxygen documentation


;; -------------------------------------------------------------------------------
;; Setting new values for some customizable variables
;; -------------------------------------------------------------------------------

;; The character to use to introduce Doxygen commands
(setq doxymacs-command-character "@")

;; String to insert to start a new member comment
(setq doxymacs-member-comment-start "/**< ")

;; String to insert to end a new member comment
(setq doxymacs-member-comment-end " */")


;; -------------------------------------------------------------------------------
;; Creating specific tempo templates
;; -------------------------------------------------------------------------------

(defconst doxymacs-dr-blank-multiline-comment-template
 '("/** " > n "* @brief " p > n "* " > n "*/" > n >)
 "Default dr-style template for a blank multiline doxygen comment.")

(defconst doxymacs-dr-blank-singleline-comment-template
 '("//! " > p)
 "Default dr-style template for a blank single line doxygen comment.")

(defconst doxymacs-dr-file-comment-template
 '("/**" > n
   " * " (doxymacs-doxygen-command-char) "file   "
   (if (buffer-file-name)
       (file-name-nondirectory (buffer-file-name))
     "") > n
   " * " (doxymacs-doxygen-command-char) "author " (user-full-name)
   ;; (doxymacs-user-mail-address)
   > n
   ;; " * " (doxymacs-doxygen-command-char) "date   " (current-time-string) > n
   ;; " * " > n
   " * " (doxymacs-doxygen-command-char) "brief  " (p "Brief description of this file: ") > n
   " * " > n
   " * " p > n
   "*/" > n )
 "Default dr-style template for file documentation.")


(defconst doxymacs-dr-function-comment-template
 '((let ((next-func (doxymacs-find-next-func)))
     (if next-func
	 (list
	  'l
	  "/**" '> 'n
	  " * @brief " 'p '> 'n
	  " * " '> 'n
	  ;; " * " '> 'n
	  (doxymacs-parm-tempo-element (cdr (assoc 'args next-func)))
	  (unless (string-match
                   (regexp-quote (cdr (assoc 'return next-func)))
                   doxymacs-void-types)
	    '(l " *  " > n " * " (doxymacs-doxygen-command-char)
		"return " (p "Returns: ") > n))
	  " */" '> 'n )
       (progn
	 (error "Can't find next function declaraton.")
	 nil))))
 "Default dr-style template for function documentation.")



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
	 ((string= doxymacs-doxygen-style "dr")
	  (list 'l " * " (doxymacs-doxygen-command-char)
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
;; 			"/*@{*/")
;; 		       ((string= doxymacs-doxygen-style "C++")
;; 			"/// @{")
;; 		       ((string= doxymacs-doxygen-style "Morphee")
;; 			"//@{")
		       ((string= doxymacs-doxygen-style "dr")
			(concat 
			 "/**\n * @addtogroup "
			 groupname
			 "\n *\n *@{\n */"))
		       (t
			(doxymacs-invalid-style)))))
	 (ender (or doxymacs-group-comment-end
		    (cond
;; 		       ((string= doxymacs-doxygen-style "JavaDoc")
;; 			"//@}")
;; 		       ((string= doxymacs-doxygen-style "Qt")
;; 			"/*@}*/")
;; 		       ((string= doxymacs-doxygen-style "C++")
;; 			"/// @}")
;; 		       ((string= doxymacs-doxygen-style "Morphee")
;; 			"//@}")
		       ((string= doxymacs-doxygen-style "dr")
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


;; Set style to the new Dr one
;; 
;; Beware this is incompatible with the radio list you may get when
;; browsing doxymacs' options
(setq doxymacs-doxygen-style "dr")


(provide 'doxymacs-dr-style)
