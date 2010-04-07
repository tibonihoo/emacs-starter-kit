;; Le style d'indentation pour Morphee
;; Yeah, groovy Baby !
;;
;; Last Modified
;;  $Date: 2007-03-22 11:32:05 +0100 (jeu, 22 mar 2007) $
;;
;; Dynamic compilation for this file
;; -*-byte-compile-dynamic: t;-*-
;;

;; Actually use tab and don't expand them into spaces!!!
(setq indent-tabs-mode t)

(c-add-style
 "morphee"
 (quote
  (
   ;; Indentation de base = 4 espaces
   (c-basic-offset . 4)
   (c-backslash-column . 48)
   (c-cleanup-list 
    scope-operator
    empty-defun-braces
    list-close-comma)
   (c-comment-only-line-offset . 0)
   (c-electric-pound-behavior)
   (c-hanging-braces-alist
    (brace-list-open)
    (brace-entry-open)
    (substatement-open before after)
    (block-close . c-snug-do-while)
    (extern-lang-open after)
    (inexpr-class-open after)
    (inexpr-class-close before))
   (c-hanging-colons-alist)
   (c-hanging-comment-starter-p . t)
   (c-hanging-comment-ender-p . t)
   (c-offsets-alist
    (string . c-lineup-dont-change)
    (c . c-lineup-C-comments)
    (defun-open . 0)
    (defun-close . 0)
    (defun-block-intro . +)
    (class-open . 0)
    (class-close . 0)
    (inline-open . 0)
    (inline-close . 0)
    (func-decl-cont . +)
    (knr-argdecl-intro . +)
    (knr-argdecl . 0)
    (topmost-intro . 0)
    (topmost-intro-cont . +)
    (member-init-intro . +)
    (member-init-cont . 0)
    (inher-intro . +)
    (inher-cont . c-lineup-multi-inher)
    (block-open . 0)
    (block-close . 0)
    (brace-list-open . 0)
    (brace-list-close . 0)
    (brace-list-intro . +)
    (brace-list-entry . 0)
    (brace-entry-open . 0)
    (statement . 0)
    (statement-cont . +)
    (statement-block-intro . +)
    (statement-case-intro . +)
    (statement-case-open . 0)
    (substatement . +)

    ;; Pas de décalage lors de l'ouverture d'un bloc
    (substatement-open . 0)
    (case-label . 0)
    (access-label . -)
    (label . 0)
    (do-while-closure . 0)
    (else-clause . 0)
    (catch-clause . 0)
    (comment-intro . c-lineup-comment)
    (arglist-intro . +)
    (arglist-cont . 0)
    (arglist-cont-nonempty . c-lineup-arglist)
    (arglist-close . +)
    (stream-op . c-lineup-streamop)
    (inclass . +)
    (cpp-macro . -1000)
    (cpp-macro-cont . c-lineup-dont-change)
    (friend . 0)
    (objc-method-intro . -1000)
    (objc-method-args-cont . c-lineup-ObjC-method-args)
    (objc-method-call-cont . c-lineup-ObjC-method-call)
    (extern-lang-open . 0)
    (extern-lang-close . 0)
    (inextern-lang . +)
    (namespace-open . 0)
    (namespace-close . 0)
    (innamespace . +)
    (template-args-cont . +)
    (inlambda . c-lineup-inexpr-block)
    (lambda-intro-cont . +)
    (inexpr-statement . 0)
    (inexpr-class . +)
    )
   )
  )
 )


;; Switch function
(defun morphee-style ()
  "Switch to morphee coding style"
  (interactive)
  ;; Actually use tab and don't expand them !!!
  (setq indent-tabs-mode t)
  (setq tab-width 4)
  (c-set-style "morphee")
)

(defun start-morphee-session ()
  "Starts a mrophe session"
  (interactive)
  (setq-default indent-tabs-mode t)
  (find-file-existing "~/morphee/src")
  (require 'cedet)
  (require 'ecb-autoloads)
)

(provide 'morphee-c-style)

;; Automatic byte compiling (from Jesper Kjær Pedersen <blackie@imada.ou.dk>)
;;; Local Variables: ***
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil) ***
;;; write-file-hooks: (byte-compile-this-file) ***
;;; End: ***
