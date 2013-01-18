;;
;; Additional customisation on top of the starter-kit's defaults.
;;
;; (c) 2010 Thibauld Nion
;;

;; Personal info for signatures in code and stuff...
(set 'user-full-name "Thibauld Nion")
(set 'user-mail-address "tnion@dxo.com")
;; Work context
(defvar my-work-context "DO")

(if (eq system-type 'windows-nt)
    (setq backup-directory-alist `(("." . ,(expand-file-name "d:/Perso/emacs/backups"))))
  )

(if (eq system-type 'windows-nt)
    (progn
      ;; Set prefered frame size
      (add-to-list 'default-frame-alist '(height . 59))
      (add-to-list 'default-frame-alist '(width . 100))
      (add-to-list 'default-frame-alist '(top + -3))
      (add-to-list 'default-frame-alist '(left + -1))
      )
  (progn
    ;; for my linux workstation
    (frame-set-fullscreen-off)
    (add-to-list 'default-frame-alist '(height . 66))
    (add-to-list 'default-frame-alist '(width . 281))
    ))

(if (eq system-type 'windows-nt)
    (set-face-attribute 'default nil :family "Consolas" :height 105)
  ;; else use lucida
  (set-face-attribute 'default nil :family "b&h-lucidatypewriter")
  )


(add-hook 'c-mode-common-hook
          '(lambda () 
             ;; Enable "hungry delete":
             ;; use: 
             ;;    *backspace deletes all contiguous blank spaces in one stroke; 
             ;;    * C-d performs an "usual" backspace.
             ;;
             (c-toggle-hungry-state 1)
             ;; Use custom c/c++ style
             (require 'cc-do-style)
             (do-style)
             (doxymacs-mode)
             (require 'doxymacs-do-style)
             ;;
             ;; Add a new function for doxygen comments
             (defun doxymacs-insert-subgrouping-comments (start end)
               "Inserts doxygen subgrouping comments around the current region."
               (interactive "*r")
               (let* (
                      (groupname 
                       (read-from-minibuffer "Enter name of the subgroup: "))
                      (starter-template '(
                                          "  //! " (doxymacs-doxygen-command-char) "name " groupname > n
                                          "  //! " > n
                                          "  //! @{" > n > n

                                          ))
                      (ender-template '( n "  // doxysubgroup: " groupname > n
                                           "  //! @} " > n >
                                           ))
                      )
                 (save-excursion
                   (goto-char end)
                   (end-of-line)
                   ;;       (insert ender)
                   (tempo-insert-template 'ender-template tempo-insert-region)
                   (goto-char start)
                   (beginning-of-line)
                   (tempo-insert-template 'starter-template tempo-insert-region)
                   ;;       (insert starter)
                   )))
             ;; (Re)define some keybindings
             ;; Groups ("g" is easier than @ on fr kbds)
             (define-key doxymacs-mode-map "\C-cdg"
               'doxymacs-insert-grouping-comments)
             ;; Subgroupiong with the  @name command
             (define-key doxymacs-mode-map "\C-cdn" 
               'doxymacs-insert-subgrouping-comments)
             ;; ;; semantic
             ;; (semantic-mode)
             ;; (semantic-idle-scheduler-mode)
             ;; (add-to-list 'semanticdb-project-roots "d:/DATA/WORK/Framework-trunk")
             ;; (add-to-list 'semanticdb-project-roots "d:/DATA/WORK/Framework-2-x")
             ;; (add-to-list 'ac-sources 'ac-source-semantic)
             (set-compilation-regexp-alist-for-msbuild)
             ;; trigger other common coding hooks
             ))


(setup-cmake-mode)

(setq  compile-command "cmake ../.. && msbuild /p:configuration=release /m:8 Framework.sln && ctest -C release --output-on-failure")
(setq  fill-column 110)
(setq grep-command "grep -nH --exclude-dir=\\.svn --exclude-dir=build -r ")

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(compile-command "cmake ../.. && msbuild /p:configuration=release /m:8 Framework.sln && ctest -C release --output-on-failure")
;;  '(fill-column 110)
;;  '(global-hl-line-mode t)
;;  '(grep-command "grep -nH --exclude-dir=\\.svn --exclude-dir=build -r ")
;;  '(semantic-idle-scheduler-idle-time 3))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(rst-level-1-face ((t (:background "grey85" :foreground "blue"))) t))

;; Always at the end !
(require 'tibonihoo_common)





