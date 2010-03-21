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


;; Standard bindings for C-z, C-x,C-v,C-c (at least when a region is selected)
(cua-mode t)
;; Binding for full screen
(global-set-key (kbd "<f11>") 'toggle-fullscreen)

;; Tab navigation among buffers
(require 'ibs)

;; Custom color theme
(color-theme-tango)


;; Specific python config
(eval-after-load 'python
  '(progn
     (setq python-indent 2)
     (yas/define-snippets 'python-mode
                          '(
                            ("tnc" "# TN: NOCOMMIT" "Mark as uncommitable" nil)
                            ("encoding" "# -*- coding: cp1252 -*-
" "Set the file encoding" nil)
                            ("shebang" "#!/usr/bin/env python
" "Insert the default shebang for python "nil)
                            )
                          )
     
     ))

(eval-after-load 'auto-complete
  '(progn
     ;; customize the colors of the autocomplete in accordance with my
     ;; preferred them.
     (set-face-background 'ac-menu-face "lightgray")
     (set-face-underline-p 'ac-menu-face "darkgray")
     (set-face-background 'ac-selection-face "steelblue")
     ))


;; *****************************************************************************
;; ********* WORKAROUNDS to starter kit ****************************************
;; *****************************************************************************
(add-hook 'paredit-mode-hook
          (lambda ()
            ;; paredit's bindings C-<right/left> are just
            ;; killing me
            (define-key paredit-mode-map (kbd "C-<right>") 'forward-word)
            (define-key paredit-mode-map (kbd "C-<left>") 'backward-word)
            ))


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
