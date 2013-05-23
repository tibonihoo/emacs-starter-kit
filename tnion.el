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

;; read the log of DOP
(autoload 'log4j-mode "log4j-mode" "Major mode for viewing log files." t)
(add-to-list 'auto-mode-alist '("DxOOpticsPro\\.[0-9]*\\.txt\\'" . log4j-mode))
(add-hook
 'log4j-mode-hook
 (lambda ()
   (define-key log4j-mode-local-map [(control down)] 'log4j-forward-record)
   (define-key log4j-mode-local-map [(control up)] 'log4j-backward-record)
   (define-key log4j-mode-local-map [(control c) (control f)] 'font-lock-fontify-buffer)
   ))

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
             (add-to-list 'ac-sources 'ac-source-gtags)
             (set-compilation-regexp-alist-for-msbuild)
             ;; trigger other common coding hooks
             ))


(setup-cmake-mode)

(setq  compile-command "cmake -G \"Visual Studio 10 Win64\" -DUTTestImagesPathNetwork=D:/DATA/UTData ../.. && msbuild /p:configuration=release /m:8 Framework.sln && ctest -C release --output-on-failure")
(setq  fill-column 110)
(setq grep-command "grep -nH --exclude-dir=\\.svn --exclude-dir=build --exclude-dir=Externals -r ")



;; -----------------------------------------------------------------------------
;; Semantic config
;; -----------------------------------------------------------------------------
;; Partly from https://github.com/alexott/emacs-configs/blob/master/rc/emacs-rc-cedet.el

(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
;(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
;(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;; Activate semantic
(semantic-mode 1)
(add-to-list 'ac-sources 'ac-source-semantic)
;; ;; use GLOBAL tag db
;; (semanticdb-enable-gnu-global-databases 'c-mode t)
(semanticdb-enable-gnu-global-databases 'c++-mode)


;; EDE
(require 'ede)
(global-ede-mode 1)
(ede-enable-generic-projects)

(defvar ede-project-build-current-build-platform "win64")
(defvar ede-project-build-current-build-mode "release")
(defvar ede-project-build-current-test-options "")

(defun generate-framework-cmake-build-command ()  
  "Generates compile string for compiling CMake project"
  (let* (
         (current-dir (file-name-directory
                       (or (buffer-file-name (current-buffer)) default-directory)))
         (prj (ede-current-project current-dir))
         (root-dir (ede-project-root-directory prj))
         (cmake-generator
          (cond
           ( (string= ede-project-build-current-build-platform "win64")
             "-G \"Visual Studio 10 Win64\""
             )
           ( t
             "")
           ))
         (build-dir-name
          (cond
           ( (string= ede-project-build-current-build-platform "win64")
             "visualx64"
             )
           ( t
             "visualx86")
           ))
         )
    (concat "cd " root-dir "/build/ && mkdir " build-dir-name " || cd " build-dir-name " && cmake ../.. " cmake-generator " -DUTTestImagesPathNetwork=D:/DATA/UTData && msbuild /p:configuration=" ede-project-build-current-build-mode " /m:8 Framework.sln")
    )
  )

(defun generate-framework-cmake-build-and-test-command ()
  "Generates compile string for compiling CMake project and running unit-test on it"
  (concat (generate-framework-cmake-build-command) " && ctest -C " ede-project-build-current-build-mode " --output-on-failure " ede-project-build-current-test-options)
  )

(ede-cpp-root-project "Framework-trunk"
                      :name "Framework-trunk"
                :file "d:/DATA/WORK/Framework-trunk/CMakeLists.txt"
                :include-path '("/include"
                                "/src/Correction/include"
                                "/src/Foundation/include"
                                )
                :local-variables (list
                                  (cons 'compile-command 'generate-framework-cmake-build-command)
                                  (cons 'compile-and-test-command 'generate-framework-cmake-build-and-test-command))
                :system-include-path '("c:/Program Files (x86)/Microsoft Visual Studio 10.0/VC/include")
                :spp-table '(("WIN32" . "")
                             ("BOOST_TEST_DYN_LINK" . "")))



(ede-cpp-root-project "Framework-3.5.x"
                      :name "Framework-3.5.x"
                      :file "d:/DATA/WORK/Framework-3.5.x/CMakeLists.txt"
                      :include-path '("/include"
                                      "/src/Correction/include"
                                      "/src/Foundation/include"
                                      )
                      :local-variables (list
                                        (cons 'compile-command 'generate-framework-cmake-build-command)
                                        (cons 'compile-and-test-command 'generate-framework-cmake-build-and-test-command))
                      :system-include-path '("c:/Program Files (x86)/Microsoft Visual Studio 10.0/VC/include")
                      :spp-table '(("WIN32" . "")
                                   ("BOOST_TEST_DYN_LINK" . "")))


(ede-cpp-root-project "Framework-3.x"
                      :name "Framework-3.x"
                      :file "d:/DATA/WORK/Framework-3.x/CMakeLists.txt"
                      :include-path '("/include"
                                      "/src/Correction/include"
                                      "/src/Foundation/include"
                                      )
                      :local-variables (list
                                        (cons 'compile-command 'generate-framework-cmake-build-command)
                                        (cons 'compile-and-test-command 'generate-framework-cmake-build-and-test-command))
                      :system-include-path '("c:/Program Files (x86)/Microsoft Visual Studio 10.0/VC/include")
                      :spp-table '(("WIN32" . "")
                                   ("BOOST_TEST_DYN_LINK" . "")))


;; from https://github.com/alexott/emacs-configs/blob/master/rc/emacs-rc-cedet.el
(defun ede-get-local-var (fname var)
  "fetch given variable var from :local-variables of project of file fname"
  (let* ((current-dir (file-name-directory fname))
         (prj (ede-current-project current-dir)))
    (when prj
      (let* ((ov (oref prj local-variables))
            (lst (assoc var ov)))
        (when lst
          (cdr lst))))))

(defun build-ede-project (&optional build-target-platform build-target-mode)
  "Saves all unsaved buffers, and compile the current project."
  (interactive)
  (save-some-buffers t)
  (when (not build-target-platform)
    (setq build-target-platform
          (read-string "platform: " ede-project-build-current-build-platform))
    )
  (when (not build-target-mode)
    (setq build-target-mode
          (read-string "mode: " ede-project-build-current-build-mode))
    )
  (setq ede-project-build-current-build-platform build-target-platform)
  (setq ede-project-build-current-build-mode build-target-mode)
  (let* ((r (ede-get-local-var
             (or (buffer-file-name (current-buffer)) default-directory)
             'compile-command))
         (cmd (if (functionp r) (funcall r) r)))
    (set (make-local-variable 'compile-command) (or cmd compile-command))
    (compile compile-command))
  )

(defun test-ede-project (&optional build-target-platform build-target-mode build-target-test-options)
  "Saves all unsaved buffers, and compile the current project."
  (interactive)
  (save-some-buffers t)
  (when (not build-target-platform)
    (setq build-target-platform
          (read-string "platform: " ede-project-build-current-build-platform))
    )
  (when (not build-target-mode)
    (setq build-target-mode
          (read-string "mode: " ede-project-build-current-build-mode))
    )
  (when (not build-target-test-options)
    (setq build-target-test-options
          (read-string "test-options: " ede-project-build-current-test-options))
    )
  (setq ede-project-build-current-build-platform build-target-platform)
  (setq ede-project-build-current-build-mode build-target-mode)
  (setq ede-project-build-current-test-options build-target-test-options)
  (let* ((r (ede-get-local-var
             (or (buffer-file-name (current-buffer)) default-directory)
             'compile-and-test-command))
         (cmd (if (functionp r) (funcall r) r)))
    (set (make-local-variable 'compile-command) (or cmd compile-command))
    (compile compile-command))
  )




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





