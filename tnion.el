;;
;; Additional customisation on top of the starter-kit's defaults.
;;
;; (c) 2010-2013 Thibauld Nion
;;

;; Personal info for signatures in code and stuff...
(set 'user-full-name "Thibauld Nion")
(set 'user-mail-address "tnion@dxo.com")
;; Work context
(defvar my-work-context "DO")
(setq initial-major-mode 'org-mode)

(if (eq system-type 'windows-nt)
    (setq backup-directory-alist `(("." . ,(expand-file-name "d:/Perso/emacs/backups"))))
  )

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PYTHONPATH")
  )


(cond
 ((eq system-type 'windows-nt)
  (progn
    ;; Set prefered frame size
    (add-to-list 'default-frame-alist '(height . 59))
    (add-to-list 'default-frame-alist '(width . 100))
    (add-to-list 'default-frame-alist '(top + -3))
    (add-to-list 'default-frame-alist '(left + -1))
    ))
 ((eq system-type 'darwin)
  (progn
       ;; for my linux workstation
       (frame-set-fullscreen-off)
       (add-to-list 'default-frame-alist '(height . 80))
       (add-to-list 'default-frame-alist '(width . 317))
       ))
 ((progn
       ;; for my linux workstation
       (frame-set-fullscreen-off)
       (add-to-list 'default-frame-alist '(height . 66))
       (add-to-list 'default-frame-alist '(width . 281))
       )
  ))

(cond
  ( (eq system-type 'windows-nt)
    (set-face-attribute 'default nil :family "Consolas" :height 105) )
  ( (eq system-type 'darwin)
    (set-face-attribute 'default nil :family "Menlo" :height 130) )
  ( (set-face-attribute 'default nil :family "b&h-lucidatypewriter") )
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
             ;; ;; gtags for completion
             ;; (add-to-list 'ac-sources 'ac-source-gtags)
             (delete 'ac-source-gtags ac-sources)
             (when (eq system-type 'windows-nt)
               (set-compilation-regexp-alist-for-msbuild)
               )
             ;; trigger other common coding hooks
             ))


(setup-cmake-mode)

(setq  fill-column 110)
(setq grep-command "grep -nH --exclude-dir=Externals -Ir ")


;; -----------------------------------------------------------------------------
;; Semantic config
;; -----------------------------------------------------------------------------
;; Partly from https://github.com/alexott/emacs-configs/blob/master/rc/emacs-rc-cedet.el

(when (not (eq system-type 'windows-nt))
  (require 'semantic/bovine/gcc)
  )

(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
;(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;; Activate semantic
(semantic-mode 1)
(add-to-list 'ac-sources 'ac-source-semantic)
;; use GLOBAL tag db
(semanticdb-enable-gnu-global-databases 'c-mode t)
(semanticdb-enable-gnu-global-databases 'c++-mode)

;; EDE
(require 'ede)
(global-ede-mode 1)
(ede-enable-generic-projects)

(cond
 ( (eq system-type 'windows-nt)
   (progn
     (setq  compile-command "cmake -G \"Visual Studio 14 Win64\" ../.. && cmake --build . --target FWKUnitTests")
     (setq root-of-build-directories "P:/Builds")     
     (setq local-projects-default-path "d:/DATA/WORK/tnion/Development/")
     (setq local-projects-platform-define "WIN32")
     (defvar ede-project-build-current-build-platform "win64")
     (defvar ede-project-build-current-build-mode "release")
     (defvar ede-project-build-current-test-options "")
     ))
 ( (eq system-type 'darwin)
   (progn
     (setq  compile-command "cmake -G \"Xcode\" ../.. && cmake --build . --target FWKUnitTests")
     (setq root-of-build-directories "/Volumes/DATA/Builds")     
     (setq local-projects-default-path "/Users/tnion/Development/")
     (setq local-projects-platform-define "__MACH__")
     (defvar ede-project-build-current-build-platform "xcode")
     (defvar ede-project-build-current-build-mode "Release")
     (defvar ede-project-build-current-test-options "")
     ))
 )

(defun generate-framework-cmake-build-command ()  
  "Generates compile string for compiling CMake project"
  (let* (
         (current-dir (file-name-directory
                       (or (buffer-file-name (current-buffer)) default-directory)))
         (prj (ede-current-project current-dir))
         (root-dir (ede-project-root-directory prj))
         (project-name (file-name-base (directory-file-name root-dir)))
         (cmake-generator
          (cond
           ( (string= ede-project-build-current-build-platform "win64")
             "-G \"Visual Studio 11 Win64\""
             )
           ( (string= ede-project-build-current-build-platform "win32")
             "-G \"Visual Studio 11\""
             )
           ( (string= ede-project-build-current-build-platform "xcode")
             "-G \"Xcode\""
             )
           ( t ;; use the default
             "")
           ))
         (build-dir-name
          (cond
           ( (string= ede-project-build-current-build-platform "win64")
             "visualx64"
             )
           ( (string= ede-project-build-current-build-platform "win32")
             "visualx86"
             ) 
           ( (string= ede-project-build-current-build-platform "xcode")
             "xcode"
             )
           ( t
             "make")
           ))
         (build-cmd
          (cond
           ( (eq system-type 'windows-nt)
             (concat "msbuild /p:configuration=" ede-project-build-current-build-mode " /m:8 Framework.sln")
             )
           ( (eq system-type 'darwin)
             (concat "xcodebuild -jobs 8 -project Framework.xcodeproj -configuration " ede-project-build-current-build-mode)
             )
           ( t
             "")
           ))
         )
    (concat "cd " root-of-build-directories " && cd " project-name " && mkdir " build-dir-name " || cd " build-dir-name " && cmake " root-dir " " cmake-generator " && " build-cmd)
    )
  )

(defun generate-framework-cmake-build-and-test-command ()
  "Generates compile string for compiling CMake project and running unit-test on it"
  (let* (
         (current-dir (file-name-directory
                       (or (buffer-file-name (current-buffer)) default-directory)))
         (prj (ede-current-project current-dir))
         (root-dir (ede-project-root-directory prj))
         (project-name (file-name-base (directory-file-name root-dir)))
         )
    (concat (generate-framework-cmake-build-command) " && ctest -C " ede-project-build-current-build-mode " --output-on-failure " ede-project-build-current-test-options)
    )
  )


(setq ede-projects-names-lists (concat dotfiles-dir "ede-projects-names.el"))
(if (file-exists-p ede-projects-names-lists)
    (load ede-projects-names-lists)
  (setq ede-cpp-projects-names (list))
  )

(loop for name in ede-cpp-projects-names do
      (ede-cpp-root-project name
                            :file (concat local-projects-default-path name "/CMakeLists.txt")
                            :include-path '("/include"
                                            "/src"
                                            "/src/Correction/include"
                                            "/src/Foundation/include"
                                            "/src/Correction/src"
                                            "/src/Foundation/src"
                                            "/Externals/boost_src"
                                            "/Externals/gtest_src/include"
                                            "/Externals/lcms_src/include"
                                            "/Externals/libdng_src/include" 
                                            "/Externals/libjpeg_src/include"
                                            "/Externals/liblua_src/include" 
                                            "/Externals/libpng_src/include" 
                                            "/Externals/libtiff_src/include"
                                            "/Externals/libxmp_src/include" 
                                            "/Externals/libzlib_src/include"
                                            "/Externals/openCL_src/include" 
                                            "/Externals/qt_lib/include"     
                                            "/Externals/sqlite3_src/include"
                                            "/Externals/tclap_src/include"
                                            "/UnitTests"
                                            )
                            :local-variables (list
                                              (cons 'compile-command 'generate-framework-cmake-build-command)
                                              (cons 'compile-and-test-command 'generate-framework-cmake-build-and-test-command)
                                              )
                            :spp-table `( (,local-projects-platform-define "")
                                          ("BOOST_TEST_DYN_LINK" . "")
                                          ("REGISTER_DEFAULT_CORRECTIONS" . "")
                                          )
                            )
      )


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

;; A list of const containing (file-name root-dir)
(setq gtags-file-targets '())
(setq gtags-running-process nil)


(defun gtags-process-sentinel (process event)
  "Sentinel called when a gtags process changes status, and in
charge of poping the next item of gtags targets."
  (if (or
         (string-equal event "finished\n")    
         (string-equal event "killed\n")
         )
      ( (setq gtags-running-process nil)
        (gtags-run-against-next-target) )
    (message (format "Unexpected even for gtags process: '%s'" event))
    )
  )

(defun gtags-run-against-next-target ()
  "Run gtags on the next file listed in gtags-file-targets"
  (when gtags-file-targets
    (let*
        (
         (current-target (car gtags-file-targets))
         (gtags-args (if (car current-target) (concat "--single-update " (car current-target)) ""))
         (current-working-directory (car (cdr current-target)))
         )
      (setq gtags-file-targets (cdr gtags-file-targets))
      (setq gtags-running-process (start-process-shell-command "GTAGS update" nil (concat "cd " current-working-directory " && gtags " gtags-args)))
      (set-process-sentinel gtags-running-process 'gtags-process-sentinel)
      )
    )
  )

(defun update-gtags-for-ede-project ()
  "Update the gtag file of EDE project."
  (interactive)
  (let* (
         (current-dir (file-name-directory
                       (or (buffer-file-name (current-buffer)) default-directory)))
         (prj (ede-current-project current-dir))
         (current-project-root-dir (ede-project-root-directory prj))
         (current-file-name (buffer-file-name))
         )
    (when (not gtags-file-targets)
      (setq gtags-file-targets (list))
      )
    (add-to-list 'gtags-file-targets (list current-file-name current-project-root-dir))
    (when (not gtags-running-process)
      (gtags-run-against-next-target)
      )
    )
  )

(add-hook 'c-mode-common-hook
          (lambda () 
            (add-hook 'after-save-hook 'update-gtags-for-ede-project nil 'make-it-local))
          )

(setq flymake-gui-warnings-enabled nil)


(require 'god-mode)
(god-mode-all)
(global-set-key (kbd "<f6>") 'god-mode-all)
(defun god-mode-update-mode-line ()
  (let ((limited-colors-p (> 257 (length (defined-colors)))))
    (cond (god-local-mode (progn
                            (set-face-background 'mode-line (if limited-colors-p "purple" "#8b2500"))
                            (set-face-background 'mode-line-inactive (if limited-colors-p "purple" "#8b2500"))))
          (t (progn
               (set-face-background 'mode-line (if limited-colors-p "black" "black"))
               (set-face-background 'mode-line-inactive (if limited-colors-p "black" "black")))))))
(add-hook 'god-mode-enabled-hook 'god-mode-update-mode-line)
(add-hook 'god-mode-disabled-hook 'god-mode-update-mode-line)


(require 'iedit)
(global-set-key (kbd "C-*") 'iedit-mode)

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





