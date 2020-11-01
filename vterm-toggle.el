;;; vterm-toggle.el --- Toggles between the vterm buffer and other buffers.  -*- lexical-binding: t; -*-

;; Author: jixiuf  jixiuf@qq.com
;; Keywords: vterm terminals
;; Version: 0.0.3
;; URL: https://github.com/jixiuf/vterm-toggle
;; Package-Requires: ((emacs "25.1") (vterm "0.0.1"))

;; Copyright (C) 2019, jixiuf, all rights reserved.

;; This file is NOT part of GNU Emacs.
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:
;;
;; Provides the command vterm-toggle which toggles between the
;; vterm buffer and whatever buffer you are editing.
;;
;; This is done in an "intelligent" way.  Features are:
;; o Starts a vterm if none is existent.
;; o Minimum distortion of your window configuration.
;; o When done in the vterm-buffer you are returned to the same window
;;   configuration you had before you toggled to the shell.
;; o If you desire, you automagically get a "cd" command in the shell to the
;;   directory where your current buffers file exists( even in a ssh session); just call
;;   vterm-toggle-cd instead of vterm-toggle.
;;


;;; Code:

(require 'cl-lib)
(require 'tramp)
(require 'tramp-sh)
(require 'vterm)

(defcustom vterm-toggle-show-hook nil
  "Hooks when swith to vterm buffer."
  :group 'vterm-toggle
  :type 'symbolp)
(defcustom vterm-toggle-hide-hook nil
  "Hooks when hide vterm buffer."
  :group 'vterm-toggle
  :type 'symbolp)

(defcustom vterm-toggle-fullscreen-p nil
  "Open vterm buffer fullscreen or not."
  :group 'vterm-toggle
  :type 'boolean)

(defcustom vterm-toggle-scope nil
  "`projectile' limit the scope only in the current project.
`frame' limit the scope only not in other frame.
`dedicated' use the dedicated vterm buffer."
  :group 'vterm-toggle
  :type '(radio
          (const :tag "all" nil)
          (const :tag "projectile" projectile)
          (const :tag "frame" frame)
          (const :tag "dedicated" dedicated)))

(defcustom vterm-toggle-projectile-root t
  "Create a new vterm buffter at projectile root directory or not.
it only work  when `vterm-toggle-scope' is `projectile'. "
  :group 'vterm-toggle
  :type 'boolean)

(defcustom vterm-toggle-cd-auto-create-buffer nil
  "If the prompt of recent vterm buffer is not available,
`vterm-toggle-cd' would create a new vterm buffer."
  :group 'vterm-toggle
  :type 'boolean)

(defcustom vterm-toggle-reset-window-configration-after-exit nil
  "Whether reset window configuration after vterm buffer is killed."
  :group 'vterm-toggle
  :type '(choice
          (const :tag "Do nothing" nil)
          (const :tag "Reset window configration after exit" t)
          (const :tag "Kill Window only" kill-window-only)))


(defcustom vterm-toggle-after-remote-login-function nil
  "Those functions are called one by one after open a ssh session.
`vterm-toggle-after-remote-login-function' should be a symbol, a hook variable.
The value of HOOK may be nil, a function, or a list of functions.
for example
   (defun vterm-toggle-after-ssh-login (method user host port localdir)
    (when (equal host \"my-host\")
        (vterm-send-string \"zsh\" t)
        (vterm-send-return)))"
  :group 'vterm-toggle
  :type 'hook)

(defvar vterm-toggle--window-configration nil)
(defvar vterm-toggle--vterm-dedicated-buffer nil)
(defvar-local vterm-toggle--dedicated-p nil)
(defvar vterm-toggle--buffer-list nil
  "The list of non-dedicated terminal buffers managed by `vterm-toggle'.")
(defvar-local vterm-toggle--cd-cmd nil)

;;;###autoload
(defun vterm-toggle(&optional args)
  "Vterm toggle.
Optional argument ARGS ."
  (interactive "P")
  (cond
   ((derived-mode-p 'vterm-mode)
    (if (equal (prefix-numeric-value args) 1)
        (vterm-toggle-hide)
      (vterm)))
   ((equal (prefix-numeric-value args) 1)
    (vterm-toggle-show))
   ((equal (prefix-numeric-value args) 4)
    (let ((vterm-toggle-fullscreen-p
           (not vterm-toggle-fullscreen-p)))
      (vterm-toggle-show)))))

;;;###autoload
(defun vterm-toggle-cd(&optional args)
  "Vterm toggle and insert a cd command.
Optional argument ARGS ."
  (interactive "P")
  (cond
   ((derived-mode-p 'vterm-mode)
    (if (equal (prefix-numeric-value args) 1)
        (vterm-toggle-hide)
      (vterm-toggle-show t)))
   ((equal (prefix-numeric-value args) 1)
    (vterm-toggle-show t))
   ((equal (prefix-numeric-value args) 4)
    (let ((vterm-toggle-fullscreen-p
           (not vterm-toggle-fullscreen-p)))
      (vterm-toggle-show t)))))

(defun vterm-toggle-hide (&optional _args)
  "Hide the vterm buffer."
  (interactive "P")
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'vterm-mode)
        (run-hooks 'vterm-toggle-hide-hook)
        (bury-buffer))))
  (when vterm-toggle--window-configration
    (set-window-configuration vterm-toggle--window-configration))
  (when (derived-mode-p 'vterm-mode)
    (bury-buffer)
    (switch-to-buffer (vterm-toggle--recent-other-buffer))))

(defun vterm-toggle-tramp-get-method-parameter (method param)
  "Return the method parameter PARAM.
If the `tramp-methods' entry does not exist, return NIL."
  (let ((entry (assoc param (assoc method tramp-methods))))
    (when entry (cadr entry))))

(when (version<= emacs-version "26.3")
  (with-eval-after-load 'tramp-sh
    (defun tramp-get-sh-extra-args (shell)
      "Find extra args for SHELL."
      (let ((alist tramp-sh-extra-args)
	        item extra-args)
        (while (and alist (null extra-args))
          (setq item (pop alist))
          (when (string-match-p (car item) shell)
	        (setq extra-args (cdr item))))
        extra-args))))

(defun vterm-toggle-cd-show(&optional  args)
  "Switch to an idle vterm buffer and insert a cd command.
Or create 1 new vterm buffer.
Optional argument ARGS optional args.
Usually I would bind it in `vterm-mode-map'
(define-key vterm-mode-map (kbd \"s-t\")   #'vmacs-vterm-toggle-show)"
  (interactive "P")
  (vterm-toggle-show (not args)))

(defun vterm-toggle-show(&optional make-cd)
  "Show the vterm buffer.
Optional argument MAKE-CD whether insert a cd command."
  (interactive "P")
  (let* ((shell-buffer (vterm-toggle--get-buffer
                        make-cd (not vterm-toggle-cd-auto-create-buffer)))
         (dir (expand-file-name default-directory))
         cd-cmd cur-host vterm-dir vterm-host cur-user cur-port remote-p cur-method login-cmd)
    (if (ignore-errors (file-remote-p dir))
        (with-parsed-tramp-file-name dir nil
          (setq remote-p t)
          (setq cur-host host)
          (setq cur-method (tramp-find-method method user cur-host))
          (setq cur-user (or (tramp-find-user cur-method user cur-host) ""))
          (setq cur-port (or port ""))
          (setq dir localname))
      (setq cur-host (system-name)))
    (setq login-cmd (vterm-toggle-tramp-get-method-parameter cur-method 'tramp-login-program))
    (setq cd-cmd (concat " cd " (shell-quote-argument dir)))
    (if shell-buffer
        (progn
          (when (and (not (derived-mode-p 'vterm-mode))
                     (not (get-buffer-window shell-buffer)))
            (setq vterm-toggle--window-configration (current-window-configuration)))
          (if vterm-toggle-fullscreen-p
              (progn
                (delete-other-windows)
                (switch-to-buffer shell-buffer))
            (pop-to-buffer shell-buffer))
          (with-current-buffer shell-buffer
            (when (derived-mode-p 'vterm-mode)
              (setq vterm-toggle--cd-cmd cd-cmd)
              (if (ignore-errors (file-remote-p default-directory))
                  (with-parsed-tramp-file-name default-directory nil
                    (setq vterm-dir localname)
                    (setq vterm-host host))
                (setq vterm-dir default-directory)
                (setq vterm-host (system-name)))
              (when (and (not (equal vterm-dir dir))
                         (equal vterm-host cur-host) make-cd)
                (vterm-send-C-a)
                (vterm-send-C-k)
                (sleep-for 0.01)
                (if (vterm--at-prompt-p)
                    (vterm-toggle-insert-cd)
                  (message "You can insert '%s' by M-x:vterm-toggle-insert-cd."
                           vterm-toggle--cd-cmd))))
            (run-hooks 'vterm-toggle-show-hook)))
      (setq vterm-toggle--window-configration (current-window-configuration))
      (with-current-buffer (setq shell-buffer (vterm-toggle--new))
        (vterm-toggle--wait-prompt)
        (when remote-p
          (let* ((method  (if (string-equal login-cmd "ssh") "ssh" cur-method))
                 (login-opts (vterm-toggle-tramp-get-method-parameter method 'tramp-login-args))
                 (login-shell (vterm-toggle-tramp-get-method-parameter method 'tramp-remote-shell))
                 (login-shell-args (tramp-get-sh-extra-args login-shell))
                 ;; (vterm-toggle-tramp-get-method-parameter cur-method 'tramp-remote-shell)
                 (spec (format-spec-make
			            ?h cur-host ?u cur-user ?p cur-port ?c ""
			            ?l (concat login-shell " " login-shell-args)))
                 (cmd
                  (concat login-cmd " "
                          (mapconcat
		                   (lambda (x)
			                 (setq x (mapcar (lambda (y) (format-spec y spec)) x))
			                 (unless (member "" x) (string-join x " ")))
		                   login-opts " "))))
            (vterm-send-string cmd)
            (vterm-send-return)
            (run-hook-with-args 'vterm-toggle-after-remote-login-function
                                method cur-user cur-host cur-port dir))
          (vterm-send-string cd-cmd)
          (vterm-send-return)
          (setq default-directory
	            (file-name-as-directory
	             (if (and (string= cur-host (system-name))
                          (string= cur-user (user-real-login-name)))
		             (expand-file-name dir)
                   (concat "/" cur-method ":" (if (string-empty-p cur-user) ""
                                                (concat cur-user "@") )
                           cur-host ":" dir)))))
        (when vterm-toggle-fullscreen-p
          (delete-other-windows))
        (run-hooks 'vterm-toggle-show-hook)))
    shell-buffer))

(defun vterm-toggle--wait-prompt()
  "Wait prompt."
  (let ((wait-ms 0))
    (cl-loop until (or (> (length (string-trim
                                   (buffer-substring-no-properties
                                    (point-min) (point-max)))) 0)
                       (> wait-ms 3000)) do
                       (sleep-for 0.01)
                       (setq wait-ms (+ wait-ms 10)))))

;;;###autoload
(defun vterm-toggle-insert-cd()
  "Cd to the directory where your previous buffer file exists.
after you have toggle to the vterm buffer with `vterm-toggle'."
  (interactive)
  (when vterm-toggle--cd-cmd
    (vterm-send-string vterm-toggle--cd-cmd t)
    (vterm-send-return)))

(defun vterm-toggle--new(&optional buffer-name)
  "New vterm buffer."
  (let ((default-directory default-directory)
        project-root)
    (when (and vterm-toggle-projectile-root
               (fboundp 'projectile-project-root)
               (eq vterm-toggle-scope 'projectile))
      (setq project-root (projectile-project-root) )
      (when project-root (setq default-directory project-root)))
    (if vterm-toggle-fullscreen-p
        (vterm buffer-name)
      (vterm-other-window buffer-name))))


(defun vterm-toggle--get-buffer(&optional make-cd ignore-prompt-p)
  "Get vterm buffer.
Optional argument MAKE-CD make cd or not.
Optional argument ARGS optional args."
  (cond
   ((eq vterm-toggle-scope 'dedicated)
    (vterm-toggle--get-dedicated-buffer))
   ((and (fboundp 'projectile-project-root)
         (eq vterm-toggle-scope 'projectile))
    (let* ((project-root (projectile-project-root))
           (buf (vterm-toggle--recent-vterm-buffer
                 make-cd ignore-prompt-p project-root)))
      buf))
   (t
    (vterm-toggle--recent-vterm-buffer make-cd ignore-prompt-p))))

(defun vterm-toggle--get-dedicated-buffer()
  "Get dedicated buffer."
  (if (buffer-live-p vterm-toggle--vterm-dedicated-buffer)
      vterm-toggle--vterm-dedicated-buffer
    (setq vterm-toggle--vterm-dedicated-buffer (vterm-toggle--new))
    (with-current-buffer vterm-toggle--vterm-dedicated-buffer
      (vterm-toggle--wait-prompt)
      (setq vterm-toggle--dedicated-p t)
      vterm-toggle--vterm-dedicated-buffer)))


(defun vterm-toggle--not-in-other-frame(frame buf)
  (let ((win (get-buffer-window buf t)))
    (if win
        (eq frame (window-frame win))
      t)))

(defun vterm-toggle--recent-vterm-buffer(&optional make-cd ignore-prompt-p dir)
  "Get recent vterm buffer.
Optional argument MAKE-CD make cd or not.
Optional argument ARGS optional args."
  (let ((shell-buffer)
        (curbuf (current-buffer))
        (curframe (window-frame))
        buffer-host
        vterm-host)
    (if (ignore-errors (file-remote-p default-directory))
        (with-parsed-tramp-file-name default-directory nil
          (setq buffer-host host))
      (setq buffer-host (system-name)))
    (cl-loop for buf in (buffer-list) do
             (with-current-buffer buf
               (when (and (derived-mode-p 'vterm-mode)
                          (not (eq curbuf buf))
                          (not vterm-toggle--dedicated-p)
                          (or (not vterm-toggle-scope)
                              (and (eq vterm-toggle-scope 'frame)
                                   (vterm-toggle--not-in-other-frame curframe buf))
                              (and (eq vterm-toggle-scope 'projectile)
                                   (fboundp 'projectile-project-root)
                                   (equal (projectile-project-root) dir))))
                 (cond
                  ((and  make-cd (derived-mode-p 'vterm-mode))
                   (if (ignore-errors (file-remote-p default-directory))
                       (with-parsed-tramp-file-name default-directory nil
                         (setq vterm-host host))
                     (setq vterm-host (system-name)))
                   (when (and (or ignore-prompt-p
                                  (vterm--at-prompt-p))
                              (equal buffer-host vterm-host))
                     (setq shell-buffer buf)))
                  (t (setq shell-buffer buf)))))
             until shell-buffer)
    shell-buffer))

(defun vterm-toggle--recent-other-buffer(&optional _args)
  "Get last viewed buffer.
Optional argument ARGS optional args."
  (let (shell-buffer)
    (cl-loop for buf in (buffer-list) do
             (with-current-buffer buf
               (when (and (not (derived-mode-p 'vterm-mode))
                          (not (char-equal ?\  (aref (buffer-name) 0))))
                 (setq shell-buffer buf)))
             until shell-buffer)
    shell-buffer))

(defun vterm-toggle--exit-hook()
  "Vterm exit hook."
  (when (derived-mode-p 'vterm-mode)
    (setq vterm-toggle--buffer-list
          (delq (current-buffer) vterm-toggle--buffer-list))
    (if (eq vterm-toggle-reset-window-configration-after-exit 'kill-window-only)
        (quit-window)
      (when (and vterm-toggle-reset-window-configration-after-exit
                 vterm-toggle--window-configration)
        (set-window-configuration vterm-toggle--window-configration)))))

(add-hook 'kill-buffer-hook #'vterm-toggle--exit-hook)
;; (add-hook 'vterm-exit-functions #'vterm-toggle--exit-hook)

(defun vterm-toggle--mode-hook()
  "Hook for `vterm-mode-hook'."
  (add-to-list 'vterm-toggle--buffer-list (current-buffer)))
(add-hook 'vterm-mode-hook #'vterm-toggle--mode-hook)

(defun vterm-toggle--switch (direction offset)
  "Internal `vterm-toggle' buffers switch function.
If DIRECTION is `forward', switch to the next term.
If DIRECTION `backward', switch to the previous term.
Option OFFSET for skip OFFSET number term buffer."
  (if vterm-toggle--buffer-list
      (let ((buffer-list-len (length vterm-toggle--buffer-list))
	        (index (cl-position (current-buffer) vterm-toggle--buffer-list)))
	    (if index
	        (let ((target-index (if (eq direction 'forward)
				                    (mod (+ index offset) buffer-list-len)
				                  (mod (- index offset) buffer-list-len))))
	          (switch-to-buffer (nth target-index vterm-toggle--buffer-list)))
	      (switch-to-buffer (car vterm-toggle--buffer-list))))
    nil))

;;;###autoload
(defun vterm-toggle-forward (&optional offset)
  "Go to the next term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET."
  (interactive "P")
  (vterm-toggle--switch 'forward (or offset 1)))

;;;###autoload
(defun vterm-toggle-backward (&optional offset)
  "Go to the previous term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET."
  (interactive "P")
  (vterm-toggle--switch 'backward (or offset 1)))

(provide 'vterm-toggle)

;; Local Variables:
;; coding: utf-8
;; End:

;;; vterm-toggle.el ends here
