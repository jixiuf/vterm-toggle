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

(require 'tramp)
(require 'vterm)

(defcustom vterm-toggle-show-hook nil
  "Hooks when swith to vterm buffer."
  :group 'vterm-toggle
  :type 'symbolp)
(defcustom vterm-toggle-hide-hook nil
  "Hooks when hide vterm buffer."
  :group 'vterm-toggle
  :type 'symbolp)

(defcustom vterm-toggle-prompt-regexp
  (concat "\\(?:^\\|\r\\)"
	      "[^]#$%>\n]*#?[#$%➜⇒»☞@λ] *\\(\e\\[[0-9;]*[a-zA-Z] *\\)*")
  "Vterm prompt regexp."
  :group 'vterm-toggle
  :type 'string)
(defcustom vterm-toggle-fullscreen-p t
  "Open vterm buffer fullscreen or not."
  :group 'vterm-toggle
  :type 'boolean)

(defcustom vterm-toggle-use-dedicated-buffer nil
  "Only toggle to or from dedicated vterm buffer."
  :group 'vterm-toggle
  :type 'boolean)
(defcustom vterm-toggle-reset-window-configration-after-exit nil
  "Whether reset window configuration after vterm buffer is killed."
  :group 'vterm-toggle
  :type 'boolean)

(defcustom vterm-toggle-after-ssh-login-function nil
  "Those functions are called one by one after open a ssh session.
`vterm-toggle-after-ssh-login-function' should be a symbol, a hook variable.
The value of HOOK may be nil, a function, or a list of functions.
for example
   (defun vterm-toggle-after-ssh-login (user host port localdir)
    (when (equal host \"my-host\")
        (vterm-send-string \"zsh\" t)
        (vterm-toggle-send-return)))"
  :group 'vterm-toggle
  :type 'hook)

(defvar vterm-toggle--window-configration nil)
(defvar vterm-toggle--vterm-dedicated-buffer nil)
(defvar vterm-toggle--vterm-buffer-p-function 'vterm-toggle--default-vterm-mode-p
  "Function to check whether a buffer is vterm-buffer mode.")
(defvar vterm-toggle--buffer-list nil
  "The list of non-dedicated terminal buffers managed by `vterm-toggle'.")

(defun vterm-toggle--default-vterm-mode-p(&optional _args)
  "Check buffer is term-mode-p.
Optional argument ARGS optional args."
  (derived-mode-p 'vterm-mode))


;;;###autoload
(defun vterm-toggle(&optional args)
  "Vterm toggle.
Optional argument ARGS ."
  (interactive "P")
  (cond
   ((funcall vterm-toggle--vterm-buffer-p-function args)
    (vterm-toggle-hide))
   (t
    (vterm-toggle-show nil args))))

;;;###autoload
(defun vterm-toggle-cd(&optional args)
  "Vterm toggle and insert a cd command.
Optional argument ARGS ."
  (interactive "P")
  (cond
   ((funcall vterm-toggle--vterm-buffer-p-function args)
    (vterm-toggle-hide args))
   (t
    (vterm-toggle-show t args))))

(defun vterm-toggle-send-return()
  (vterm-send-key "<return>"))

(defun vterm-toggle-hide(&optional args)
  "Hide the vterm buffer.
Optional argument ARGS ."
  (interactive "P")
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (funcall vterm-toggle--vterm-buffer-p-function args)
        (run-hooks 'vterm-toggle-hide-hook)
        (bury-buffer))))
  (when vterm-toggle--window-configration
    (set-window-configuration vterm-toggle--window-configration))
  (when (funcall vterm-toggle--vterm-buffer-p-function args)
    (switch-to-buffer (vterm-toggle--recent-other-buffer))))

(defun vterm-toggle-tramp-get-method-parameter (method param)
  "Return the method parameter PARAM.
If the `tramp-methods' entry does not exist, return NIL."
  (let ((entry (assoc param (assoc method tramp-methods))))
    (when entry (cadr entry))))

(defun vterm-toggle-show(&optional make-cd args)
  "Show the vterm buffer.
Optional argument MAKE-CD whether insert a cd command.
Optional argument ARGS optional args."
  (interactive)
  (let* ((shell-buffer (vterm-toggle--get-buffer make-cd args))
         (dir (and make-cd
                   (expand-file-name default-directory)))
         cd-cmd cur-host vterm-dir vterm-host cur-user cur-port remote-p)
    (when make-cd
      (if (ignore-errors (file-remote-p dir))
          (with-parsed-tramp-file-name dir nil
            (setq remote-p t)
            (setq cur-host host)
            (setq cur-user user)
            (setq cur-port (if port (concat ":" port) ""))
            (setq dir localname))
        (setq cur-host (system-name)))
      (setq cd-cmd (concat " cd " (shell-quote-argument dir))))
    (if shell-buffer
        (progn
          (when (and (not (funcall vterm-toggle--vterm-buffer-p-function args))
                     (not (get-buffer-window shell-buffer)))
            (setq vterm-toggle--window-configration (current-window-configuration)))
          (pop-to-buffer shell-buffer)
          (with-current-buffer shell-buffer
            (when (derived-mode-p 'vterm-mode)
              (if (ignore-errors (file-remote-p default-directory))
                  (with-parsed-tramp-file-name default-directory nil
                    (setq vterm-dir localname)
                    (setq vterm-host host))
                (setq vterm-dir default-directory)
                (setq vterm-host (system-name)))
              (when (and (not (equal vterm-dir dir))
                         (equal vterm-host cur-host))
                (vterm-send-key "u" nil nil t)
                (vterm-send-string cd-cmd t)
                (vterm-toggle-send-return)))
            (run-hooks 'vterm-toggle-show-hook))
          (when vterm-toggle-fullscreen-p
            (delete-other-windows)))
      (setq vterm-toggle--window-configration (current-window-configuration))
      (with-current-buffer (vterm-toggle--new)
        (when remote-p
          (let* ((method (tramp-find-method nil cur-user cur-host))
                 (login-cmd (vterm-toggle-tramp-get-method-parameter method 'tramp-login-program)))
            (if cur-user
                (vterm-send-string (format "%s %s@%s%s" login-cmd cur-user cur-host cur-port) t)
              (vterm-send-string (format "%s %s%s"  login-cmd cur-host cur-port) t)))
          (vterm-toggle-send-return)
          (run-hook-with-args 'vterm-toggle-after-ssh-login-function
                              cur-user cur-host cur-port dir)
          (vterm-send-string cd-cmd t)
          (vterm-toggle-send-return))
        (when vterm-toggle-fullscreen-p
          (delete-other-windows))
        (run-hooks 'vterm-toggle-show-hook)))))

(defun vterm-toggle--new()
  "New vterm buffer."
  (if vterm-toggle-fullscreen-p
      (vterm)
    (vterm-other-window)))

(defun vterm-toggle--skip-prompt ()
  "Skip past the text matching regexp `vterm-toggle-prompt-regexp'.
If this takes us past the end of the current line, don't skip at all."
  (let ((eol (line-end-position)))
    (when (and (looking-at vterm-toggle-prompt-regexp)
	           (<= (match-end 0) eol))
      (goto-char (match-end 0)))))

(defun vterm-toggle--accept-cmd-p ()
  "Check whether the vterm can accept user comand."
  (save-excursion
    (goto-char (point-at-bol))
    (vterm-toggle--skip-prompt)))


(defun vterm-toggle--get-buffer(&optional make-cd args)
  "Get vterm buffer.
Optional argument MAKE-CD make cd or not.
Optional argument ARGS optional args."
  (if vterm-toggle-use-dedicated-buffer
      (vterm-toggle--get-dedicated-buffer)
    (vterm-toggle--recent-vterm-buffer make-cd args)))

(defun vterm-toggle--get-dedicated-buffer()
  "Get dedicated buffer."
  (if (buffer-live-p vterm-toggle--vterm-dedicated-buffer)
      vterm-toggle--vterm-dedicated-buffer
    (setq vterm-toggle--vterm-dedicated-buffer (vterm-toggle--new))))

(defun vterm-toggle--recent-vterm-buffer(&optional make-cd args)
  "Get recent vterm buffer.
Optional argument MAKE-CD make cd or not.
Optional argument ARGS optional args."
  (let ((shell-buffer)
        buffer-host
        vterm-host)
    (if (ignore-errors (file-remote-p default-directory))
        (with-parsed-tramp-file-name default-directory nil
          (setq buffer-host host))
      (setq buffer-host (system-name)))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (funcall vterm-toggle--vterm-buffer-p-function args)
          (cond
           ((and (derived-mode-p 'vterm-mode)
                 make-cd)
            (if (ignore-errors (file-remote-p default-directory))
                (with-parsed-tramp-file-name default-directory nil
                  (setq vterm-host host))
              (setq vterm-host (system-name)))
            (when (and (vterm-toggle--accept-cmd-p)
                       (equal buffer-host vterm-host))
              (unless shell-buffer
                (setq shell-buffer buf))))
           (t
            (unless shell-buffer
              (setq shell-buffer buf)))))))
    shell-buffer))

(defun vterm-toggle--recent-other-buffer(&optional args)
  "Get last viewed buffer.
Optional argument ARGS optional args."
  (let (shell-buffer)
    (cl-loop for buf in (buffer-list) do
             (with-current-buffer buf
               (when (and (not (funcall vterm-toggle--vterm-buffer-p-function args))
                          (not (char-equal ?\  (aref (buffer-name) 0))))
                 (setq shell-buffer buf)))
             until shell-buffer)
    shell-buffer))

(defun vterm-toggle--exit-hook()
  "Vterm exit hook."
  (when (derived-mode-p 'vterm-mode)
    (setq vterm-toggle--buffer-list
	      (delq (current-buffer) vterm-toggle--buffer-list))
    (when (and vterm-toggle-reset-window-configration-after-exit
               vterm-toggle--window-configration)
      (set-window-configuration vterm-toggle--window-configration))))

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
