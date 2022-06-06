;;; vterm-toggle.el --- Toggles between the vterm buffer and other buffers.  -*- lexical-binding: t; -*-

;; Author: jixiuf  jixiuf@qq.com
;; Keywords: vterm terminals
;; Version: 0.0.4
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

(declare-function vterm "vterm")
(declare-function vterm-send-C-a "vterm")
(declare-function vterm-send-C-k "vterm")
(declare-function vterm-cursor-in-command-buffer-p "vterm")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")
(declare-function vterm-other-window "vterm")
(defvar vterm-buffer-name  "*vterm*")

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
  "`project' limit the scope only in the current project.
`frame' limit the scope only not in other frame.
`dedicated' use the dedicated vterm buffer."
  :group 'vterm-toggle
  :type '(radio
          (const :tag "all" nil)
          (const :tag "project" project)
          (const :tag "frame" frame)
          (const :tag "dedicated" dedicated)))

(defcustom vterm-toggle-project-root t
  "Create a new vterm buffter at project root directory or not.
it only work  when `vterm-toggle-scope' is `project'. "
  :group 'vterm-toggle
  :type 'boolean)

(defcustom vterm-toggle-cd-auto-create-buffer nil
  "If the prompt of recent vterm buffer is not available,
`vterm-toggle-cd' would create a new vterm buffer."
  :group 'vterm-toggle
  :type 'boolean)

(defcustom vterm-toggle-reset-window-configration-after-exit 'kill-window-only
  "Whether reset window configuration after vterm buffer is killed."
  :group 'vterm-toggle
  :type '(choice
          (const :tag "Do nothing" nil)
          (const :tag "Reset window configration after exit" t)
          (const :tag "Kill Window only" kill-window-only)))

(defcustom vterm-toggle-hide-method 'delete-window
  "How to hide the vterm buffer"
  :group 'vterm-toggle
  :type '(choice
          (const :tag "Toggle without closing the vterm window(focus other window)" nil)
          (const :tag "Reset Window configration" reset-window-configration)
          (const :tag "Bury All vterm buffer" bury-all-vterm-buffer)
          (const :tag "Quit window" quit-window)
          (const :tag "Delete window" delete-window)))

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
   ((or (derived-mode-p 'vterm-mode)
        (and (vterm-toggle--get-window)
             vterm-toggle-hide-method))
    (if (equal (prefix-numeric-value args) 1)
        (vterm-toggle-hide)
      (vterm vterm-buffer-name)))
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
   ((or (derived-mode-p 'vterm-mode)
        (and (vterm-toggle--get-window)
             vterm-toggle-hide-method))
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
  (or (derived-mode-p 'vterm-mode)
      (select-window (vterm-toggle--get-window)))
  (run-hooks 'vterm-toggle-hide-hook)
  (cond
   ((eq vterm-toggle-hide-method 'reset-window-configration)
    (when vterm-toggle--window-configration
      (set-window-configuration vterm-toggle--window-configration)))
   ((eq vterm-toggle-hide-method 'bury-all-vterm-buffer)
    (vterm-toggle--bury-all-vterm))
   ((eq vterm-toggle-hide-method 'quit-window)
    (quit-window))
   ((eq vterm-toggle-hide-method 'delete-window)
    (if (window-deletable-p)
        (delete-window)
      (vterm-toggle--bury-all-vterm)))
   ((not vterm-toggle-hide-method)
    (let ((buf (vterm-toggle--recent-other-buffer)))
      (when buf
        (if (get-buffer-window buf)
            (select-window (get-buffer-window buf))
          (if vterm-toggle-fullscreen-p
              (switch-to-buffer buf)
            (switch-to-buffer-other-window buf))))))))

(defun vterm-toggle--get-window()
  "Get the vterm window which is visible (active or inactive)."
  (cl-find-if #'(lambda(w)
                  (provided-mode-derived-p
                   (buffer-local-value 'major-mode (window-buffer w))
                   'vterm-mode))
              (window-list)))

(defun vterm-toggle--bury-all-vterm ()
  "Bury all vterm buffer in order."
  (dolist (buf (buffer-list))
    (when (eq (buffer-local-value 'major-mode buf) 'vterm-mode)
      (with-current-buffer buf
        (bury-buffer)))))

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
  (when (eq vterm-toggle-scope 'projectile)
    (warn "the value of `vterm-toggle-scope' is 'projectile, please change it to 'project"))
  (let* ((shell-buffer (vterm-toggle--get-buffer
                        make-cd (not vterm-toggle-cd-auto-create-buffer)))
         (dir (expand-file-name default-directory))
         cd-cmd cur-host vterm-dir vterm-host cur-user cur-port   )
    (if (ignore-errors (file-remote-p dir))
        (with-parsed-tramp-file-name dir nil
          (setq cur-host host)
          (setq dir localname))
      (setq cur-host (system-name)))
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
            (if (eq major-mode 'vterm-mode)
                (switch-to-buffer shell-buffer nil t)
              (pop-to-buffer shell-buffer)))
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
                (if (vterm-toggle--in-cmd-buffer-p)
                    (vterm-toggle-insert-cd)
                  (message "You can insert '%s' by M-x:vterm-toggle-insert-cd."
                           vterm-toggle--cd-cmd))))
            (run-hooks 'vterm-toggle-show-hook)))
      (unless (eq major-mode 'vterm-mode)
        (setq vterm-toggle--window-configration (current-window-configuration)))
      (with-current-buffer (setq shell-buffer (vterm-toggle--new))
        (vterm-toggle--wait-prompt)
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
  (if (eq major-mode 'vterm-mode)
      (when vterm-toggle--cd-cmd
        (vterm-send-string vterm-toggle--cd-cmd t)
        (vterm-send-return))
    (call-interactively #'vterm-toggle-cd-show)))

(defun vterm-toggle--new(&optional buffer-name)
  "New vterm buffer."
  (let ((default-directory default-directory)
        (buffer-name (or buffer-name vterm-buffer-name))
        project-root)
    (when (and vterm-toggle-project-root
               (eq vterm-toggle-scope 'project))
      (setq project-root (vterm-toggle--project-root))
      (when project-root
        (setq default-directory project-root)))
    (if vterm-toggle-fullscreen-p
        (vterm buffer-name)
      (if (eq major-mode 'vterm-mode)
          (let ((display-buffer-alist nil))
            (vterm buffer-name))
        (vterm-other-window buffer-name)))))


(defun vterm-toggle--get-buffer(&optional make-cd ignore-prompt-p)
  "Get vterm buffer.
Optional argument MAKE-CD make cd or not.
Optional argument ARGS optional args."
  (cond
   ((eq vterm-toggle-scope 'dedicated)
    (vterm-toggle--get-dedicated-buffer))
   ((eq vterm-toggle-scope 'project)
    (let* ((project-root (vterm-toggle--project-root))
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
                              (and (eq vterm-toggle-scope 'project)
                                   (equal (vterm-toggle--project-root) dir))))
                 (cond
                  ((and  make-cd (derived-mode-p 'vterm-mode))
                   (if (ignore-errors (file-remote-p default-directory))
                       (with-parsed-tramp-file-name default-directory nil
                         (setq vterm-host host))
                     (setq vterm-host (system-name)))
                   (when (and (or ignore-prompt-p
                                  (vterm-toggle--in-cmd-buffer-p))
                              (equal buffer-host vterm-host))
                     (setq shell-buffer buf)))
                  (t (setq shell-buffer buf)))))
             until shell-buffer)
    shell-buffer))

(defun vterm-toggle--in-cmd-buffer-p()
  (when (vterm-cursor-in-command-buffer-p)
    (or (eq (vterm--get-prompt-point) (vterm--get-cursor-point))
        (and (vterm--backward-char)
             (vterm--forward-char)))))

(defun vterm-toggle--project-root()
  (let ((proj (project-current)))
    (when proj
      (if (fboundp 'project-root)
          (project-root proj)
        (car (project-roots proj))))))

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
        (if (window-deletable-p)
            (delete-window)
          (quit-window))
      (when (and vterm-toggle-reset-window-configration-after-exit
                 vterm-toggle--window-configration)
        (set-window-configuration vterm-toggle--window-configration)))))

(add-hook 'kill-buffer-hook #'vterm-toggle--exit-hook)
;; (add-hook 'vterm-exit-functions #'vterm-toggle--exit-hook)

(defun vterm-toggle--mode-hook()
  "Hook for `vterm-mode-hook'."
  (add-to-list 'vterm-toggle--buffer-list (current-buffer)))
(add-hook 'vterm-mode-hook #'vterm-toggle--mode-hook)

(dolist (buf (buffer-list))
  (when (eq (buffer-local-value 'major-mode buf) 'vterm-mode)
    (add-to-list 'vterm-toggle--buffer-list buf t)))

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
    (call-interactively 'vterm)))

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

(require 'vterm nil t)                  ; https://github.com/jixiuf/vterm-toggle/issues/24

;; Local Variables:
;; coding: utf-8
;; End:

;;; vterm-toggle.el ends here
