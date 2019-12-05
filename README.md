# vterm-toggle Toggle vterm buffer

[emacs-libvterm](https://github.com/akermu/emacs-libvterm) implements a bridge
to libvterm to display a terminal in a emacs buffer.

This package provides the command `vterm-toggle` which toggles between the vterm
buffer and whatever buffer you are editing.

 This is done in an "intelligent" way.  Features are:
 1. Starts a vterm if none is existent.
 2. Minimum distortion of your window configuration.
 3. When done in the vterm-buffer you are returned to the same window
    configuration you had before you toggled to the shell.
 4. If you desire, you automagically get a `cd` command in the shell to the
    directory where your current buffers file exists(`even remote file through
    tramp is supported `); just call `vterm-toggle-cd` instead of
    `vterm-toggle`.

```
(global-set-key [f2] 'vterm-toggle)
(global-set-key [C-f2] 'vterm-toggle-cd)

;; you can cd to the directory where your previous buffer file exists
;; after you have toggle to the vterm buffer with `vterm-toggle'.
(define-key vterm-mode-map [(control return)]   #'vterm-toggle-insert-cd)

;Switch to next vterm buffer
(define-key vterm-mode-map (kbd "s-n")   'vterm-toggle-forward)
;Switch to previous vterm buffer
(define-key vterm-mode-map (kbd "s-p")   'vterm-toggle-backward)

```
##  open a terminal in a specified remote location
```
(defun myssh()
    (interactive)
    (let ((default-directory "/ssh:root@host:~"))
      (vterm-toggle-cd)))

```
With https://github.com/emacs-pe/docker-tramp.el
you can open a terminal in a specified docker container
```
(defun mydocker()
    (interactive)
    (let ((default-directory "/docker:root@697a9e2789c3:/root/"))
      (vterm-toggle-cd)))
```

# Customize
## show vterm buffer in current window
```
(setq vterm-toggle-fullscreen-p nil)
(setq display-buffer-alist
      '(
        ("^v?term.*" ;; match your vterm buffer name
         (display-buffer-reuse-window display-buffer-same-window))
        ))


```
## show vterm buffer in bottom side
```
(setq vterm-toggle-fullscreen-p nil)
(add-to-list 'display-buffer-alist
             '("^v?term.*"
                (display-buffer-reuse-window display-buffer-at-bottom)
                ;;(display-buffer-reuse-window display-buffer-in-direction)
                ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                ;;(direction . bottom)
                ;;(dedicated . t) ;dedicated is supported in emacs27
                (reusable-frames . visible)
                (window-height . 0.3)))
```
## show  vterm buffer in side window
If you want show vterm buffer at bottom side window:
```
(setq vterm-toggle-fullscreen-p nil)
(add-to-list 'display-buffer-alist
             '("^v?term.*"
                (display-buffer-reuse-window display-buffer-in-side-window)
                (side . bottom)
                ;;(dedicated . t) ;dedicated is supported in emacs27
                (reusable-frames . visible)
                (window-height . 0.3)))
```

## vterm-toggle-prompt-regexp
You need make sure your shell prompt match this regexp

# work with centaur-tabs (select next/prev vterm buffer)

   https://github.com/ema2159/centaur-tabs/

You can customize `centaur-tabs` and make all vterm buffer in a tab group, and
use `centaur-tabs-forward` and `centaur-tabs-backward` switch from one vterm
buffer to another.

```
(global-set-key  (kbd "s-n") 'centaur-tabs-forward)
(global-set-key  (kbd "s-p") 'centaur-tabs-backward)
```

```
(setq centaur-tabs-buffer-groups-function 'vmacs-awesome-tab-buffer-groups)
(defun vmacs-awesome-tab-buffer-groups ()
  "`vmacs-awesome-tab-buffer-groups' control buffers' group rules. "
  (list
   (cond
    ((derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode)
     "Term")
    ((string-match-p (rx (or
                          "\*Helm"
                          "\*helm"
                          "\*tramp"
                          "\*Completions\*"
                          "\*sdcv\*"
                          "\*Messages\*"
                          "\*Ido Completions\*"
                          ))
                     (buffer-name))
     "Emacs")
    (t "Common"))))

(setq vterm-toggle--vterm-buffer-p-function 'vmacs-term-mode-p)
(defun vmacs-term-mode-p(&optional args)
  (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode))

```
# vterm-toggle-use-dedicated-buffer
When  vterm-toggle-use-dedicated-buffer is not nil, you can toggle to a dedidated vterm buffer.

you can toggle to different dedidated buffer for different purpose.
For example, I want to replace default `compile` with my `vterm-compile`
```
(defvar vterm-compile-dedidated-buffer nil)
(defun vterm-compile ()
  (interactive)
  (let ((vterm-toggle-use-dedicated-buffer t)
        (vterm-toggle--vterm-dedicated-buffer vterm-compile-dedidated-buffer))
    (with-current-buffer (vterm-toggle-cd)
      (setq vterm-compile-dedidated-buffer (current-buffer))
      (rename-buffer "term compile")
      (compilation-shell-minor-mode 1)
      (vterm-send-string compile-command t)
      (vterm-send-return))
    )
  )
```