# vterm-toggle Toggle to and from the vterm buffer

  https://github.com/akermu/emacs-libvterm

```
(global-set-key [f2] 'vterm-toggle)
(global-set-key [C-f2] 'vterm-toggle-cd)
```
 Provides the command `vterm-toggle` which toggles between the
 vterm buffer and whatever buffer you are editing.

 This is done in an "intelligent" way.  Features are:
 1. Starts a vterm if none is existent.
 2. Minimum distortion of your window configuration.
 3. When done in the vterm-buffer you are returned to the same window
    configuration you had before you toggled to the shell.
 4. If you desire, you automagically get a `cd` command in the shell to the
   directory where your current buffers file exists(`even in a ssh session`); just call
   `vterm-toggle-cd` instead of `vterm-toggle`.
# Customize
## show vterm buffer in current window
```
(setq vterm-toggle-fullscreen-p nil)
(setq display-buffer-alist
      '(
        ("vterm.*" ;; match your vterm buffer name
         (display-buffer-reuse-window display-buffer-same-window))
        ))


```
## show vterm buffer in left side
```
(setq display-buffer-alist
      '(
        ("vterm.*"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (reusable-frames . visible)
         (side . left)
         (window-width . 0.5)
         )))
```
## vterm-toggle-prompt-regexp
 you need make sure your shell prompt match this regexp
