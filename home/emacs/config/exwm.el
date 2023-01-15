;;; exwm --- Configuration for EXWM -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq display-time-24hr-format 1
      display-time-format "[%H:%M - %Y/%m/%d]")
(display-time-mode 1)

(display-battery-mode 1)

;; exwm
;; (require 'exwm-config)
(setq exwm-input-global-keys
      `((,(kbd "s-R")   . exwm-reset)
        (,(kbd "s-f")   . exwm-layout-toggle-fullscreen)
        (,(kbd "s-f")   . exwm-floating-toggle-floating)
        (,(kbd "s-w")   . exwm-workspace-switch)
        (,(kbd "s-SPC") . dmenu)

        (,(kbd "s-h") . windmove-left)
        (,(kbd "s-j") . windmove-down)
        (,(kbd "s-k") . windmove-up)
        (,(kbd "s-l") . windmove-right)

        (,(kbd "s-H") . windower-swap-left)
        (,(kbd "s-J") . windower-swap-below)
        (,(kbd "s-K") . windower-swap-above)
        (,(kbd "s-L") . windower-swap-right)

        (,(kbd "C-s-h") . windower-move-border-left)
        (,(kbd "C-s-j") . windower-move-border-below)
        (,(kbd "C-s-k") . windower-move-border-above)
        (,(kbd "C-s-l") . windower-move-border-right)

        (,(kbd "C-s-H") . split-window-horizontally)
        (,(kbd "C-s-J") . split-window-vertically)
        (,(kbd "C-s-K") . split-window-below)
        (,(kbd "C-s-L") . split-window-right)))

(exwm-enable)

;; ednc
(ednc-mode 1)
(diminish 'ednc-mode)

(provide 'exwm)
;;; exwm.el ends here
