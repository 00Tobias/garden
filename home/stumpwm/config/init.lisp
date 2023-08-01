(in-package :stumpwm)
;; (asdf:load-system :sb-cltl2)

(require 'slynk)
(defcommand start-slynk () ()
            (slynk:create-server :dont-close t :port 4006))
(define-key *top-map* (kbd "s-s") "start-slynk")

(defun list-define-keys (map alist)
  "define key using alist."
  (loop for (key . command) in alist
        do (define-key map (kbd key) command)))

(set-prefix-key (kbd "s-t"))

(list-define-keys *top-map*
                  '(("s-;" . "colon")

                    ("s-1" . "gselect Base")
                    ("s-2" . "gselect Mail")
                    ("s-3" . "gselect Console")
                    ("s-4" . "gselect Work")

                    ("s-h" . "move-focus left")  ("s-n" . "move-focus left")
                    ("s-j" . "move-focus down")  ("s-e" . "move-focus down")
                    ("s-k" . "move-focus up")    ("s-i" . "move-focus up")
                    ("s-l" . "move-focus right") ("s-o" . "move-focus right")

                    ("C-s-h" . "hsplit")  ("C-s-n" . "hsplit")
                    ("C-s-j" . "vsplit")  ("C-s-e" . "vsplit")
                    ;; ("C-s-k" . "move-focus up")    ("C-s-i" . "move-focus up")
                    ;; ("C-s-l" . "move-focus right") ("C-s-o" . "move-focus right")

                    ("s-H" . "move-window left")  ("s-N" . "move-window left")
                    ("s-J" . "move-window down")  ("s-E" . "move-window down")
                    ("s-K" . "move-window up")    ("s-I" . "move-window up")
                    ("s-L" . "move-window right") ("s-O" . "move-window right")

                    ("C-s-H" . "exchange-direction left")  ("C-s-N" . "exchange-direction left")
                    ("C-s-J" . "exchange-direction down")  ("C-s-E" . "exchange-direction down")
                    ("C-s-K" . "exchange-direction up")    ("C-s-I" . "exchange-direction up")
                    ("C-s-L" . "exchange-direction right") ("C-s-O" . "exchange-direction right")

                    ("s-n" . "next-in-frame") ("s-p" . "prev-in-frame")

                    ("s-f" . "toggle-float") ("s-F" . "fullscreen")

                    ("s-q" . "kill-window") ("s-Q" . "remove-split")

                    ("s-Return" . "exec emacsclient -cne '(switch-to-buffer nil)'")

                    ("XF86AudioRaiseVolume" . "exec playerctl volume 0.03+")
                    ("XF86AudioLowerVolume" . "exec playerctl volume 0.03-")
                    ("XF86AudioMute" . "exec pulsemixer --toggle-mute")

                    ("XF86AudioPlay" . "exec playerctl play-pause")
                    ("XF86AudioPrev" . "exec playerctl next")
                    ("XF86AudioNext" . "exec playerctl previous")

                    ("XF86MonBrightnessUp" . "backlight-up")
                    ("XF86MonBrightnessDown" . "backlight-down")
                    ))

;; (asdf:load-system :cpu)
;; (load-module "cpu")

;; (asdf:load-system :mem)
;; (load-module "mem")

;; (asdf:load-system :disk)
;; (load-module "disk")

;; (asdf:load-system :battery-portable)
;; (load-module "battery-portable")

;; (asdf:load-system :notify)
;; (load-module "notify")
;; (notify:notify-server-toggle)

(setf *startup-message* nil)
(setf *mouse-focus-policy* :click
      *float-window-modifiers* :SUPER)
(setf *message-window-gravity* :top
      *input-window-gravity* :center
      *window-border-style* :thin)
(setf *ignore-wm-honts* t)

(setf *window-format* "%m%n%s%c")
(setf *screen-mode-line-format* (list "[^B%n^b] %W^>%d"))
(setf *time-modeline-string* "%a %b %e %k:%M")

(enable-mode-line (current-screen) (current-head) t)
