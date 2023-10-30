(in-package :stumpwm)
;; (asdf:load-system :sb-cltl2)

;; (dolist (module '("cpu" "mem" "net" "battery-portable" "wifi" "notify" "pass"))
;;   (add-to-load-path (format nil "~a/.guix-home/profile/share/common-lisp/sbcl/stumpwm-~a/"
;;                             (uiop:getenv "HOME")
;;                             module)))

(require 'slynk)
(defcommand start-slynk () ()
            (slynk:create-server :dont-close t :port 4006))
(define-key *top-map* (kbd "s-s") "start-slynk")

(defcommand run-nyxt () ()
            (if (not (string= "" (run-shell-command "pidof nyxt" t)))
                (run-shell-command "nyxt --remote --eval '(make-window)'")
              (run-shell-command "nyxt")))

(defcommand run () ()
            (let ((selection (car (select-from-menu (current-screen)
                                                    (list
                                                     ;; Flatpak
                                                     "discord"
                                                     "discord-screenaudio"
                                                     "steam"
                                                     "spotify"
                                                     "prism"
                                                     "veloren"
                                                     ;; Apps
                                                     "nyxt"
                                                     "qutebrowser"
                                                     "emacs"
                                                     "icecat"
                                                     "urxvt"
                                                     "obs"
                                                     "zrythm"
                                                     "blender"
                                                     "kdenlive"
                                                     "krita"
                                                     "inkscape")
                                                    "run:"))))
              (cond ((string= selection "discord")
                     (run-shell-command "flatpak run com.discordapp.Discord"))
                    ((string= selection "discord-screenaudio")
                     (run-shell-command "flatpak run de.shorsh.discord-screenaudio"))
                    ((string= selection "steam")
                     (run-shell-command "flatpak run com.valvesoftware.Steam"))
                    ((string= selection "spotify")
                     (run-shell-command "flatpak run com.spotify.Client"))
                    ((string= selection "prism")
                     (run-shell-command "flatpak run org.prismlauncher.PrismLauncher"))
                    ((string= selection "veloren")
                     (run-shell-command "flatpak run net.veloren.airshipper"))
                    ((string= selection "nyxt")
                     (run-nyxt))
                    (t (run-shell-command selection)))))

(defcommand power-menu () ()
            (let ((selection (car (select-from-menu (current-screen) (list "loginctl poweroff" "loginctl reboot") "power menu:"))))
              (run-shell-command selection)))

;; (defcommand power-menu () ()
;;   (let ((selection (car (select-from-menu (current-screen)
;;                                              (list "shutdown"
;;                                                    "reboot")
;;                                              "power menu:"))))
;;     (cond ((string= selection "shutdown")
;;            (run-shell-command "shepherd shutdown"))
;;           ((string= selection "reboot")
;;            (run-shell-command "shepherd reboot")))))


(grename "main")
(gnewbg "browser")
(gnewbg "steam")
(gnewbg "game")

(set-prefix-key (kbd "s-t"))

(defun list-define-keys (map alist)
  "define key using alist."
  (loop for (key . command) in alist
        do (define-key map (kbd key) command)))

;; TODO: F-keys for group switching and Win+F-key for original
(list-define-keys *top-map*
                  '(("s-;" . "colon")
                    ("s-Escape" . "power-menu")

                    ("s-1" . "gselect 1")
                    ("s-2" . "gselect 2")
                    ("s-3" . "gselect 3")
                    ("s-4" . "gselect 4")
                    ;; ("F1"  . "gselect main")
                    ;; ("s-F1" . '(send-raw-key-or-seiomthbing))

                    ("s-h" . "move-focus left")  ("s-n" . "move-focus left")
                    ("s-j" . "move-focus down")  ("s-e" . "move-focus down")
                    ("s-k" . "move-focus up")    ("s-i" . "move-focus up")
                    ("s-l" . "move-focus right") ("s-o" . "move-focus right")

                    ("C-s-l" . "hsplit")  ("C-s-o" . "hsplit")
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

                    ("s-TAB" . "next-in-frame") ("s-ISO_Left_Tab" . "prev-in-frame")

                    ("s-f" . "toggle-float") ("s-F" . "fullscreen")

                    ("s-t" . "toggle-always-show") ("s-T" . "toggle-always-on-top")

                    ("s-q" . "delete-window") ("s-Q" . "remove-split")

                    ("s-Return" . "exec emacsclient -cne '(switch-to-buffer nil)'")

                    ;; Apps
                    ("s-space" . "run")

                    ("XF86AudioRaiseVolume" . "exec playerctl volume 0.03+")
                    ("XF86AudioLowerVolume" . "exec playerctl volume 0.03-")
                    ("XF86AudioMute" . "exec pulsemixer --toggle-mute")

                    ("XF86AudioPlay" . "exec playerctl play-pause")
                    ("XF86AudioNext" . "exec playerctl next")
                    ("XF86AudioPrev" . "exec playerctl previous")

                    ("XF86MonBrightnessUp" . "backlight-up")
                    ("XF86MonBrightnessDown" . "backlight-down")))

;; (asdf:load-system :cpu)
;; (load-module "cpu")

;; (asdf:load-system :mem)
;; (load-module "mem")

;; (asdf:load-system :disk)
;; (load-module "net")

;; (asdf:load-system :battery-portable)
;; (load-module "battery-portable")

;; (asdf:load-system :notify)
;; (load-module "notify")
;; (notify:notify-server-toggle)

;; (load-module "pass")

(setf *startup-message* nil
      *suppress-frame-indicator* t)
(setf *mouse-focus-policy* :click
      *float-window-modifiers* :SUPER)
(setf *message-window-gravity* :top
      *input-window-gravity* :center
      *window-border-style* :thin)
(setf *ignore-wm-hints* t)

(setf *window-format* "%m%n%s%c")
(setf *screen-mode-line-format* (list "[^B%n^b] %W^>%d"))
(setf *time-modeline-string* "%a %b %e %k:%M")

(enable-mode-line (current-screen) (current-head) t)

(which-key-mode)

;; FIXME: This sucks
(run-shell-command "xsetroot -solid black")
(run-shell-command "xrandr --output DP-0 --mode 3440x1440 --rate 144")
