;;; init-exwm --- Configuration for EXWM -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; exwm
;; (require 'exwm-config)
(setq exwm-input-global-keys
      `(([s-R] . exwm-reset)
        ([s-SPC] . dmenu)))

(exwm-enable)

(provide 'init-exwm)
;;; init-exwm.el ends here
