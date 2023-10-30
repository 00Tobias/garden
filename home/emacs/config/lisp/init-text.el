;;; init-text.el --- Configuration for text modes  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq sentence-end-double-space nil)

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(keymap-global-set "C-c f" 'flyspell-mode)

(keymap-global-set "C-c d" 'dictionary-lookup-definition)

(setq-default fill-column 80)

(add-hook 'text-mode-hook 'visual-line-mode)

;;; Org mode

(setq org-startup-indented t)

(with-eval-after-load 'org
  (keymap-set-keys org-mode-map
    "C-M-p" 'org-up-element
    "C-M-f" 'org-forward-heading-same-level
    "C-M-b" 'org-backward-heading-same-level
    "C-M-n" 'org-down-element))

;; Complete org-mode blocks
;; TODO: Uncomment when on *ELPA
;; (with-package 'org-block-capf
;;   (add-hook 'org-mode-hook 'org-block-capf-add-to-completion-at-point-functions))

;;; package: org-roam
(setq org-roam-directory (file-truename "~/org/")
      org-roam-dailies-directory "journals/"
      org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
      org-roam-capture-templates '(("d" "default" plain
                                    "%?" :target
                                    (file+head "pages/${slug}.org" "#+title: ${title}\n")
                                    :unnarrowed t)))
(keymap-global-set-keys
 "C-c n l" 'org-roam-buffer-toggle
 "C-c n f" 'org-roam-node-find
 "C-c n g" 'org-roam-graph
 "C-c n i" 'org-roam-node-insert
 "C-c n c" 'org-roam-capture
 "C-c n j" 'org-roam-dailies-capture-today)
(org-roam-db-autosync-mode)

(provide 'init-text)
;;; init-text.el ends here
