;;; init-text.el --- Configuration for text modes  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq sentence-end-double-space nil)

(keymap-global-set "C-c d" 'dictionary-search)

(setq-default fill-column 80)

(add-hook 'text-mode-hook 'visual-line-mode)

;;; package: jinx
(setq jinx-languages "en_US sv_SE")

(dolist (hook '(text-mode-hook conf-mode-hook))
  (add-hook hook #'jinx-mode))

(keymap-global-set "M-$" #'jinx-correct)
(keymap-global-set "C-c c" #'jinx-correct)
(keymap-global-set "C-M-$" #'jinx-languages)

;;; Org mode

(setq org-startup-indented t
      org-hide-emphasis-markers t
      org-pretty-entities t
      org-ellipsis " ➤"
      org-startup-with-inline-images t
      org-latex-tables-centered nil)

(with-eval-after-load 'org
  (keymap-set-keys org-mode-map
    "C-M-p" 'org-up-element
    "C-M-f" 'org-forward-heading-same-level
    "C-M-b" 'org-backward-heading-same-level
    "C-M-n" 'org-down-element))

;;; package: org-block-capf
(add-hook 'org-mode-hook 'org-block-capf-add-to-completion-at-point-functions)
(add-hook 'org-mode-hook (lambda ()
                           (setq-local electric-pair-inhibit-predicate
                                       `(lambda (c)
                                          (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

;;; package: org-bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-bullets-bullet-list '("∙"))

(provide 'init-text)
;;; init-text.el ends here
