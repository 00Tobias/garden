;;; init-text.el --- Configuration for text modes  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq sentence-end-double-space nil)

(setq-default fill-column 80)

(add-hook 'text-mode-hook 'visual-line-mode)

(defun calculate-and-replace-number ()
  (interactive)
  (let* ((buffer-operand (number-at-point))
         (operand-bounds (bounds-of-thing-at-point 'number))
         (operation (completing-read (format "Operation for %s: " buffer-operand)
                                     '("add" "subtract" "multiply" "divide" "modulo")
                                     nil t))
         (prompted-operand (read-number (format "Number to %s with: " operation))))
    (kill-region (car operand-bounds) (cdr operand-bounds))
    (insert (number-to-string
             (pcase operation
               ("add"      (+   buffer-operand prompted-operand))
               ("subtract" (-   buffer-operand prompted-operand))
               ("multiply" (*   buffer-operand prompted-operand))
               ("divide"   (/   buffer-operand prompted-operand))
               ("modulo"   (mod buffer-operand prompted-operand))
               (_ (error "Unsupported operation")))))))

(keymap-global-set "C-c n" 'calculate-and-replace-number)

(use-package jinx
  :after vertico-multiform
  :hook ((text-mode conf-mode) . jinx-mode)
  :bind (("M-$"   . jinx-correct)
         ("C-c c" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :init (setq jinx-languages "en_US sv_SE")
  :config (add-to-list 'vertico-multiform-categories
                       '(jinx grid (vertico-grid-annotate . 20))))

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

;; Font configuration adapted from https://zzamboni.org/post/beautifying-org-mode-in-emacs/
;; and https://sophiebos.io/posts/beautifying-emacs-org-mode/
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(require 'org-indent)
(set-face-attribute 'org-table nil           :inherit 'fixed-pitch)
(set-face-attribute 'org-block nil           :foreground 'unspecified :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil            :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil          :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil        :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil       :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil        :inherit 'fixed-pitch)

(dolist (face '((org-level-1 . 1.35)
                (org-level-2 . 1.3)
                (org-level-3 . 1.2)
                (org-level-4 . 1.1)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "AtkynsonNext Nerd Font Propo" :weight 'bold :height (cdr face)))

(set-face-attribute 'org-document-title nil :font "AtkynsonNext Nerd Font Propo" :weight 'bold :height 1.8)

(add-hook 'org-mode-hook 'variable-pitch-mode)

(use-package org-bullets
  :hook org-mode
  :init (setq org-bullets-bullet-list '("∙")))

(provide 'init-text)
;;; init-text.el ends here
