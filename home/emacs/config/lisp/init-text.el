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

(use-package org-bullets
  :hook org-mode
  :init (setq org-bullets-bullet-list '("∙")))

(provide 'init-text)
;;; init-text.el ends here
