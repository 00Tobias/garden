;;; init-modal.el --- Personalized modal editing using ryo-modal  -*- lexical-binding: t; -*-

;;; Commentary:
;; TODO: word-select.kak type movement

;;; Code:

(defvar modal-mode-map
  (let ((map (make-sparse-keymap))) map)
  "Keymap for `modal-mode'.")

(define-minor-mode modal-mode
  "Simple mode used to add a modal keymap to Emacs."
  :global nil
  :lighter " [M]"
  :keymap modal-mode-map
  :group modal
  (if modal-mode
      (setq-local cursor-type 'box)
    (setq-local cursor-type 'bar)))

(add-hook 'text-mode-hook 'modal-mode 1)
(add-hook 'prog-mode-hook 'modal-mode 1)
(add-hook 'xref--xref-buffer-mode-hook 'modal-mode 1)

(keymap-global-set "<escape>" (lambda () (interactive) (modal-mode 1)))

(keymap-global-set "C-c p" (lambda () (interactive) (setq unread-command-events (listify-key-sequence "\C-x\p"))))

(defun delete-indentation-below ()
  (interactive)
  (forward-line)
  (delete-indentation))

(defun fake-C-c ()
  (interactive)
  (setq unread-command-events (listify-key-sequence "\C-c")))

(defun modal-insert ()
  "Simple helper function to exit ryo-modal-mode"
  (interactive)
  (modal-mode 0))

(defun modal-dynamic-append ()
  (interactive)
  (if (region-active-p)
      (progn (goto-char (region-end))
             (deactivate-mark))
    (end-of-line))
  (modal-mode 0))

(defun modal-dynamic-kill ()
  (interactive)
  (if (region-active-p)
      (progn (call-interactively 'kill-region)
             (cond ((looking-back "[-_]") (backward-char) (call-interactively (key-binding (kbd "C-d"))))
                   ((looking-at "[-_]") (call-interactively (key-binding (kbd "C-d"))))
                   ((looking-back " ") (backward-char) (call-interactively (key-binding (kbd "C-d"))))
                   ((looking-at " ") (call-interactively (key-binding (kbd "C-d"))))))
    (call-interactively (key-binding (kbd "C-d")))))

(defun modal-change ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-region)
    (call-interactively (key-binding (kbd "C-d"))))
  (modal-mode 0))

(defun modal-forward-word-select ()
  (interactive)
  (if (or (not (looking-at "[a-zA-Z0-9]")) (region-active-p))
      (forward-to-word))
  (er/mark-word))

(defun modal-backward-word-select ()
  (interactive)
  (if (or (not (looking-at "[a-zA-Z0-9]")) (region-active-p))
      (backward-to-word))
  (er/mark-word))

(defun backward-mark-word ()
  (interactive)
  (mark-word -1 t))

(defun modal-kill-current-buffer-if-read-only ()
  (interactive)
  (if buffer-read-only
      (progn (call-interactively (key-binding (kbd "q")))
             (modal-mode 0))
    (previous-buffer)))

(defun modal-open-line-above ()
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode)
  (modal-mode 0))

(defun modal-open-line-below ()
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent)
  (modal-mode 0))

(keymap-set-keys modal-mode-map
  "0" "M-0"
  "1" "M-1"
  "2" "M-2"
  "3" "M-3"
  "4" "M-4"
  "5" "M-5"
  "6" "M-6"
  "7" "M-7"
  "8" "M-8"
  "9" "M-9"
  "<escape>" 'keyboard-escape-quit
  "g" 'keyboard-escape-quit
  "G" 'exchange-point-and-mark
  "," 'fake-C-c

  ;; Movement Colemak and QWERTY
  "h" 'backward-char
  "j" 'next-line
  "k" 'previous-line
  "l" 'forward-char
  "H" "C-M-u"
  "J" "C-M-f"
  "K" "C-M-b"
  "L" "C-M-d"
  "n" 'backward-char
  "e" 'next-line
  "i" 'previous-line
  "o" 'forward-char
  "N" "C-M-u"
  "E" "C-M-f"
  "I" "C-M-b"
  "O" "C-M-d"

  "w" 'modal-forward-word-select
  "W" 'mark-word
  "b" 'modal-backward-word-select
  "B" 'backward-mark-word

  "p" 'er/expand-region
  "P" 'er/contract-region

  "f" 'avy-goto-char
  "F" 'avy-goto-char-2

  "z" 'move-beginning-of-line
  "Z" "C-M-a"
  "x" 'move-end-of-line
  "X" "C-M-e"

  ">" 'end-of-buffer
  "<" 'beginning-of-buffer

  "m" 'point-to-register
  "M" 'jump-to-register

  "q" 'modal-kill-current-buffer-if-read-only
  "Q" 'kill-current-buffer
  "t" 'consult-buffer
  "T" 'project-find-file

  "v" 'delete-indentation
  "V" 'delete-indentation-below

  ;; Deleting
  "d" 'modal-dynamic-kill
  "D" "C-k"

  ;; Insert / Append / Substitute text
  "s" 'modal-insert
  "S" 'modal-dynamic-append
  "r" 'modal-open-line-below
  "R" 'modal-open-line-above
  "c" 'modal-change "C" 'modal-change

  ;; Copy / paste region
  "y" 'kill-ring-save
  "Y" 'yank

  "SPC" 'set-mark-command

  ;; Undo / redo
  "u" 'undo
  "U" 'undo-redo

  "a" 'embark-act
  "A" 'embark-dwim

  "." 'repeat
  ":" 'execute-extended-command

  ;; Add comment above / below
  ";" 'comment-line)

(provide 'init-modal)
;;; init-modal.el ends here
