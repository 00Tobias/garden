;;; init-modal.el --- Personalized modal editing using ryo-modal  -*- lexical-binding: t; -*-

;;; Commentary:
;; TODO: word-select.kak type movement

;;; Code:

(defvar modal-mode-map
  (let ((map (make-sparse-keymap))) map)
  "Keymap for `modal-mode'.")

(define-minor-mode modal-mode
  "Simple mode used to add a modal keymap to Emacs"
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

(keymap-global-set "<escape>" (lambda ()
                                (interactive)
                                (modal-mode 1)))


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

(defun modal-dynamic-kill ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-region)
    (call-interactively (key-binding (kbd "C-d")))))

(defun modal-change ()
  (interactive)
  (modal-dynamic-kill)
  (modal-mode 0))

(defun modal-kill-current-buffer-if-read-only ()
  (interactive)
  (modal-mode 0)
  (if buffer-read-only
      (call-interactively (key-binding (kbd "q")))
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
  "," 'fake-C-c

  ;; Movement Colemak and QWERTY
  "h" 'backward-char
  "j" 'next-line
  "k" 'previous-line
  "l" 'forward-char
  "H" "C-M-p"
  "J" "C-M-f"
  "K" "C-M-b"
  "L" "C-M-n"
  "n" 'backward-char
  "e" 'next-line
  "i" 'previous-line
  "o" 'forward-char
  "N" "C-M-p"
  "E" "C-M-f"
  "I" "C-M-b"
  "O" "C-M-n"

  "w" 'right-word
  "W" 'avy-goto-word-1-below
  "b" 'left-word
  "B" 'avy-goto-word-1-above

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

  "v" 'delete-indentation
  "V" 'delete-indentation-below

  ;; Deleting
  "d" 'modal-dynamic-kill
  "D" "C-k"

  ;; Insert / Append / Substitute text
  "s" 'modal-insert "S" 'modal-insert
  "r" 'modal-open-line-below
  "R" 'modal-open-line-above
  "c" 'modal-change

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

  ;; Add comment above / below
  ";" 'comment-line)

(provide 'init-modal)
;;; init-modal.el ends here
