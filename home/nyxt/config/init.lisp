(define-configuration buffer
  ((default-modes (append '(nyxt::vi-normal-mode) ; Vi bindings
                          '(blocker-mode) ; blocker-mode on every buffer
                          %slot-default%))))

(define-configuration prompt-buffer
  ;; Automatically enter insert mode in prompt buffer
  ((default-modes (append '(nyxt::vi-insert-mode) %slot-default%))))

;; Don't run search autocompletions
(define-configuration buffer
  ((search-always-auto-complete-p nil)))
