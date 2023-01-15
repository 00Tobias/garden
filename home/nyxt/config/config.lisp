(defmethod initialize-instance :after
           ((interface password:keepassxc-interface)
            &key &allow-other-keys)
           "It's obviously not recommended to set master password here,
as your config is likely unencrypted and can reveal your password to someone
peeking at the screen."
           (setf (password:password-file interface) "~/kpxc-db/passwords.kdbx"
                 ;; (password:yubikey-slot interface) "1:1111"
                 ))

(define-configuration nyxt/password-mode:password-mode
                      ((nyxt/password-mode:password-interface
                        (make-instance 'password:keepassxc-interface))))

(define-configuration web-buffer
                      ((default-modes (append '(dark-mode) %slot-default%))))

(define-configuration buffer
                      ((default-modes (append (list 'nyxt/vi-mode:vi-normal-mode      ; Vi bindings
                                                    'nyxt/password-mode:password-mode ; Password management
                                                    'nyxt/blocker-mode:blocker-mode)  ; Adblock
                                              %slot-default%))))

(define-configuration prompt-buffer
                      ((default-modes (append '(nyxt/vi-mode:vi-insert-mode) %slot-default%))))

(define-configuration browser
                      ((theme
                        (make-instance 'theme:theme :dark-p t :background-color "black"
                                       :on-background-color "#808080" :accent-color
                                       "#37a8e4" :on-accent-color "black" :primary-color
                                       "gray" :on-primary-color "white" :secondary-color
                                       "darkgray" :on-secondary-color "black"))))


;; Don't autocomplete searches
(define-configuration buffer
                      ((search-always-auto-complete-p nil)))

;; Make Kagi my search engine
(defvar *my-search-engines*
  (list
   '("kagi" "https://kagi.com/search?q=~a" "https://kagi.com/"))
  "List of search engines.")

(define-configuration context-buffer
                      "Go through the search engines above and `make-search-engine' out of them."
                      ((search-engines
                        (append
                         (mapcar (lambda (engine) (apply 'make-search-engine engine))
                                 *my-search-engines*)
                         %slot-default%))))
