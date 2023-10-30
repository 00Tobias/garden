(in-package :nyxt-user)

(define-configuration browser
  ((theme (make-instance 'theme:theme
                         :background-color "black"
                         :on-background-color "white"
                         :primary-color "rgb(170, 170, 170)"
                         :on-primary-color "black"
                         :secondary-color "rgb(100, 100, 100)"
                         :on-secondary-color "white"
                         :accent-color "#37A8E4"
                         :on-accent-color "black"))))

(define-configuration prompt-buffer
  ((default-modes (append '(vi-insert-mode) %slot-value%))))

;; (define-configuration nyxt/mode/hint:hint-mode
;;   (;; (nyxt/mode/hint:fit-to-prompt-p t)
;;    (nyxt/mode/hint:auto-follow-hints-p t)))

;; (defvar *search-engines*
;;   (list '("qwant" "https://lite.qwant.com/?q=~a&s=1&theme=1&ta=2&a=1" "https://lite.qwant.com/?&s=1&theme=1&ta=2&a=1")))

;; (define-mode my-blocker-mode (nyxt/blocker-mode:blocker-mode)
;;   ((nyxt/blocker-mode:hostlists (list *my-blocked-hosts* nyxt/blocker-mode:*default-hostlist*))))

;; (define-configuration base-mode
;;   ((keyscheme-map
;;     (define-keyscheme-map "my-base" (list :import %slot-value%)
;;                           keyscheme:emacs
;;                           (list "g b" )))))

(define-configuration web-buffer
  ((default-modes (append '(vi-normal-mode) %slot-value%))
   (search-auto-complete-p nil)
   (search-always-auto-complete-p nil)
   ;; (search-engines (append %slot-value%
   ;;                         (mapcar (lambda (engine) (apply 'make-search-engine engine))
   ;;                                 *search-engines*)))
   (request-resource-hook
    (hooks:add-hook %slot-value%
                    (url-dispatching-handler
                     'mpv-youtube-links
                     (match-regex "youtube\.com\/watch")
                     (lambda (url)
                       (uiop:run-program (list "mpv" (render-url url)))))))))

