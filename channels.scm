(define-module (channels)
  #:use-module (guix channels))

(cons* (channel
        (name 'incognita)
        (url "https://codeberg.org/terra/incognita.git")
        (introduction
         (make-channel-introduction
          "bd1a7531db92fd2fc98d3c90882b5ae259ac6604"
          (openpgp-fingerprint
           "4925 08B3 25EA 2DB3 7D05  9595 72F0 61B8 1813 165A"))))
       (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (introduction
         (make-channel-introduction
          "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
          (openpgp-fingerprint
           "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
       (channel
        (name 'saayix)
        (url "https://codeberg.org/look/saayix.git")
        (branch "entropy")
        (introduction
         (make-channel-introduction
          "12540f593092e9a177eb8a974a57bb4892327752"
          (openpgp-fingerprint
           "3FFA 7335 973E 0A49 47FC  0A8C 38D5 96BE 07D3 34AB"))))
       (channel
        (name 'rde)
        (url "https://git.sr.ht/~abcdw/rde")
        (introduction
         (make-channel-introduction
          "257cebd587b66e4d865b3537a9a88cccd7107c95"
          (openpgp-fingerprint
           "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
       (channel
        (name 'emacs-master)
        (url "https://github.com/gs-101/emacs-master.git")
        (branch "main")
        (introduction
         (make-channel-introduction
          "568579841d0ca41a9d222a2cfcad9a7367f9073b"
          (openpgp-fingerprint
           "3049 BF6C 0829 94E4 38ED  4A15 3033 E0E9 F7E2 5FE4"))))
       %default-channels)
