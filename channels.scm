(define-module (channels)
  #:use-module (guix channels))

(cons* (channel
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
       %default-channels)
