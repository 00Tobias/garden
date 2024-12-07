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
        (name 'guix-science-nonfree)
        (url "https://github.com/guix-science/guix-science-nonfree.git")
        (introduction
         (make-channel-introduction
          "58661b110325fd5d9b40e6f0177cc486a615817e"
          (openpgp-fingerprint
           "CA4F 8CF4 37D7 478F DA05  5FD4 4213 7701 1A37 8446"))))
       (channel
        (name 'guix-gaming-games)
        (url "https://gitlab.com/guix-gaming-channels/games.git")
        (introduction
         (make-channel-introduction
          "c23d64f1b8cc086659f8781b27ab6c7314c5cca5"
          (openpgp-fingerprint
           "50F3 3E2E 5B0C 3D90 0424  ABE8 9BDC F497 A4BB CC7F"))))
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
