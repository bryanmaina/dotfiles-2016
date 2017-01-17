;;; on a belgian azerty keyboard, in order to type
;; "~", "´" or "`" you have to use 3 keystrokes instead
;; of 2 keystrokes on a querty keyboard. This make my life
;; easier while working with TidalCycles
(define-key key-translation-map (kbd "§") (kbd "~"))
(define-key key-translation-map (kbd "~") (kbd "§"))
(define-key key-translation-map (kbd "ù") (kbd "´"))
(define-key key-translation-map (kbd "´") (kbd "ù"))
(define-key key-translation-map (kbd "µ") (kbd "`"))
(define-key key-translation-map (kbd "`") (kbd "µ"))
