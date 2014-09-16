#lang racket

(define last-character
  (lambda (s)
    (string-ref s (- (string-length s) 1))))

(define first-character
  (lambda (s)
    (string-ref s 0)))

(define voice-1
  (lambda ()
    (let* ((measure-1 (car (shuffle '("r8 e g f e g e c" 
                                      "e2. c4" 
                                      "c4 g e' c" 
                                      "e4 g e c" 
                                      "r4 c8 d e4 c" 
                                      "e8 f g f e d e c" 
                                      "c2 e" 
                                      "r8 c c d e e d c" 
                                      "c4 c8 d e d e c"))))
           
           (measure-2-notes (car (shuffle '("d4 g, g'2~"
                                            "g8 a g fis g2~"
                                            "d8 g, g' fis g2~"
                                            "d4 d g2~"
                                            "d4 g8 fis g2~"
                                            "d2 g~"
                                            "g8 g, g' g g2~"
                                            "d8 d g fis g2~"
                                            "g4 g, g'2~"))))
           
           (measure-2 (if (and (eq? (first-character measure-2-notes) #\g)
                               (or (eq? (last-character measure-1) #\c)
                                   (eq? (last-character measure-1) #\4)))
                          (string-replace measure-2-notes "g" "g'" #:all? #f)
                          measure-2-notes)))
             
      (string-join (list measure-1 measure-2)))))
