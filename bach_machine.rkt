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
                          measure-2-notes))
           
           (measure-3 (car (shuffle '("g8 a b c f,2~"
                                      "g4 c, f2~"
                                      "g8 c, d e f4 f~"
                                      "g8 c, b c f2~"
                                      "g8 g c g f4 f~"
                                      "g2 f~"
                                      "g8 g f e f2~"
                                      "g8 e d c f4 f~"
                                      "g8 c, f e f2~"))))
           
           (measure-4 (car (shuffle '("f4 f e8 c a' c,"
                                      "f4 e8 d e4 c"
                                      "f8 f e d e e d c"
                                      "f4 e8 d e g e c"
                                      "f4 f f8 e d c"
                                      "f4. f8 e d e c"
                                      "f4 f2 e4"
                                      "f4 f e a"
                                      "f8 b, c d e g f e"))))
           
           (measure-5-notes (car (shuffle '("d4 g8 a g a f g"
                                            "d2. c8 d"
                                            "d4 g8 a g4. f8"
                                            "d2 d"
                                            "d1"
                                            "d4 g2 f8 g"
                                            "d4 d8 c d e f4"
                                            "d4 g, g' f"
                                            "d4 g g, f'"))))
           
           (measure-5 (if (eq? (last-character measure-4) #\a)
                          (string-replace measure-5-notes "d" "d," #:all? #f)
                          measure-5-notes))
           
             
      (string-join (list measure-1 measure-2 measure-3)))))
