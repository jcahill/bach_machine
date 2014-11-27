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
           
           (measure-6 (car (shuffle '("e8 c' g e c2"
                                      "e8 f d e c2"
                                      "e8 f e d c2"
                                      "e4 d8 e c2"
                                      "e8 d e d c2"
                                      "e8 f g e c2"
                                      "e8 g e d c2"
                                      "e8 c e d c2"
                                      "e4. d8 c2")))))
             
      (string-join (list measure-1 measure-2 measure-3
                         measure-4 measure-5 measure-6)))))

(define voice-2
  (lambda ()
    (let* ((measure-1 (car (shuffle '("r2 c2~"
                                      "c,2 c'~"
                                      "c4 e8 d c4 c~"
                                      "c2. c4~"
                                      "r2 c4 c~"
                                      "c8 c, e g c4 c~"
                                      "c,4 e8 g c4 c~"
                                      "c1~"
                                      "r4 c,8 d c4 c'~"))))
           
           (measure-2 (car (shuffle '("c4 b8 a b4 g"
                                      "c8 c b a c b a g"
                                      "c4 b e8 d c b"
                                      "c4 b8 a b c d e"
                                      "c4 c b e"
                                      "c4 c2 b4"
                                      "c4 b8 a b d b g"
                                      "c4 c b b"))))
           
           (measure-3-notes (car (shuffle '("a2. aes4"
                                            "a2 d8 c b a"
                                            "a2~ a8 c b a"
                                            "a4 a2 d4"
                                            "a4 a~ a8 a g f"
                                            "a2. d4"
                                            "a2 d,8 f e d"
                                            "a2~ a8 b c d8"
                                            "a4 a a8 c a f"))))
           
           (measure-3 (if (eq? #\e (last-character measure-2))
                          (string-replace measure-3-notes "a" "a," #:all? #f)
                          measure-3-notes))
           
           (measure-4-notes (car (shuffle '("g4 c8 b c2~"
                                            "g8 g c b c4 c~"
                                            "g4 g c c~"
                                            "g8 d' c b c4 c~"
                                            "g8 g a b c2~"
                                            "g4 g c2~"
                                            "g2 c4 c~"
                                            "g4 a8 b c2~"
                                            "g2 c,4 c'~"))))
           
           (measure-4 (if (and (member #\d (string->list measure-3))
                               (or (eq? (last-character measure-3) #\4)
                                   (eq? (last-character measure-3) #\8)))
                          (string-replace measure-4-notes "g" "g," #:all? #f)
                          measure-4-notes))
           
           (measure-5 (car (shuffle '("c4 b8 a b2"
                                      "c8 c b a b b a b"
                                      "c2 b4 a8 b"
                                      "c2 b"
                                      "c4 b2 a8 b"
                                      "c4 c b b"
                                      "c8 a b c b c a b"
                                      "c4 b~ b8 a b4"
                                      "c4 b8 a b4 a8 b")))))
      
      (string-join (list measure-1 measure-2 measure-3 measure-4 measure-5 "c1")))))
           
(define lilypond-template #<<END
#(set-global-staff-size 19) 

\header {
  title = "Bach Machine"
  composer = "C.P.E. Bach"
  }

\paper {
  left-margin = 0\cm
  top-margin = 5\cm
}

  upper = \relative c'' {
  \clef treble
  \key c \major
  \time 2/2

  TOP VOICE GOES HERE
}

lower = \relative c' {
  \clef bass
  \key c \major
  \time 2/2

  BOTTOM VOICE GOES HERE
}

\score {
  \new PianoStaff <<
    \set PianoStaff.instrumentName = #"Piano  "
    \new Staff = "upper" { \upper }
    \new Staff = "lower" { \lower }
  >>
  \layout { }
  \midi { }
} 
END
  )

(define lilypond-score
  (lambda()
  (write-string (string-replace (string-replace lilypond-template "BOTTOM VOICE GOES HERE" (voice-2)) "TOP VOICE GOES HERE" (voice-1)))))