; *********************************************
; *  314 Principles of Programming Languages  *
; *  Fall 2017                                *
; *  Author: Ulrich Kremer                    *
; *********************************************
;; -----------------------------------------------------
;; ENVIRONMENT
;; contains "ltv", "vtl",and "reduce" definitions
;;#lang racket
(load "include.ss")

;; contains a test document consisting of three paragraphs. 
(load "document.ss")

;; contains a test-dictionary, which has a much smaller dictionary for testing
;; the dictionary is needed for spell checking
;;(load "test-dictionary.ss")

(load "dictionary.ss") ;; the real thing with 45,000 words


;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; *** CODE FOR ANY HELPER FUNCTION GOES HERE ***

;;Breaks down paragraphs into words
(define parHandler
	(lambda (par encoder)
		(map (lambda (k) (encoder k)) par) ;;For each word, apply the provided encoder
	)
)

;;Loops through all possible values of n until it finds a match
(define nloop
 (lambda (par n numWords)
  (cond 
   ((> (try (parHandler par (encode-n n))) (quotient numWords 2)) n);;More than half of words are spelled correctly for this 'n
   ((> n 26) -1) ;;Could not find 'n
   (else (nloop par (+ n 1) numWords)) ;;Try next 'n
  )
 )
)

(define nloopB
 (lambda (par n)
  (cond
   ((validOccurences (countLetters (flatten (parHandler par (encode-n n)))0))n) ;;e-t-a are most frequent in that order for this 'n
   ((> n 26) -1) ;;Could not find 'n
   (else (nloopB par (+ n 1)))
  )
 )
)

;;This version checks if the letters e-t-a occur most frequently in this order, making it a tighter restriction and may not be the way the professor inteded
;(define validOccurences
; (lambda (nums)
;  (cond
;   ((and (eCheck nums) (tCheck nums) (aCheck nums))#t) ;;e-t-a are in valid occurence order
;   (else #f)
;  )
; )
;)

(define validOccurences
 (lambda (nums)
  (cond
   ((eCheck nums)#t) ;;e is most frequent letter
   (else #f)
  )
 )
)


;;Checks if e is most common
(define eCheck
 (lambda (nums)
  (cond
   ((and (equal? (car (sort nums >)) (list-ref nums 4)) (not(equal? (list-ref nums 0)(list-ref nums 1)))) #t)
   (else #f)
  )
 )
)
;;Check if t is second most common
(define tCheck
 (lambda (nums)
  (cond
   ((and (equal? (list-ref (sort nums >) 1) (list-ref nums 19)) (not(equal? (list-ref (sort nums >) 1)(list-ref (sort nums >) 2)))) #t)
   (else #f)
  )
 )
)
;;Check if a is third most common
(define aCheck
 (lambda (nums)
  (cond
   ((and (equal? (list-ref (sort nums >) 2) (list-ref nums 0)) (not(equal? (list-ref (sort nums >) 2)(list-ref (sort nums >) 3)))) #t)
   (else #f)
  )
 )
)



(define countLetters
 (lambda (fpar char)
  (cond
   ((> char 25) empty)
   (else (append (list (numOfOcc fpar char)) (countLetters fpar (+ 1 char))))
  )
 )
)

(define numOfOcc
 (lambda (fpar char)
  (cond
   ((null? fpar) 0)
   ((equal? (ltv (car fpar)) char) (+ 1 (numOfOcc (cdr fpar) char)))
   (else (+ 0 (numOfOcc (cdr fpar) char)))
  )
 )
)

;;Returns the number of proper words for a given n
(define try
 (lambda (par)
  (cond
   ((null? par) 0)
   ((spell-checker (car par)) (+ 1 (try (cdr par))))
   (else (+ 0 (try (cdr par))))
  )
 )
)

(define wordCount 
 (lambda(x)
  (cond ((null? x)0)
   (else (+ 1 (wordCount(cdr x))))
  )
 )
)

(define letterCount
 (lambda (x)
  (cond ((null? x)0)
   (else (+ (numLetters (car x)) (letterCount(cdr x))))
  )
 )
)

(define numLetters
 (lambda (x)
  (cond ((null? x)0)
   (else (+ 1 (numLetters (cdr x))))
  )
 )
)

(define flatten
 (lambda (p)
  (cond 
   ((null? p) empty)
   (else (append (car p) (flatten (cdr p))))
  )
 )
)

;; -----------------------------------------------------
;; SPELL CHECKER FUNCTION

;;check a word's spell correctness
;;INPUT:a word(a global variable "dictionary" is included in the file "test-dictionary.ss", and can be used directly here)
;;OUTPUT:true(#t) or false(#f)
(define spell-checker 
  (lambda (w)
	(if (member w dictionary) #t #f)
   ))

;; -----------------------------------------------------
;; ENCODING FUNCTIONS

;;generate an Caesar Cipher single word encoders
;;INPUT:a number "n"
;;OUTPUT:a function, whose input is a word, and output is the encoded word
(define encode-n
  (lambda (n);;"n" is the distance, eg. n=3: a->d,b->e,...z->c
    (lambda (w);;"w" is the word to be encoded
		(map (lambda (k) (vtl (modulo (+ n (ltv k)) 26))) w) ;;For each letter in the word, convert it to a value, add n to it, modulo the result by 26 so it wraps around, and convert it back to a letter
	)))

;;encode a document
;;INPUT: a document "d" and a "encoder"
;;OUTPUT: an encoded document using a provided encoder
(define encode-d;;this encoder is supposed to be the output of "encode-n"
  (lambda (d encoder)
   (map (lambda (k) (parHandler k encoder)) d) ;;For each paragraph, pass it on to the paragraph handler
    ))
    
;; -----------------------------------------------------
;; DECODE FUNCTION GENERATORS
;; 2 generators should be implemented, and each of them returns a decoder

;;generate a decoder using brute-force-version spell-checker
;;INPUT:an encoded paragraph "p"
;;OUTPUT:a decoder, whose input=a word, output=decoded word
(define Gen-Decoder-A
  (lambda (p)
   (encode-n (nloop p 0 (wordCount p)))
  ))

;;generate a decoder using frequency analysis
;;INPUT:same as above
;;OUTPUT:same as above
(define Gen-Decoder-B
  (lambda (p)
   (encode-n (nloopB p 0))
  ))

;; -----------------------------------------------------
;; CODE-BREAKER FUNCTION

;;a codebreaker
;;INPUT: an encoded document(of course by a Caesar's Cipher), a decoder(generated by functions above)
;;OUTPUT: a decoded document
(define Code-Breaker
  (lambda (d decoder)
	(encode-d d decoder) 
  )
)

;; -----------------------------------------------------
;; EXAMPLE APPLICATIONS OF FUNCTIONS
;;(spell-checker '(h e l l o))
;;(define add5 (encode-n 5))
;;(encode-d document add5)
;;(define decoderSP1 (Gen-Decoder-A paragraph))
;;(define decoderFA1 (Gen-Decoder-B paragraph))
;;(Code-Breaker document decoderSP1)
