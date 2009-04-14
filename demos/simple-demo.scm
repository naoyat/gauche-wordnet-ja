;;
;; simple demo.
;;
;; transported from JAWJAW:main.java.edu.cmu.lti.jawjaw.demo.SimpleDemo
;; by naoya_t
;;
(use srfi-1)
(use gauche.charconv)
(use lang.wordnet-ja)

(define (run word pos)
;  (format #t "lang of ~a : \t~a\n"
;		  word (find-lang word))
;  (format #t "synsets of ~a : \t~a\n"
;		  word (word->synsets word pos))
  (format #t "hypernyms of ~a : \t~a\n"
		  word (find-hypernyms word pos))
  (format #t "hyponyms of ~a : \t~a\n"
		  word (find-hyponyms word pos))
  (format #t "~a entails : \t\t~a\n"
		  word (find-entailments word pos))
  (format #t "translations of ~a : \t~a\n"
		  word (find-translations word pos))
  (format #t "definitions of ~a : \t~a\n"
		  word (find-definitions word pos)) )

(define (main args)
  (if (= 3 (length args))
      (let ([word (ces-convert (second args) "*jp" "utf-8")]
			[pos (string->symbol (third args))])
		(wordnet-ja-initialize)
		(run word pos)
		(newline))
	  (print "usage: simple-demo.scm word pos"))
  0)

;(run "買収" 'v)
