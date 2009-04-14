;;
;; advanced demo.
;;
;; transported from JAWJAW:main.java.edu.cmu.lti.jawjaw.demo.AdvancedDemo
;; by naoya_t
;;
(use srfi-1)
(use gauche.charconv)
(use lang.wordnet-ja)

(define (run word pos)
  ;; 日本語 WordNet から情報を抽出
  (let* ([word (car-or-false (find-words-by-lemma-and-pos word pos))]
		 [sense (car-or-false (find-senses-by-wordid (word-wordid word)))]
		 [synset-id (sense-synset sense)]
		 [synset (find-synset-by-synset synset-id)]
		 [synset-def (find-synsetdef-by-synset-and-lang synset-id 'eng)]
		 [synlinks (find-synlinks-by-synset synset-id)])
	;; 結果表示（多義語はごっちゃになっています）
	(print word)
	(print sense)
	(print synset)
	(print synset-def)
	(print synlinks)
	))

(define (main args)
  (if (= 3 (length args))
      (let ([word (ces-convert (second args) "*jp" "utf-8")]
			[pos (string->symbol (third args))])
		(wordnet-ja-initialize)
		(run word pos)
		(newline))
	  (print "usage: advanced-demo.scm word pos"))
  0)

;; "自然言語処理"(名詞)という単語から得られる関係の一部をデモします
;(run "自然言語処理" 'n)
