;;
;; wn.scm
;;
;; transported from yanbe's wn.py
;; by naoya_t
;;
(use srfi-1)
(use gauche.charconv)
(use lang.wordnet-ja)

(define (show-usage)
  (print "usage: wn.scm word link [lang]
    word
      word to investigate

    link
      syns - Synonyms
      hype - Hypernyms
      inst - Instances
      hypo - Hyponym
      hasi - Has Instance
      mero - Meronyms
      mmem - Meronyms --- Member
      msub - Meronyms --- Substance
      mprt - Meronyms --- Part
      holo - Holonyms
      hmem - Holonyms --- Member
      hsub - Holonyms --- Substance
      hprt - Holonyms -- Part
      attr - Attributes
      sim - Similar to
      entag - Entails
      causg - Causes
      dmncg - Domain --- Category
      dmnug - Domain --- Usage
      dmnrg - Domain --- Region
      dmtcg - In Domain --- Category
      dmtug - In Domain --- Usage
      dmtrg - In Domain --- Region
      antsg - Antonyms

    lang (default: jpn)
      jpn - Japanese
      eng - English"))

(define (get-synlinks-recursive senses link . args)
  (let-optionals* args ((lang 'jpn) (depth 0))
    (for-each (lambda (sense)
                (let* ([wordid (sense-wordid sense)]
                       [synset-id (sense-synset sense)]
                       [synlinks (find-synlinks-by-synset-and-link synset-id link)])
                  (unless (null? synlinks)
                    (format #t "~a~a ~a\n"
                            (make-string (* depth 2) #\Space)
                            (word-lemma (find-word-by-wordid wordid))
                            (synset-name (find-synset-by-synset synset-id))))
                  (let1 _senses (append-map (lambda (synlink)
                                              (find-senses-by-synset-and-lang (synlink-synset2 synlink) lang))
                                            synlinks)
                    (get-synlinks-recursive _senses link lang (+ depth 1)))
                  ))
              senses)))

(define (main args)
  (wordnet-ja-initialize)
  (if (<= 3 (length args))
      (let* ([input-word (ces-convert (second args) "*jp" "utf-8")]
             [word (car-or-false (find-words-by-lemma input-word))])
        (if word
            (let* ([senses (find-senses-by-wordid (word-wordid word))]
                   [link (third args)]
                   [lang (if (= 4 (length args)) (fourth args) 'jpn)])
              ;;(format #t "~a => { words: ~a, link: ~a, lang: ~a }\n" input-word words link lang)
              (get-synlinks-recursive senses link lang))
            (print "(nothing found)"))) ; >>sys.stderr
      (show-usage))
  0)
