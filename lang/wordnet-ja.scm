;;
;; gauche-wordnet-ja
;; by naoya_t
;;
(define-module lang.wordnet-ja
  (export wordnet-ja-initialize
          car-or-false

          make-linkdef linkdef?
          linkdef-link linkdef-lang linkdef-def

          make-sense sense?
          sense-synset sense-wordid sense-lang sense-rank sense-lexid sense-freq sense-src

          make-synset synset?
          synset-synset synset-pos synset-name synset-src

          make-synsetex synsetex?
          synsetex-synset synsetex-lang synsetex-def synsetex-sid

          make-word word?
          word-wordid word-lang word-lemma word-pron word-pos

          make-posdef posdef?
          posdef-pos posdef-lang posdef-def

          make-synlink synlink?
          synlink-synset1 synlink-synset2 synlink-link synlink-src

          make-synsetdef synsetdef?
          synsetdef-synset synsetdef-lang synsetdef-def synsetdef-sid

          make-variant variant?
          variant-varid variant-wordid variant-lang variant-lemma variant-vartype

          find-words-by-lemma find-words-by-lemma-and-pos find-word-by-wordid
          find-senses-by-synset find-senses-by-wordid find-senses-by-synset-and-lang
          find-synsetdef-by-synset-and-lang
          find-synlinks-by-synset find-synlinks-by-synset-and-link
          find-synset-by-synset find-synsets-by-name find-synsets-by-name-and-pos

          ;; JAWJAW
          ;; find-links find-lang word->synsets
          find-hypernyms
          find-hyponyms
          find-meronyms
          find-holonyms
          find-instances
          find-has-instances
          find-attributes
          find-similar-to
          find-entailments
          find-causes
          find-see-also
          find-synonyms
          find-antonyms
          find-domains
          find-in-domains
          find-translations
          find-definitions

          ;; compatibility with yanbe's
          get-words get-word get-senses get-sense get-synset get-synlinks
          ))

(select-module lang.wordnet-ja)

(use srfi-1)
(use dbi)
(use dbd.sqlite3)
(use gauche.collection)
;;(use util.relation)

(define wnjpn-dbfile "wnjpn-0.9.db")

;; link_def | sense | synset | synset_ex | word | pos_def | synlink | synset_def | variant
(define *conn* #f)

(define *find-word-by-lemma* #f)
(define *find-word-by-lemma-and-pos* #f)
(define *find-word-by-wordid* #f)

(define *find-senses-by-synset* #f)
(define *find-senses-by-wordid* #f)
(define *find-senses-by-synset-and-lang* #f)

(define *find-synsetdef-by-synset-and-lang* #f)

(define *find-synlink-by-synset* #f)
(define *find-synlink-by-synset-and-link* #f)
(define *find-synlink-by-synset-and-links* #f)

(define *find-synset-by-synset* #f)
(define *find-synsets-by-name* #f)
(define *find-synsets-by-name-and-pos* #f)

(define (wordnet-ja-initialize); dbfile)
  (set! *conn* (dbi-connect #`"dbi:sqlite3:db=,|wnjpn-dbfile|"))

  (set! *find-word-by-lemma*
        (dbi-prepare *conn* "SELECT * FROM word WHERE lemma=?"))
  (set! *find-word-by-lemma-and-pos*
        (dbi-prepare *conn* "SELECT * FROM word WHERE lemma=? AND pos=?"))
  (set! *find-word-by-wordid*
        (dbi-prepare *conn* "SELECT * FROM word WHERE wordid=?"))

  (set! *find-senses-by-synset*
        (dbi-prepare *conn* "SELECT * FROM sense WHERE synset=?"))
  (set! *find-senses-by-wordid*
        (dbi-prepare *conn* "SELECT * FROM sense WHERE wordid=?"))
  (set! *find-senses-by-synset-and-lang*
        (dbi-prepare *conn* "SELECT * FROM sense WHERE synset=? AND lang=?"))

  (set! *find-synsetdef-by-synset-and-lang*
        (dbi-prepare *conn* "SELECT * FROM synset_def WHERE synset=? AND lang=?"))

  (set! *find-synlink-by-synset*
        (dbi-prepare *conn* "SELECT * FROM synlink WHERE synset1=?"))
  (set! *find-synlink-by-synset-and-link*
        (dbi-prepare *conn* "SELECT * FROM synlink WHERE synset1=? AND link=?"))
  (set! *find-synlink-by-synset-and-links*
        (dbi-prepare *conn* "SELECT * FROM synlink WHERE synset1=? AND link IN (?,?,?)"))

  (set! *find-synset-by-synset*
        (dbi-prepare *conn* "SELECT * FROM synset WHERE synset=?"))
  (set! *find-synsets-by-name*
        (dbi-prepare *conn* "SELECT * FROM synset WHERE name=?"))
  (set! *find-synsets-by-name-and-pos*
        (dbi-prepare *conn* "SELECT * FROM synset WHERE name=? AND pos=?"))
  )

(define-macro (execute-prepared-sql* prepared-sql maker-proc . params)
  `(let* ([r (dbi-execute ,prepared-sql ,@params)]
          [getter (relation-accessor r)])
     (map (lambda (row) (,maker-proc getter row)) r)))

;;
;; utils
;;
(define (tag-with tag content) (cons tag content))
(define (tagged-with? tag obj) (and (list? obj) (eq? tag (car obj))))

(define (>>> obj) ; turn into symbol if string
  (if (and obj (string? obj)) (string->symbol obj) obj))

(define (=>int val) (if val (x->integer val) 0))

(define (car-or-false lis) ; #f if the list is (), otherwise its car
  (if (null? lis) #f (car lis)))

;;;
;;; records
;;;

;;
;; link_def (25): link:text lang:text def:text
;;
(define (make-linkdef link lang def)
  (tag-with :linkdef (list link lang def)))
(define (linkdef? obj) (tagged-with? :linkdef obj))

(define (linkdef-link linkdef) (second linkdef))
(define (linkdef-lang linkdef) (third linkdef))
(define (linkdef-def linkdef) (fourth linkdef))

(define (=>linkdef getter row)
  (make-linkdef (>>> (getter row "link"))
                (>>> (getter row "lang"))
                (getter row "def")))

;;
;; sense (363625): synset:text wordid:int lang:text rank:text lexid:int freq:int src:text
;;
(define (make-sense synset wordid lang rank lexid freq src)
  (tag-with :sense (list synset wordid lang rank lexid freq src)))
(define (sense? obj) (tagged-with? :sense obj))

(define (sense-synset sense) (second sense))
(define (sense-wordid sense) (third sense))
(define (sense-lang sense) (fourth sense))
(define (sense-rank sense) (fifth sense))
(define (sense-lexid sense) (sixth sense))
(define (sense-freq sense) (seventh sense))
(define (sense-src sense) (eighth sense))

(define (=>sense getter row)
  (make-sense (>>> (getter row "synset"))
              (=>int (getter row "wordid"))
              (>>> (getter row "lang"))
              (=>int (getter row "rank"))
              (=>int (getter row "lexid"))
              (=>int (getter row "freq"))
              (>>> (getter row "src"))))

;;
;; synset (117659): synset:text pos:text name:text src:text
;;
(define (make-synset synset pos name src)
  (tag-with :synset (list synset pos name src)))
(define (synset? obj) (tagged-with? :synset obj))

(define (synset-synset synset) (second synset))
(define (synset-pos synset) (third synset))
(define (synset-name synset) (fourth synset))
(define (synset-src synset) (fifth synset))

(define (=>synset getter row)
  (make-synset (>>> (getter row "synset"))
               (>>> (getter row "pos"))
               (getter row "name")
               (>>> (getter row "src"))))

;;
;; synset_ex (0): synset:text lang:text def:text sid:text
;;
(define (make-synsetex synset lang def sid)
  (tag-with :synsetex (list synset lang def sid)))
(define (synsetex? obj) (tagged-with? :synsetex obj))

(define (synsetex-synset synsetex) (second synsetex))
(define (synsetex-lang synsetex) (third synsetex))
(define (synsetex-def synsetex) (fourth synsetex))
(define (synsetex-sid synsetex) (fifth synsetex))

(define (=>synsetex getter row)
  (make-synsetex (getter row "synset")
                 (getter row "lang")
                 (getter row "def")
                 (getter row "sid")))

;;
;; word (241253): wordid:int, lang, lemma, pron, pos
;;
(define (make-word wordid lang lemma pron pos)
  (tag-with :word (list wordid lang lemma pron pos)))
(define (word? obj) (tagged-with? :word obj))

(define (word-wordid word) (second word))
(define (word-lang word) (third word))
(define (word-lemma word) (fourth word))
(define (word-pron word) (fifth word))
(define (word-pos word) (sixth word))

(define (=>word getter row)
  (make-word (=>int (getter row "wordid"))
             (>>> (getter row "lang"))
             (getter row "lemma")
             (getter row "pron")
             (>>> (getter row "pos"))
             ))

;;
;; pos_def (8): pos:text, lang:text, def:text
;;
(define (make-posdef pos lang def)
  (tag-with :posdef (list pos lang def)))
(define (posdef? obj) (tagged-with? :posdef obj))

(define (posdef-pos posdef) (second posdef))
(define (posdef-lang posdef) (third posdef))
(define (posdef-def posdef) (fourth posdef))

(define (=>posdef getter row)
  (make-posdef (>>> (getter row "pos"))
               (>>> (getter row "lang"))
               (getter row "def")))

;;
;; synlink (283600) : synset1:text, synset2:text, link:text, src:text
;;
(define (make-synlink synset1 synset2 link src) ; synlink (283600)
  (tag-with :synlink (list synset1 synset2 link src)))
(define (synlink? obj) (tagged-with? :synlink obj))

(define (synlink-synset1 synlink) (second synlink))
(define (synlink-synset2 synlink) (third synlink))
(define (synlink-link synlink) (fourth synlink))
(define (synlink-src synlink) (fifth synlink))

(define (=>synlink getter row)
  (make-synlink (>>> (getter row "synset1"))
                (>>> (getter row "synset2"))
                (>>> (getter row "link"))
                (>>> (getter row "src"))))

;;
;; synset_def (118529) : synset:text lang:text def:text sid:text
;;
(define (make-synsetdef synset lang def sid)
  (tag-with :synsetdef (list synset lang def sid)))
(define (synsetdef? obj) (tagged-with? :synsetdef obj))

(define (synsetdef-synset synsetdef) (second synsetdef))
(define (synsetdef-lang synsetdef) (third synsetdef))
(define (synsetdef-def synsetdef) (fourth synsetdef))
(define (synsetdef-sid synsetdef) (fifth synsetdef))

(define (=>synsetdef getter row)
  (make-synsetdef (>>> (getter row "synset"))
                   (>>> (getter row "lang"))
                   (getter row "def")
                   (=>int (getter row "sid"))))

;;
;; variant (0) : varid:int wordid:int lang:text lemma:text vartype:text
;;
(define (make-variant varid wordid lang lemma vartype)
  (tag-with :variant (list varid wordid lang lemma vartype)))
(define (variant? obj) (tagged-with? :variant obj))

(define (variant-varid variant) (second variant))
(define (variant-wordid variant) (third variant))
(define (variant-lang variant) (fourth variant))
(define (variant-lemma variant) (fifth variant))
(define (variant-vartype variant) (sixth variant))

(define (=>variant getter row)
  (make-variant (getter row "varid")
                (getter row "wordid")
                (>>> (getter row "lang"))
                (getter row "lemma")
                (>>> (getter row "vartype"))))

;;;
;;; APIs
;;;

;; Find words by lemma
(define (find-words-by-lemma lemma)
  ;; lemma ... can be either in English or Japanese.
  (execute-prepared-sql* *find-word-by-lemma* =>word lemma))

;; Find words by lemma and POS.
(define (find-words-by-lemma-and-pos lemma pos)
  ;; lemma ... cannonical form of the word. can be either in English or Japanese.
  ;; pos ..... POS of the lemma
  (execute-prepared-sql* *find-word-by-lemma-and-pos* =>word lemma pos))

;; Find word by word-id
(define (find-word-by-wordid wordid)
  (car-or-false (execute-prepared-sql* *find-word-by-wordid* =>word wordid)))

;; Find sense records by synset (one-to-many relationship)
(define (find-senses-by-synset synset)
  (execute-prepared-sql* *find-senses-by-synset* =>sense synset))

;; Find sense records by word id (one-to-many relationship)
(define (find-senses-by-wordid wordid)
  (execute-prepared-sql* *find-senses-by-wordid* =>sense wordid))

;; Find sense records by synset and language (one-to-many relationship)
(define (find-senses-by-synset-and-lang synset lang)
  (execute-prepared-sql* *find-senses-by-synset-and-lang* =>sense synset lang))

;; Find synset definition record by synset and lang
(define (find-synsetdef-by-synset-and-lang synset lang)  ;; lang: jpn, eng
  (car-or-false (execute-prepared-sql* *find-synsetdef-by-synset-and-lang* =>synsetdef synset lang)))

;; Find synlink records by synset (one-to-many relationship)
(define (find-synlinks-by-synset synset)
  (execute-prepared-sql* *find-synlink-by-synset* =>synlink synset))

;; Find synlink records by synset and link (one-to-many relationship)
(define (find-synlinks-by-synset-and-link synset link)
  ;; synset ... first argument of a relationship
  ;; link ..... lexical relationship
  (if (list? link)
	  (execute-prepared-sql* *find-synlink-by-synset-and-links* =>synlink synset (first links) (second links) (third links))
	  (execute-prepared-sql* *find-synlink-by-synset-and-link* =>synlink synset link) ))

;; Find synset record by synset id key
(define (find-synset-by-synset synset)
  (car-or-false (execute-prepared-sql* *find-synset-by-synset* =>synset synset)))

;; Find synset records by name.
;;   This method is deprecated since synset only has a name in English.
;;   Start from word record instead.
(define (find-synsets-by-name name) ;deprecated
  ;; name ... label on a synset
  (execute-prepared-sql* *find-synsets-by-name* =>synset name))

;; Find synset records by name and POS.
;;   This method is deprecated since synset only has a name in English.
;;   Start from word record instead.
(define (find-synsets-by-name-and-pos name pos) ;deprecated
  (execute-prepared-sql* *find-synsets-by-name-and-pos* =>synset name pos))


;;;
;;; from JAWJAW
;;;
;;;   word ... word in English or Japanese
;;;   pos .... part of speech
;;;   link ... lexical relationship
;;;
(define (find-links-proc link)
  (lambda (word pos)
	(let1 lang (find-lang word)
	  (append-map (lambda (synset)
					(append-map (lambda (synlink)
								  (map (lambda (sense) (word-lemma (find-word-by-wordid (sense-wordid sense))))
									   (find-senses-by-synset-and-lang (synlink-synset2 synlink) lang)))
								(find-synlinks-by-synset-and-link synset link)))
				  (word->synsets word pos)))))

(define (find-lang word)
  (let1 word (car-or-false (find-words-by-lemma word))
    (if word (word-lang word) 'jpn))) ;default = jpn

(define (word->synsets word pos)
  (append-map (lambda (word) (map sense-synset (find-senses-by-wordid (word-wordid word))))
              (find-words-by-lemma-and-pos word pos)))

;; Finds hypernyms of a word. According to wikipedia,
;;  * (Noun) hypernyms: Y is a hypernym of X if every X is a (kind of) Y (canine is a hypernym of dog)
;;  * (Verb) hypernym: the verb Y is a hypernym of the verb X if the activity X is a (kind of) Y (travel is an hypernym of movement)
(define find-hypernyms (find-links-proc 'hype))

;; Finds hyponyms of a word. According to wikipedia,
;;  * (Noun) hyponyms: Y is a hyponym of X if every Y is a (kind of) X (dog is a hyponym of canine)
(define find-hyponyms (find-links-proc 'hypo))

;; Finds meronyms of a word. According to wikipedia,
;;  * (Noun) meronym: Y is a meronym of X if Y is a part of X (window is a meronym of building)
(define find-meronyms (find-links-proc '(mmem msub mprt)))

;; Finds holonyms of a word. According to wikipedia,
;;  * (Noun) holonym: Y is a holonym of X if X is a part of Y (building is a holonym of window)
(define find-holonyms (find-links-proc '(hmem hsub hprt)))

;; Finds instances of a word.
(define find-instances (find-links-proc 'inst))

;; Finds has-instance relations of a word.
(define find-has-instances (find-links-proc 'hasi))

;; Get attributes of a word.
(define find-attributes (find-links-proc 'attr))

;; Finds similar-to relations of an adjective(?).
(define find-similar-to (find-links-proc 'sim))

;; Finds entailed consequents of a word. According to wikipedia,
;;  * (Verb) entailment: the verb Y is entailed by X if by doing X you must be doing Y (to sleep is entailed by to snore)
(define find-entailments (find-links-proc 'enta))

;; Finds causes of a word.
(define find-causes (find-links-proc 'caus))

;; Finds words in see also relationship.
(define find-see-also (find-links-proc 'also))

;; Find synonyms of a word.
(define find-synonyms (find-links-proc 'syns))

;; Find antonyms of a word.
(define find-antonyms (find-links-proc 'ants))

;; Get domains of a word.
(define find-domains (find-links-proc '(dmnc dmnr dmnu)))

;; Get in-domain relations of a word.
(define find-in-domains (find-links-proc '(dmtc cmtr dmtu)))

;; Finds translations of a word.
(define (find-translations word pos)
  (let1 another-lang (if (eq? 'jpn (find-lang word)) 'eng 'jpn)
    (append-map (lambda (synset)
                  (map (lambda (sense) (word-lemma (find-word-by-wordid (sense-wordid sense))))
                       (find-senses-by-synset-and-lang synset another-lang)))
                (word->synsets word pos))))

;; Finds definitions of a word.
;; As of Japanese WordNet version 0.9, only English definitions are available.
(define (find-definitions word pos)
  ;; Currently, only English is available.
  (let1 lang 'eng
    (filter-map (lambda (synset)
                  (let1 synsetdef (find-synsetdef-by-synset-and-lang synset lang)
                    (if synsetdef (synsetdef-def synsetdef) #f)))
                (word->synsets word pos))))


;;;
;;; for compatibility with python frontend by yanbe
;;;
(define get-words find-words-by-lemma)
(define get-word find-word-by-wordid)
(define (get-senses word) (find-senses-by-wordid (word-wordid word)))
(define (get-sense synset . args)
  (let-optionals* args ((lang 'jpn))
    (car-or-false (find-senses-by-synset-and-lang synset lang))))
(define get-synset find-synset-by-synset)
(define (get-synlinks sense link) (find-synlinks-by-synset-and-link (sense-synset sense) link))

(provide "lang/wordnet-ja")
