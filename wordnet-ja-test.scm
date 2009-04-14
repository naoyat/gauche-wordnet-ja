;; wordnet-ja test
;; by naoya_t

(use gauche.test)

(use lang.wordnet-ja)
(test-module 'lang.wordnet-ja)

(test-start "wordnet-ja")
(wordnet-ja-initialize)

(test-section "peer APIs")
(let* ([word-to-test "自然言語処理"]
	   [word (car-or-false (find-words-by-lemma-and-pos word-to-test 'n))]
	   [word-id (word-wordid word)]
	   [senses (find-senses-by-wordid word-id)]
	   [sense (car senses)]
	   [synset-id (sense-synset sense)]
	   [synset (find-synset-by-synset synset-id)]
	   [synset-def (find-synsetdef-by-synset-and-lang synset-id 'eng)]
	   [synlink (car-or-false (find-synlinks-by-synset synset-id))]
	   )
  (test* "word" '(:word 201821 jpn "自然言語処理" #f n) word)
  (test* "sense" '(:sense 06142412-n 201821 jpn 0 0 0 mlsn) sense)
  (test* "synset" '(:synset 06142412-n n "human_language_technology" eng30) synset)
  (test* "synset_def" '(:synsetdef 06142412-n eng "the branch of information science that deals with natural language information" 0) synset-def)
  (test* "synlink" '(:synlink 06142412-n 06142118-n hype eng30) synlink)
  )

(test-section "APIs transported from JAWJAW")
(let ([word "買収"] [pos 'v])
  (test* "hypernyms"
		 '("支辨" "払出す" "会計" "払いだす" "払い出す" "出費" "支出" "出金" "支払う" "払いこむ" "支弁" "精算" "得る" "手に入れる" "入手" "買い上げる" "買いつける" "買求める" "召す" "買取り" "買収" "買いあげる" "買いもとめる" "買上げる" "買い入れる" "買込む" "購う" "購買" "買い付ける" "購入" "購求" "買う" "買いとる" "買入れる" "買いいれる" "買い求める" "買い取り" "買い取る" "買い受ける" "買い出し" "買いこむ" "買取" "買取る" "買い込む")
		 (find-hypernyms word pos))
  (test* "hyponyms"
		 '("買いうける" "買い上げる" "譲りうける" "買収" "買受ける" "買い取る" "譲受ける" "買取る")
		 (find-hyponyms word pos))
  (test* "entailments"
		'("支辨" "払出す" "会計" "払いだす" "払い出す" "出費" "支出" "出金" "支払う" "払いこむ" "支弁" "精算" "採択" "択む" "選む" "選考" "選分" "選び取る" "より取る" "選りわける" "選取" "選定" "選りすぐる" "択る" "チョイス" "択ぶ" "選りどる" "選る" "選取る" "選り分ける" "選り抜く" "より分ける" "セレクト" "選抜" "選する" "精選" "選り取る" "選び出す" "選抜く" "より抜く" "簡抜" "選択" "選り出す" "選分ける" "選りぬく" "選出す" "より出す" "選りだす" "選ぶ")
		(find-entailments word pos))
  (test* "translations"
		'("corrupt" "buy" "bribe" "grease_one's_palms" "purchase" "buy" "buy_out" "take_over" "buy_up")
		(find-translations word pos))
  (test* "definitions"
		'("make illegal payments to in exchange for favors or influence; \"This judge can be bought\""
		  "obtain by purchase; acquire by means of a financial transaction; \"The family purchased a new car\"; \"The conglomerate acquired a new company\"; \"She buys for the big department store\""
		  "take over ownership of; of corporations and companies")
		(find-definitions word pos))
  )

(test-end)
