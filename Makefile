GOSH = gosh

demo:
	make jawjaw-demos
	/usr/bin/read -n 1 -p '<< Hit any key to continue >>'
	make wn-demo

wn-demo:
	$(GOSH) -I. wn.scm 言語 hypo

jawjaw-demos:
	$(GOSH) -I. demos/simple-demo.scm 研究 v
	$(GOSH) -I. demos/simple-demo.scm 言語 n
	$(GOSH) -I. demos/advanced-demo.scm 新しい a
	$(GOSH) -I. demos/advanced-demo.scm 実験 v
	$(GOSH) -I. demos/advanced-demo.scm 自然言語処理 n

test:
	$(GOSH) -I. wordnet-ja-test.scm

