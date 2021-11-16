# A golden value test using goldplate

stem=IntroductoryExample

.PHONY: test
test:
	goldplate $(stem).goldplate

.PHONY: golden
golden: $(stem).golden

%.golden : %.hs
	runghc --ghc-arg="-package regex-base" --ghc-arg="-package regex-posix" $< > $@

#EOF
