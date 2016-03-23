SRC = Tangram.elm

build:
	elm make Tangram.elm

watch:
	-$(MAKE) build
	fswatch -l 0.2 $(SRC) | \
	  while read f; do echo "$$f"; $(MAKE) build; done

testbrowser:
	browser-sync start --server --files index.html --startPath index.html
