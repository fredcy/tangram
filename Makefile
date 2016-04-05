SRC = Tangram.elm Model.elm View.elm Update.elm Colors.elm Start.elm

build:
	elm make Tangram.elm

watch:
	-$(MAKE) build
	fswatch -l 0.2 $(SRC) | \
	  while read f; do clear;  echo "$$f"; $(MAKE) build; done

browse:
	browser-sync start --server --files index.html --startPath index.html
