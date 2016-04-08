SRC = Tangram.elm Model.elm View.elm Update.elm Colors.elm Start.elm

build:
	elm make Tangram.elm --output elm.js

watch:
	-$(MAKE) build
	fswatch -l 0.2 $(SRC) | \
	  while read f; do clear;  echo "$$f"; $(MAKE) build; done

browse:
	browser-sync start --server --files elm.js --startPath tangram.html

