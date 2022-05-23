.PHONY: dev

.DEFAULT_GOAL:= dist

clean:
	rm -rf dist dist.old

dist: clean
	gleam build
	npx vite build

dev:
	npx concurrently \
		vite \
		'watchexec --filter "src/*" gleam build'

lint:
	npx elm-review --fix-all
