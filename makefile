run:
	cabal run

build:
	cabal build

clean:
	rm -rf dist-newstyle
	find docs -type f -delete

site:
	echo make | make run
