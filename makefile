run:
	cabal run

build:
	cabal build

clean:
	find docs -type f -delete

site:
	echo make | make run
