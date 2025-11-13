build :
	dune build
	cp -f ./_build/default/bin/main.exe ./rogue

run : build
	./rogue