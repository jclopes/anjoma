compile: 
	erl -make

clean:
	rm ./ebin/*.beam
	rm package.zip

pkg:
	zip package.zip src/*.erl
