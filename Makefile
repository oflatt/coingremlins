.PHONY: docs all test

all: docs test

docs:
	mkdir -p ./docs
	scribble --dest ./docs *.scrbl

test:
	raco test *.rkt

