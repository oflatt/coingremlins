.PHONY: docs all test

all: docs test

docs:
  # delete old docs if exists
	rm -rf ./docs
	scribble --dest ./docs *.scrbl

test:
	raco test *.rkt

