pwd := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

.PHONY: test

test:
	ros run -e "(asdf:load-asd \"$(pwd)/liray.asd\") (ql:quickload :liray) (asdf:test-system :liray) (uiop:quit 0)"
