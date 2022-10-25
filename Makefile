# first target is also the default target
all: compile link

compile:
	./compile-ndbapi-wrap-cxx.sh

link:
	./link-ndbapi-wrap-cxx.sh
