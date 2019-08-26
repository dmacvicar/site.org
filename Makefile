all: site

site:
	./build.sh

site-container:
	podman run -ti --network host -v $(shell pwd):/workspace iquiw/alpine-emacs sh -c "cd /workspace; ./build.sh"

clean:
	rm -rf public/*
