PORT ?= 8000

.PHONY: all site site-container serve clean

all: site

site:
	./build.sh

site-container:
	podman run -ti --network host -v $(shell pwd):/workspace iquiw/alpine-emacs sh -c "cd /workspace; ./build.sh"

serve: site
	cd public && python3 -c 'import http.server, webbrowser; \
		server = http.server.ThreadingHTTPServer(("127.0.0.1", $(PORT)), http.server.SimpleHTTPRequestHandler); \
		webbrowser.open("http://localhost:$(PORT)/"); \
		server.serve_forever()'

clean:
	rm -rf public/*
