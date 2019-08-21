all: site

site:
	emacs --batch --no-init-file --load publish.el --funcall toggle-debug-on-error --funcall duncan-publish-all

clean:
	rm -rf public/*
	rm -rf ~/.org-timestamps
