#!/bin/sh
set -x
emacs --batch --no-init-file --directory $PWD --load publish.el --funcall toggle-debug-on-error --funcall duncan-publish-all
