#!/bin/sh
set -x
emacs --batch --no-init-file --directory $PWD --script publish.el
