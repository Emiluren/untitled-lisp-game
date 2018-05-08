#!/bin/sh
sbcl --eval "(asdf:operate :build-op :system-name)"
