SHELL := /bin/bash
now   := $(shell date +%Y%m%d%H%M)
buildroot := $(shell pwd)

.PHONY: default setup backend frontend backend-clean frontend-clean run

default:
	cat README.md

setup:
	cd backend  && stack setup --install-ghc --no-system-ghc
	cd frontend && stack setup --install-ghc --no-system-ghc

backend:
	(cd backend && stack build)

frontend:
	cd frontend \
	  && stack build \
	  && cp "`stack path --dist-dir`/build/twic2ui/twic2ui.jsexe/all.js" ../webexe/Main.js \
		&& echo "Installed jsexe to ./webexe/"

backend-clean:
	cd backend && stack clean

frontend-clean:
	cd frontend && stack clean

run:
	cd backend && stack exec twic serve

atom-build-any:
	if [[ "${ARG}" == *"frontend"* ]]; then make frontend; else make backend; fi
